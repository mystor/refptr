//! Reference counting types used by [`Refcounted`] objects.

use crate::Refcounted;
use alloc::boxed::Box;
use core::cell::Cell;
use core::fmt;
use core::mem::{self, ManuallyDrop};
use core::ptr;
use core::sync::atomic::Ordering;

#[cfg(feature = "atomic")]
use core::sync::atomic::{self, AtomicUsize};

/// Allocation format used by [`Refcounted`] types.
#[repr(C)]
pub struct Inner<T: ?Sized + Refcounted> {
    // XXX: Should these be private and only exposed with manual manual pointer
    // fiddling to help preserve provenance?
    /// The reference count used by `T`.
    pub refcnt: T::Rc,
    /// Allocation for `T`'s data.
    pub data: ManuallyDrop<T>,
}

unsafe fn data_offset<T: ?Sized + Refcounted>(ptr: *const T) -> usize {
    // Align the unsized value to the end of the `Inner`.
    // FIXME: Use a raw pointer variant if possible in the future.
    let align = mem::align_of_val(&*ptr);

    // We're guaranteed this doesn't overflow, as `ptr` is a pointer to a valid allocation.
    mem::size_of::<T::Rc>().wrapping_add(align).wrapping_sub(1) & !align.wrapping_sub(1)
}

/// Sets the data pointer part of a `?Sized` raw pointer.
///
/// This will leave the metadata of a slice/trait object unmodified, and will
/// just set a sized pointer in place.
unsafe fn set_data_ptr<T: ?Sized, U>(mut ptr: *mut T, data: *mut U) -> *mut T {
    ptr::write(&mut ptr as *mut *mut T as *mut *mut u8, data as *mut u8);
    ptr
}

impl<T: ?Sized + Refcounted> Inner<T> {
    /// Recover a pointer to an `Inner<T>` from a pointer to the inner `T`.
    pub(crate) unsafe fn cast(val: *mut T) -> *mut Self {
        let offset = data_offset(val);
        let new_data = (val as *mut u8).sub(offset);
        let ptr = set_data_ptr(val, new_data) as *mut Self;
        debug_assert_eq!(&*(*ptr).data as *const T, val as *const T);
        ptr
    }
}

/// A reference count
pub unsafe trait Refcount: Sized {
    /// Metadata type which can be obtained from the underlying `Refcounted` object.
    type Metadata;

    /// Construct a new instance of the refcount, initialized to a default value.
    unsafe fn new() -> Self;

    /// Increment the strong reference count for this object.
    ///
    /// Prefer managing the lifecycle of `Refcounted` objects with [`RefPtr`]
    /// over manually calling these methods.
    ///
    /// This must only be called while the strong reference count is at least 1.
    /// If only weak references only exist for this object, the `upgrade` method
    /// must be used instead.
    ///
    /// [`RefPtr`]: crate::RefPtr
    unsafe fn inc_strong<T: ?Sized>(ptr: *const Inner<T>)
    where
        T: Refcounted<Rc = Self>;

    /// Decrement the strong reference count for this object.
    ///
    /// Prefer managing the lifecycle of `Refcounted` objects with [`RefPtr`]
    /// over manually calling these methods.
    ///
    /// [`RefPtr`]: crate::RefPtr
    unsafe fn dec_strong<T: ?Sized>(ptr: *const Inner<T>)
    where
        T: Refcounted<Rc = Self>;

    /// Gets the number of strong references to this allocation.
    unsafe fn strong_count<T: ?Sized>(ptr: *const Inner<T>) -> usize
    where
        T: Refcounted<Rc = Self>;
}

/// A reference count supporting weak references
pub unsafe trait WeakRefcount: Refcount {
    /// Called to increment the weak reference count of an object.
    unsafe fn inc_weak<T: ?Sized>(ptr: *const Inner<T>)
    where
        T: Refcounted<Rc = Self>;

    /// Called to decrement the weak reference count of an object.
    ///
    /// This method may free the data stored in `ptr`.
    unsafe fn dec_weak<T: ?Sized>(ptr: *const Inner<T>)
    where
        T: Refcounted<Rc = Self>;

    /// Called to attempt to obtain a new strong reference to an object.
    ///
    /// This method will return `true` if the strong reference
    /// count was successfully incremented, and `false` otherwise.
    unsafe fn upgrade<T: ?Sized>(ptr: *const Inner<T>) -> bool
    where
        T: Refcounted<Rc = Self>;

    /// Try to get the number of weak references to this allocation.
    ///
    /// If there are no remaining strong references, this will
    /// return `0`.
    unsafe fn weak_count<T: ?Sized>(ptr: *const Inner<T>) -> usize
    where
        T: Refcounted<Rc = Self>;
}

/// A soft limit on the amount of references that may be made to an
/// AtomicRefcnt.
///
/// Going above this limit will abort your program (although not necessarily) at
/// _exactly_ `MAX_REFCOUNT + 1` references.
const MAX_REFCOUNT: usize = isize::max_value() as usize;

// `std::process::abort` isn't available with no_std, and
// `core::intrinsics::abort` is perma-unstable. Double-panics are unrecoverable,
// so use one to ensure we exit.
//
// XXX: Make this use `std::process::abort` if a `std` feature is added?
#[inline(never)]
#[cold]
fn abort() -> ! {
    struct Bomb;
    impl Drop for Bomb {
        fn drop(&mut self) {
            panic!("double-panicing to force abort");
        }
    }
    let _bomb = Bomb;
    panic!("aborting due to excessively large reference count");
}

macro_rules! dec_strong_impl {
    () => {
        unsafe fn dec_strong<T: ?Sized>(ptr: *const Inner<T>)
        where
            T: Refcounted<Rc = Self>,
        {
            let drop_fields = || {
                ManuallyDrop::drop(&mut (*(ptr as *mut Inner<T>)).data);
            };

            if (*ptr).refcnt.inner.dec_strong(drop_fields) {
                drop(Box::from_raw(ptr as *mut Inner<T>));
            }
        }
    };
    (finalize) => {
        unsafe fn dec_strong<T: ?Sized>(ptr: *const Inner<T>)
        where
            T: Refcounted<Rc = Self>,
        {
            let drop_fields = || {
                ManuallyDrop::drop(&mut (*(ptr as *mut Inner<T>)).data);
            };
            let finalize = || {
                let finalize_fn = (*ptr).data.refcount_metadata();
                finalize_fn((&(*ptr).data) as *const _ as *const u8);
            };

            if (*ptr)
                .refcnt
                .inner
                .dec_strong_finalize(drop_fields, finalize)
            {
                drop(Box::from_raw(ptr as *mut Inner<T>));
            }
        }
    };
}

macro_rules! metadata_type {
    () => { () };
    (finalize) => { unsafe fn(*const u8) };
}

macro_rules! decl_refcnt {
    ($name:ident, [$strong:ty $(, $weak:ty)?] $(, $finalize:ident)?) => {
        /// Reference count type used by the `#[refcounted]` attribute.
        pub struct $name {
            inner: RefcntImpl<$strong $(, $weak)?>,
        }

        unsafe impl Refcount for $name {
            type Metadata = metadata_type!($($finalize)?);

            unsafe fn new() -> Self {
                $name {
                    inner: RefcntImpl::new(),
                }
            }

            unsafe fn inc_strong<T: ?Sized>(ptr: *const Inner<T>)
            where
                T: Refcounted<Rc = Self>,
            {
                (*ptr).refcnt.inner.inc_strong()
            }

            dec_strong_impl!($($finalize)?);

            unsafe fn strong_count<T: ?Sized>(ptr: *const Inner<T>) -> usize
            where
                T: Refcounted<Rc = Self>,
            {
                (*ptr).refcnt.inner.strong_count()
            }
        }

        $(
            unsafe impl WeakRefcount for $name {
                unsafe fn inc_weak<T: ?Sized>(ptr: *const Inner<T>)
                where
                    T: Refcounted<Rc = Self>,
                {
                    type _Ignore = $weak;
                    (*ptr).refcnt.inner.inc_weak()
                }

                unsafe fn dec_weak<T: ?Sized>(ptr: *const Inner<T>)
                where
                    T: Refcounted<Rc = Self>,
                {
                    if (*ptr).refcnt.inner.dec_weak() {
                        drop(Box::from_raw(ptr as *mut Inner<T>));
                    }
                }

                unsafe fn upgrade<T: ?Sized>(ptr: *const Inner<T>) -> bool
                where
                    T: Refcounted<Rc = Self>,
                {
                    (*ptr).refcnt.inner.upgrade()
                }

                unsafe fn weak_count<T: ?Sized>(ptr: *const Inner<T>) -> usize
                where
                    T: Refcounted<Rc = Self>,
                {
                    (*ptr).refcnt.inner.weak_count()
                }
            }
        )?

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_struct(stringify!($name))
                    .field("strong", &self.inner.strong_count())
                    $(
                        .field("weak", {
                            type _Ignore = $weak;
                            &self.inner.weak_count()
                        })
                    )?
                    .finish()
            }
        }
    }
}

decl_refcnt!(Local, [Cell<usize>]);
decl_refcnt!(LocalWeak, [Cell<usize>, Cell<usize>]);
#[cfg(feature = "atomic")]
decl_refcnt!(Atomic, [AtomicUsize]);
#[cfg(feature = "atomic")]
decl_refcnt!(AtomicWeak, [AtomicUsize, AtomicUsize]);

decl_refcnt!(LocalFinalize, [Cell<usize>], finalize);
decl_refcnt!(LocalWeakFinalize, [Cell<usize>, Cell<usize>], finalize);
#[cfg(feature = "atomic")]
decl_refcnt!(AtomicFinalize, [AtomicUsize], finalize);
#[cfg(feature = "atomic")]
decl_refcnt!(AtomicWeakFinalize, [AtomicUsize, AtomicUsize], finalize);

/// Internal trait for abstracting over `AtomicUsize`, `Cell<usize>`, and `()`.
///
/// Not for external use.
trait IncDecCount {
    fn new() -> Self;
    fn load(&self) -> usize;
    fn dec(&self) -> usize;
    fn inc(&self) -> usize;
    fn fence(order: Ordering);
}

#[cfg(feature = "atomic")]
impl IncDecCount for AtomicUsize {
    fn new() -> Self {
        AtomicUsize::new(1)
    }

    fn load(&self) -> usize {
        self.load(Ordering::SeqCst)
    }

    fn dec(&self) -> usize {
        self.fetch_sub(1, Ordering::Release)
    }

    fn inc(&self) -> usize {
        // Using a relaxed ordering is OK here, as knowledge of an existing
        // reference prevents other threads from erroneously deleting the object
        // while we're incrementing.
        //
        // As we have a reference to `self`, we know there is a live reference
        // to our containing object which this count is referring to.
        //
        // This is documented in more detail in the rustc source code for `Arc`.
        let prev = self.fetch_add(1, Ordering::Relaxed);

        // Guard against excessively-large reference counts.
        if prev > MAX_REFCOUNT - 1 {
            abort();
        }
        prev
    }

    fn fence(order: Ordering) {
        atomic::fence(order)
    }
}

impl IncDecCount for Cell<usize> {
    fn new() -> Self {
        Cell::new(1)
    }

    fn load(&self) -> usize {
        self.get()
    }

    fn dec(&self) -> usize {
        let prev = self.get();
        self.set(prev - 1);
        prev
    }

    fn inc(&self) -> usize {
        let prev = self.get();

        // Guard against excessively-large reference counts.
        let next = prev.checked_add(1).unwrap_or_else(|| abort());
        self.set(next);
        prev
    }

    fn fence(_: Ordering) {}
}

/// NOTE: This impl should only be used for weak counts, as it doesn't make
/// sense for strong counts.
impl IncDecCount for () {
    fn new() -> Self {
        ()
    }
    fn load(&self) -> usize {
        unreachable!()
    }
    fn dec(&self) -> usize {
        1
    }
    fn inc(&self) -> usize {
        unreachable!()
    }
    fn fence(_: Ordering) {
        unreachable!()
    }
}

struct RefcntImpl<T, W = ()> {
    strong: T,
    weak: W,
}

impl<T: IncDecCount, W: IncDecCount> RefcntImpl<T, W> {
    unsafe fn new() -> Self {
        RefcntImpl {
            strong: T::new(),
            weak: W::new(),
        }
    }

    fn strong_count(&self) -> usize {
        self.strong.load()
    }

    fn weak_count(&self) -> usize {
        let weak = self.weak.load();
        let strong = self.strong.load();
        if strong == 0 {
            // std::sync::Arc returns `0` for `weak_count` when the strong count
            // has reached `0`. That behaviour is copied here.
            0
        } else {
            // We loaded the strong count after the weak count. As there is a
            // strong reference, we know the implicit weak reference is present,
            // and can subtract it from our loaded `weak` value.
            weak - 1
        }
    }

    unsafe fn inc_strong(&self) {
        self.strong.inc();
    }

    unsafe fn dec_strong(&self, drop_fields: impl FnOnce()) -> bool {
        // If we've decreased to a non-zero value, we can immediately return.
        let old_count = self.strong.dec();
        if old_count != 1 {
            return false;
        }

        // We're about to drop the object in question due to the count reaching
        // zero. As documented in the `Arc` implementation, we need to be sure
        // that any other writes (e.g. in interior mutability) are visible to
        // our thread before we release, so we need to perform an `Acquire`
        // fence here.
        T::fence(Ordering::Acquire);

        // Drop any remaining fields, and then drop our self-weak-reference,
        // which may cause the backing storage to also be freed.
        //
        // PANIC NOTE: If `drop_fields` panics, the weak reference will never be
        // freed, and the backing allocation will leak.
        drop_fields();
        self.dec_weak()
    }

    unsafe fn inc_weak(&self) {
        self.weak.inc();
    }

    unsafe fn dec_weak(&self) -> bool {
        self.weak.dec() == 1
    }
}

#[cfg(feature = "atomic")]
impl RefcntImpl<AtomicUsize, ()> {
    unsafe fn dec_strong_finalize(
        &self,
        drop_fields: impl FnOnce(),
        finalize: impl FnOnce(),
    ) -> bool {
        // As there are no weak pointers, it's OK to decrement our refcount to
        // `0`, so long as we increment it back to `1` before finalization.
        let old_count = self.strong.fetch_sub(1, Ordering::Release);
        if old_count != 1 {
            return false;
        }

        // We are the last remaining reference to the object. Stabilize the
        // refcount to `1` to invoke `finalize`.
        self.strong.store(1, Ordering::Relaxed);

        // We perform an `Acquire` fence here to ensure writes from other
        // threads are visible in `finalize`.
        atomic::fence(Ordering::Acquire);

        // PANIC: If `finalize` panics, memory controlled by this operation will
        // be leaked.
        finalize();

        // We've already finalized, so can drop our reference like normal.
        // Use the standard codepath, as `finalize` may have acquired a new
        // reference.
        self.dec_strong(drop_fields)
    }
}

#[cfg(feature = "atomic")]
impl RefcntImpl<AtomicUsize, AtomicUsize> {
    /// Attempt to upgrade a weak reference to a strong reference by
    /// incrementing the strong reference count. If this method returns `true`,
    /// the upgrade was successful, and if it returns `false`, the strong
    /// reference count has reached `0`, and the object has been destroyed.
    unsafe fn upgrade(&self) -> bool {
        // We use a CAS loop to increment the strong count instead of a
        // fetch_add because once the count hits 0 it must never be above 0.

        // Relaxed load because any write of 0 that we can observe leaves the
        // field in a permanently zero state (so a "stale" read of 0 is fine),
        // and any other value is confirmed via the CAS.
        //
        // Relaxed is valid for the store for the same reason it is on `addref`.
        let mut prev = self.strong.load(Ordering::Relaxed);
        while prev != 0 {
            if prev > MAX_REFCOUNT - 1 {
                abort();
            }

            match self.strong.compare_exchange_weak(
                prev,
                prev + 1,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => return true,
                Err(actual) => prev = actual,
            }
        }
        false
    }

    unsafe fn dec_strong_finalize(
        &self,
        drop_fields: impl FnOnce(),
        finalize: impl FnOnce(),
    ) -> bool {
        // We cannot drop our strong refcount to `0` until after finalization,
        // as otherwise `upgrade` could observe a `0` strong refcount when the
        // object is not yet dead.
        //
        // Do a manual compare_exchange loop before we begin finalizing.
        let mut old_count = self.strong.load(Ordering::Relaxed);
        while old_count != 1 {
            match self.strong.compare_exchange_weak(
                old_count,
                old_count - 1,
                Ordering::Release,
                Ordering::Relaxed,
            ) {
                Ok(_) => return false,
                Err(actual) => old_count = actual,
            }
        }

        debug_assert!(old_count == 1);

        // We've observed that, at least for a moment, we were the last
        // remaining strong reference. It is possible that weak references can
        // be upgraded, or new pointers created, while `finalize` is being
        // called.
        //
        // We perform an `Acquire` fence here to ensure writes from other
        // threads are visible in `finalize`.
        atomic::fence(Ordering::Acquire);

        // PANIC: If `finalize` panics, memory controlled by this operation will
        // be leaked.
        finalize();

        // We've already finalized, so can drop our reference like normal.
        // Use the standard codepath, as `finalize` may have acquired a new
        // reference.
        self.dec_strong(drop_fields)
    }
}

impl<W> RefcntImpl<Cell<usize>, W>
where
    W: IncDecCount,
{
    /// Attempt to upgrade a weak reference to a strong reference by
    /// incrementing the strong reference count. If this method returns `true`,
    /// the upgrade was successful, and if it returns `false`, the strong
    /// reference count has reached `0`, and has been destroyed.
    unsafe fn upgrade(&self) -> bool {
        let prev_value = self.strong.get();
        if prev_value == 0 {
            return false;
        }

        // Guard against massive reference counts by aborting if it would
        // exceed `isize::MAX`.
        if prev_value > MAX_REFCOUNT - 1 {
            abort();
        }

        self.strong.set(prev_value + 1);
        true
    }

    unsafe fn dec_strong_finalize(
        &self,
        drop_fields: impl FnOnce(),
        finalize: impl FnOnce(),
    ) -> bool {
        let prev_value = self.strong.get();
        if prev_value != 1 {
            self.strong.set(prev_value - 1);
            return false;
        }

        // We are the last remaining reference, but haven't decremented it yet.
        // Invoke finalize to give the object a chance to clean up. This may
        // create new references.
        finalize();

        // Try to drop our last remaining reference.
        self.dec_strong(drop_fields)
    }
}
