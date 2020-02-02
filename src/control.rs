//! Objects which act as control headers for refcounted objects. All of these
//! objects are wildly unsafe to create outside of autogenerated code, and
//! generally shouldn't be used by users of this crate.

use crate::{RefPtr, Refcounted};
use alloc::boxed::Box;
use alloc::rc::Rc;
use core::cell::Cell;
use core::fmt;
use core::marker::PhantomData;
use core::mem::ManuallyDrop;
use core::sync::atomic::Ordering;

#[cfg(feature = "atomic")]
use alloc::sync::Arc;

#[cfg(feature = "atomic")]
use core::sync::atomic::{self, AtomicUsize};

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

#[must_use]
#[derive(Eq, PartialEq, Debug)]
pub enum UpgradeAction {
    /// The caller should take no action.
    None,

    /// The caller has acquired a strong reference to the object's backing
    /// memory.
    Upgrade,
}

impl UpgradeAction {
    pub unsafe fn take_action<T: ?Sized + Refcounted>(self, ptr: *const T) -> Option<RefPtr<T>> {
        match self {
            UpgradeAction::None => None,
            UpgradeAction::Upgrade => Some(RefPtr::from_raw(ptr)),
        }
    }
}

#[must_use]
#[derive(Eq, PartialEq, Debug)]
pub enum FreeAction {
    /// The caller should take no action.
    None,

    /// The caller should free the object's backing memory without invoking
    /// `Drop`.
    FreeMemory,
}

impl FreeAction {
    pub unsafe fn take_action<T: ?Sized>(self, ptr: *const T) {
        match self {
            FreeAction::None => {}
            FreeAction::FreeMemory => {
                // Unsafely drop the value stored in `ptr`.
                Box::from_raw(ptr as *mut T as *mut ManuallyDrop<T>);
            }
        }
    }
}

pub trait ControlBlock {
    /// Create a new control block, initialized to a strong (and weak,
    /// if applicable), reference count of `1`.
    unsafe fn new() -> Self;
}

macro_rules! decl_control {
    ($name:ident, $atomic:ident, [$strong:ty $(, $weak:ty)?]) => {
        /// Reference counting control block used by the `#[refcounted]`
        /// attribute.
        pub struct $name<T: ?Sized> {
            inner: RefcntImpl<$strong $(, $weak)?>,
            // This marker is required to limit the type to only be `Sync` if
            // it is also `Send`, as otherwise we could pass a non-`Send` `&T`
            // to another thread, acquire a reference, and drop it there.
            _marker: PhantomData<$atomic<T>>,
        }

        impl<T: ?Sized> ControlBlock for $name<T> {
            unsafe fn new() -> Self {
                $name {
                    inner: RefcntImpl::new(),
                    _marker: PhantomData,
                }
            }
        }

        impl<T: ?Sized> $name<T> {
            /// Increment the allocation's strong reference count.
            pub unsafe fn inc_strong(&self) {
                self.inner.inc_strong()
            }

            /// Decrement the allocation's strong reference count, potentially
            /// dropping fields.
            ///
            /// Use of this method is more efficient than `dec_strong_finalize`,
            /// but does not provide an opportunity for finalization.
            ///
            /// Callers of this method should free the underlying memory if
            /// a `FreeAction::FreeMemory` action is returned.
            pub unsafe fn dec_strong(
                &self,
                drop_fields: impl FnOnce(),
            ) -> FreeAction {
                self.inner.dec_strong(drop_fields)
            }

            /// Decrement the allocation's strong reference count, potentially
            /// running a finalizer and dropping fields.
            ///
            /// The finalizer may be called multiple times during an object's
            /// lifetime.
            ///
            /// Callers of this method should free the underlying memory if
            /// a `FreeAction::FreeMemory` action is returned.
            pub unsafe fn dec_strong_finalize(
                &self,
                drop_fields: impl FnOnce(),
                finalize: impl FnOnce(),
            ) -> FreeAction {
                self.inner.dec_strong_finalize(drop_fields, finalize)
            }

            /// Gets the number of strong references to this allocation.
            pub fn strong_count(&self) -> usize {
                self.inner.strong_count()
            }

            $(
                /// Gets the number of weak references to this allocation.
                ///
                /// If there are no remaining strong references, this will
                /// return `0`.
                pub fn weak_count(&self) -> usize {
                    self.inner.weak_count()
                }

                /// Increment the allocation's weak reference count.
                pub unsafe fn inc_weak(&self) {
                    type _Ignore = $weak;
                    self.inner.inc_weak()
                }

                /// Decrement the allocation's weak reference count.
                ///
                /// Callers of this method should free the underlying memory if
                /// a `FreeAction::FreeMemory` action is returned.
                pub unsafe fn dec_weak(&self) -> FreeAction {
                    self.inner.dec_weak()
                }

                /// Attempt to upgrade a weak reference to a strong reference by
                /// incrementing the strong reference count.
                ///
                /// If this method returns `true`, the upgrade was successful,
                /// and if it returns `false`, the strong reference count has
                /// already reached `0`, and fields have been dropped.
                pub unsafe fn upgrade(&self) -> UpgradeAction {
                    self.inner.upgrade()
                }
            )?
        }

        impl<T: ?Sized> fmt::Debug for $name<T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_struct(stringify!($name))
                    .field("strong", &self.strong_count())
                    $(
                        .field("weak", {
                            type _Ignore = $weak;
                            &self.weak_count()
                        })
                    )?
                    .finish()
            }
        }
    }
}

decl_control!(Refcnt, Rc, [Cell<usize>]);
decl_control!(RefcntWeak, Rc, [Cell<usize>, Cell<usize>]);
#[cfg(feature = "atomic")]
decl_control!(AtomicRefcnt, Arc, [AtomicUsize]);
#[cfg(feature = "atomic")]
decl_control!(AtomicRefcntWeak, Arc, [AtomicUsize, AtomicUsize]);

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
        if prev > MAX_REFCOUNT - 1 {
            abort();
        }
        self.set(prev + 1);
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

    unsafe fn dec_strong(&self, drop_fields: impl FnOnce()) -> FreeAction {
        // If we've decreased to a non-zero value, we can immediately return.
        let old_count = self.strong.dec();
        if old_count != 1 {
            return FreeAction::None;
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

    unsafe fn dec_weak(&self) -> FreeAction {
        if self.weak.dec() == 1 {
            FreeAction::FreeMemory
        } else {
            FreeAction::None
        }
    }
}

#[cfg(feature = "atomic")]
impl RefcntImpl<AtomicUsize, ()> {
    unsafe fn dec_strong_finalize(
        &self,
        drop_fields: impl FnOnce(),
        finalize: impl FnOnce(),
    ) -> FreeAction {
        // As there are no weak pointers, it's OK to decrement our refcount to
        // `0`, so long as we increment it back to `1` before finalization.
        let old_count = self.strong.fetch_sub(1, Ordering::Release);
        if old_count != 1 {
            return FreeAction::None;
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
    unsafe fn upgrade(&self) -> UpgradeAction {
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
                Ok(_) => return UpgradeAction::Upgrade,
                Err(actual) => prev = actual,
            }
        }
        UpgradeAction::None
    }

    unsafe fn dec_strong_finalize(
        &self,
        drop_fields: impl FnOnce(),
        finalize: impl FnOnce(),
    ) -> FreeAction {
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
                Ok(_) => return FreeAction::None,
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
    unsafe fn upgrade(&self) -> UpgradeAction {
        let prev_value = self.strong.get();
        if prev_value == 0 {
            return UpgradeAction::None;
        }

        // Guard against massive reference counts by aborting if it would
        // exceed `isize::MAX`.
        if prev_value > MAX_REFCOUNT - 1 {
            abort();
        }

        self.strong.set(prev_value + 1);
        UpgradeAction::Upgrade
    }

    unsafe fn dec_strong_finalize(
        &self,
        drop_fields: impl FnOnce(),
        finalize: impl FnOnce(),
    ) -> FreeAction {
        let prev_value = self.strong.get();
        if prev_value != 1 {
            self.strong.set(prev_value - 1);
            return FreeAction::None;
        }

        // We are the last remaining reference, but haven't decremented it yet.
        // Invoke finalize to give the object a chance to clean up. This may
        // create new references.
        finalize();

        // Try to drop our last remaining reference.
        self.dec_strong(drop_fields)
    }
}
