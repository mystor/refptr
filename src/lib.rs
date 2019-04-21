use std::cell::Cell;
use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ops::Deref;
use std::process::abort;
use std::ptr::NonNull;
use std::sync::atomic::Ordering::{Acquire, Relaxed, Release, SeqCst};
use std::sync::atomic::{self, AtomicUsize};
use std::fmt;
use std::hash::{Hash, Hasher};

pub use refcounted_macros::refcounted;

/// A soft limit on the amount of references that may be made to an AtomicRefcnt.
///
/// Going above this limit will abort your program (although not necessarily) at
/// _exactly_ `MAX_REFCOUNT + 1` references.
const MAX_REFCOUNT: usize = isize::max_value() as usize;

/// A reference counted pointer type for holding refcounted objects.
pub struct RefPtr<T: ?Sized + Refcounted> {
    ptr: NonNull<T>,
    _marker: PhantomData<T>,
}

impl<T: ?Sized + Refcounted> RefPtr<T> {
    pub fn new(val: &T) -> RefPtr<T> {
        unsafe {
            val.addref();
            RefPtr::dont_addref(val)
        }
    }

    pub unsafe fn dont_addref(val: &T) -> RefPtr<T> {
        RefPtr {
            ptr: NonNull::new_unchecked(val as *const T as *mut T),
            _marker: PhantomData,
        }
    }
}

impl<T: ?Sized + Refcounted> Clone for RefPtr<T> {
    fn clone(&self) -> Self {
        RefPtr::new(&self)
    }
}

impl<T: ?Sized + Refcounted> Deref for RefPtr<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: ?Sized + Refcounted> Drop for RefPtr<T> {
    fn drop(&mut self) {
        unsafe { self.release() }
    }
}

unsafe impl<T: ?Sized + Refcounted + Sync + Send> Send for RefPtr<T> {}
unsafe impl<T: ?Sized + Refcounted + Sync + Send> Sync for RefPtr<T> {}


/// The primary trait of this library. This trait is implemented by objects which
/// are invasively reference counted. It has a few different constraints put upon
/// those objects.
///
/// ## Safety
///
///  * `Refcounted` objects are always heap-allocated
///  * Only shared references may exist to `Refcounted` objects
///  * If the `addref` method has been called more times than the `release`
///    method, the object must not be dropped.
pub unsafe trait Refcounted {
    unsafe fn addref(&self);
    unsafe fn release(&self);
}

/// Atomic internal reference count.
pub struct AtomicRefcnt {
    val: AtomicUsize,
}

impl AtomicRefcnt {
    /// Create a new `AtomicRefcnt`
    ///
    /// This method is unsafe to prevent creation of `Refcounted` types outside
    /// of generated methods.
    #[inline]
    pub unsafe fn new() -> Self {
        AtomicRefcnt {
            val: AtomicUsize::new(0),
        }
    }

    #[inline]
    pub fn get(&self) -> usize {
        self.val.load(SeqCst)
    }

    #[inline]
    pub unsafe fn inc(&self) {
        // Using a relaxed ordering is OK here, as knowledge of an existing
        // reference prevents other threads from erroneously deleting the object
        // while we're incrementing.
        //
        // As we have a reference to `self`, we know there is a live reference
        // to our containing object which this count is referring to.
        //
        // This is documented in more detail in the rustc source code for `Arc`.
        let old_count = self.val.fetch_add(1, Relaxed);

        // Guard against massive reference counts by aborting if you exceed
        // `isize::MAX`.
        if old_count > MAX_REFCOUNT {
            abort();
        }
    }

    #[inline]
    pub unsafe fn dec(&self) -> bool {
        // If we've decreased to a non-zero value, we can immediately return.
        let old_count = self.val.fetch_sub(1, Release);
        if old_count != 1 {
            return false;
        }

        // We're about to drop the object in question due to the count reaching
        // zero. As documented in the `Arc` implementation, we need to be sure
        // that any other writes (e.g. in interior mutability) are visible to
        // our thread before we release, so we need to perform an `Acquire`
        // fence here.
        atomic::fence(Acquire);
        true
    }
}

impl fmt::Debug for AtomicRefcnt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("AtomicRefcnt")
            .field(&self.get())
            .finish()
    }
}

/// Non-atomic internal reference count.
pub struct Refcnt {
    val: Cell<usize>,
}

impl Refcnt {
    /// Create a new `Refcnt`
    ///
    /// This method is unsafe to prevent creation of `Refcounted` types outside
    /// of generated methods.
    #[inline]
    pub unsafe fn new() -> Self {
        Refcnt { val: Cell::new(0) }
    }

    #[inline]
    pub fn get(&self) -> usize {
        self.val.get()
    }

    #[inline]
    pub unsafe fn inc(&self) {
        if self.val.get() == usize::max_value() {
            abort();
        }
        self.val.set(self.val.get() + 1);
    }

    #[inline]
    pub unsafe fn dec(&self) -> bool {
        self.val.set(self.val.get() - 1);
        self.val.get() == 0
    }
}

impl fmt::Debug for Refcnt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Refcnt")
            .field(&self.get())
            .finish()
    }
}


// Trait impls for `RefPtr<T>`

impl<T: ?Sized + Refcounted + PartialEq> PartialEq for RefPtr<T> {
    fn eq(&self, other: &RefPtr<T>) -> bool {
        **self == **other
    }
}

impl<T: ?Sized + Refcounted + PartialOrd> PartialOrd for RefPtr<T> {
    fn partial_cmp(&self, other: &RefPtr<T>) -> Option<Ordering> {
        (**self).partial_cmp(&**other)
    }
    fn lt(&self, other: &RefPtr<T>) -> bool {
        *(*self) < *(*other)
    }
    fn le(&self, other: &RefPtr<T>) -> bool {
        *(*self) <= *(*other)
    }
    fn gt(&self, other: &RefPtr<T>) -> bool {
        *(*self) > *(*other)
    }
    fn ge(&self, other: &RefPtr<T>) -> bool {
        *(*self) >= *(*other)
    }
}

impl<T: ?Sized + Refcounted + Ord> Ord for RefPtr<T> {
    fn cmp(&self, other: &RefPtr<T>) -> Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: ?Sized + Refcounted + Eq> Eq for RefPtr<T> {}

impl<T: ?Sized + Refcounted + fmt::Display> fmt::Display for RefPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: ?Sized + Refcounted + fmt::Debug> fmt::Debug for RefPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + Refcounted> fmt::Pointer for RefPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&(&**self as *const T), f)
    }
}

impl<T: ?Sized + Refcounted + Hash> Hash for RefPtr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

impl<T: ?Sized + Refcounted> From<&T> for RefPtr<T> {
    fn from(v: &T) -> Self {
        RefPtr::new(v)
    }
}
