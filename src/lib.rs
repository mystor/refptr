use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::fmt;
use std::hash::{Hash, Hasher};

pub use refcounted_macros::refcounted;

/// Control block objects used by refcounted-macros.
pub mod control;

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


/// A reference counted pointer type for holding refcounted objects.
pub struct WeakPtr<T: ?Sized + WeakRefcounted> {
    ptr: NonNull<T>,
    _marker: PhantomData<T>,
}

impl<T: ?Sized + WeakRefcounted> WeakPtr<T> {
    pub fn new(val: &T) -> WeakPtr<T> {
        unsafe {
            val.weak_addref();

            WeakPtr {
                ptr: NonNull::new_unchecked(val as *const T as *mut T),
                _marker: PhantomData,
            }
        }
    }

    pub fn upgrade(&self) -> Option<RefPtr<T>> {
        unsafe {
            if self.ptr.as_ref().upgrade() {
                Some(RefPtr::dont_addref(self.ptr.as_ref()))
            } else {
                None
            }
        }
    }
}

impl<T: ?Sized + WeakRefcounted> Clone for WeakPtr<T> {
    fn clone(&self) -> Self {
        unsafe {
            self.ptr.as_ref().weak_addref();
        }

        WeakPtr {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T: ?Sized + WeakRefcounted> Drop for WeakPtr<T> {
    fn drop(&mut self) {
        unsafe { self.ptr.as_ref().weak_release() }
    }
}

unsafe impl<T: ?Sized + WeakRefcounted + Sync + Send> Send for WeakPtr<T> {}
unsafe impl<T: ?Sized + WeakRefcounted + Sync + Send> Sync for WeakPtr<T> {}


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
    /// Increment the strong reference count for this object.
    ///
    /// This must only be called while the strong reference count is at least 1.
    /// If only weak references only exist for this object, the `upgrade` method
    /// must be used instead.
    unsafe fn addref(&self);

    /// Decrement the strong reference count for this object.
    unsafe fn release(&self);

    /// Unsafely drop all fields in this struct without freeing the corresponding
    /// storage (e.g. when the strong refcount reaches 0, but the weak refcount
    /// is still non-zero).
    unsafe fn drop_fields(&self);
}


pub unsafe trait WeakRefcounted: Refcounted {
    /// Increment the weak reference count of this object.
    unsafe fn weak_addref(&self);

    /// Decrement the weak reference count of this object.
    unsafe fn weak_release(&self);

    /// Attempt to obtain a new strong reference to this object. This method will
    /// return `true` if the strong reference count was successfully incremented.
    unsafe fn upgrade(&self) -> bool;
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
