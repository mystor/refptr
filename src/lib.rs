use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::ptr::NonNull;

pub mod control;

/// Attribute for declaring [`Refcounted`] structs.
///
/// The `#[refcounted]` attribute can be applied to a `struct` declaration to
/// mark it as refcounted. Refcounted structs are always allocated on the heap,
/// and are constructed using the `make_refptr` helper macro.
///
/// # Finalization and `Drop`
///
/// Types annotated with this attribute cannot manually implement `Drop`, as it
/// would allow recovering a `RefPtr<Self>` while the object is being dropped,
/// leading to a use-after-free.
///
/// If a finalization method is needed, the `#[refcounted(finalize)]` attribute
/// provides support for custom finalization. THis attribute causes a `fn
/// finalize(&self)` method before dropping any fields. It is possible for code
/// to acquire a new strong reference during the `finalize` method, which may
/// cause the struct to not be dropped after it returns. Because of this,
/// `finalize` may be called on the same struct multiple times over it's
/// lifetime.
///
/// Fields are dropped first-to-last, as in normal structs.
///
/// # Options
///
/// ## `#[refcounted(atomic)]`
///
/// Use atomic reference counting, allowing the struct to to be shared between
/// threads, so long as other fields implement `Send` and `Sync`.
///
/// ## `#[refcounted(nonatomic)]`
///
/// (default) Marks the struct as using non-atomic reference counts. Non-atomic
/// reference counting may be faster than atomic, but structs using it cannot be
/// shared between threads.
///
/// ## `#[refcounted(weak)]`
///
/// Adds support for weak reference counts and the [`WeakPtr`] smart pointer
/// type. This annotation may be combined with other annotations.
///
/// ## `#[refcounted(finalize)]`
///
/// Calls a `fn finalize(&self)` method on the struct before attempting to
/// destroy it. See the "Finalization" section for more details. This annotation
/// may be combined with other annotations.
pub use refptr_macros::refcounted;

/// Smart pointer for holding [`Refcounted`] objects.
pub struct RefPtr<T: ?Sized + Refcounted> {
    ptr: NonNull<T>,
    _marker: PhantomData<T>,
}

impl<T: ?Sized + Refcounted> RefPtr<T> {
    /// Acquire a strong reference to the passed-in object.
    pub fn new(val: &T) -> RefPtr<T> {
        unsafe {
            val.addref();
            RefPtr::from_raw(val)
        }
    }

    /// Consumes the `RefPtr`, returning the wrapped pointer.
    ///
    /// To avoid a leak, the pointer must be turned back into an `RefPtr` using
    /// [`RefPtr::from_raw`].
    pub fn into_raw(this: RefPtr<T>) -> *const T {
        let ptr = this.ptr.as_ptr();
        mem::forget(this);
        ptr
    }

    /// Constructs a `RefPtr` from this pointer.
    ///
    /// The raw pointer must have been returned from `RefPtr::into_raw`.
    ///
    /// This function is unsafe because improper use may lead to memory
    /// problems. For example, a double-free may occur if the function is called
    /// twice on the same raw pointer.
    pub unsafe fn from_raw(raw: *const T) -> RefPtr<T> {
        RefPtr {
            ptr: NonNull::new_unchecked(raw as *mut T),
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
        unsafe {
            T::release(self.ptr.as_ptr());
        }
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
            T::weak_addref(val);

            WeakPtr {
                ptr: val.into(),
                _marker: PhantomData,
            }
        }
    }

    pub fn upgrade(&self) -> Option<RefPtr<T>> {
        unsafe {
            let action = T::upgrade(self.ptr.as_ptr());
            action.take_action(self.ptr.as_ptr())
        }
    }
}

impl<T: ?Sized + WeakRefcounted> Clone for WeakPtr<T> {
    fn clone(&self) -> Self {
        unsafe {
            T::weak_addref(self.ptr.as_ptr());
        }

        WeakPtr {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T: ?Sized + WeakRefcounted> Drop for WeakPtr<T> {
    fn drop(&mut self) {
        unsafe {
            T::weak_release(self.ptr.as_ptr());
        }
    }
}

impl<T: ?Sized + WeakRefcounted> fmt::Debug for WeakPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(WeakPtr)")
    }
}

unsafe impl<T: ?Sized + WeakRefcounted + Sync + Send> Send for WeakPtr<T> {}
unsafe impl<T: ?Sized + WeakRefcounted + Sync + Send> Sync for WeakPtr<T> {}

/// The primary trait of this library. This trait is implemented by objects which
/// are invasively reference counted. It has a few different constraints put upon
/// those objects.
///
/// Objects of this type are always allocated on the heap, and have their
/// lifecycle managed using the [`RefPtr`] smart pointer.
///
/// ## Safety
///
///  * `Refcounted` objects are always heap-allocated
///  * Only shared references may exist to `Refcounted` objects
///  * If the `addref` method has been called more times than the `release`
///    method, the object must not be dropped.
///
/// ## Trait Objects
///
/// Unfortunately, the `Refcounted` interface is currently not object-safe, as
/// the `release` method takes a `this: *const Self`. This may change if Rust
/// adds support for raw pointer receivers on unsafe trait methods.
///
/// The `release` method cannot take `&self`, as it may be freed or become
/// partially invalid during the execution of the method, which would violate
/// reference type guarantees.
pub unsafe trait Refcounted {
    /// Increment the strong reference count for this object.
    ///
    /// Prefer managing the lifecycle of `Refcounted` objects with [`RefPtr`]
    /// over manually calling these methods.
    ///
    /// This must only be called while the strong reference count is at least 1.
    /// If only weak references only exist for this object, the `upgrade` method
    /// must be used instead.
    unsafe fn addref(&self);

    /// Decrement the strong reference count for this object.
    ///
    /// Prefer managing the lifecycle of `Refcounted` objects with [`RefPtr`]
    /// over manually calling these methods.
    unsafe fn release(this: *const Self);
}

pub unsafe trait WeakRefcounted: Refcounted {
    /// Increment the weak reference count of this object.
    ///
    /// Prefer managing the lifecycle of `WeakRefcounted` objects with
    /// [`WeakPtr`] over manually calling these methods.
    unsafe fn weak_addref(this: *const Self);

    /// Decrement the weak reference count of this object.
    ///
    /// Prefer managing the lifecycle of `WeakRefcounted` objects with
    /// [`WeakPtr`] over manually calling these methods.
    unsafe fn weak_release(this: *const Self);

    /// Attempt to obtain a new strong reference to this object.
    ///
    /// Prefer managing the lifecycle of `WeakRefcounted` objects with
    /// [`WeakPtr`] over manually calling these methods.
    ///
    /// This method will return `UpgradeAction::Upgrade` if the strong reference
    /// count was successfully incremented.
    unsafe fn upgrade(this: *const Self) -> control::UpgradeAction;
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

// Not public API.
#[doc(hidden)]
pub mod __rt {
    use crate::{RefPtr, Refcounted};
    pub use std::mem::ManuallyDrop;

    pub unsafe fn alloc<T: Refcounted>(value: ManuallyDrop<T>) -> RefPtr<T> {
        RefPtr::from_raw(Box::into_raw(Box::new(value)) as *mut T)
    }
}

/// Allocate a new instance of a [`Refcounted`] struct using a struct literal.
///
/// Returns a `RefPtr<T>` strong reference to the newly allocated struct.
///
/// # Example
///
/// ```
/// # use refptr::*;
/// #[refcounted]
/// struct HeapInt { value: i32 }
///
/// let ptr = make_refptr!(HeapInt { value: 10 });
/// ```
#[macro_export]
macro_rules! make_refptr {
    ($($seg:ident $(::<$($t:ty),*>)?)::+ { $($f:tt)* }) => {
        {
            let value = $crate::__rt::ManuallyDrop::new($($seg $(::<$($t),*>)?)::+ {
                refcnt: unsafe { $crate::control::ControlBlock::new() },
                $($f)*
            });

            unsafe { $crate::__rt::alloc(value) }
        }
    }
}
