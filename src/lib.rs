//! Macros, attributes, and traits for invasively reference-counted structs in Rust.
//!
//! This crate is centered around manipulating invasively reference counted
//! structs. These structs are declared using the `#[refcounted]` attribute,
//! constructed with the [`make_refptr`] macro, and have their lifetimes managed
//! using the [`RefPtr`] and [`WeakPtr`] smart pointer types.
//!
//! # Declaring a refcounted struct
//!
//! The `#[refcounted]` attribute can be applied to a `struct` declaration to
//! mark it as refcounted. Refcounted structs are always allocated on the heap,
//! and are constructed using the `make_refptr` helper macro.
//!
//! ## Example
//!
//! ```
//! # use refptr::*;
//! # use std::cell::Cell;
//! #[refcounted]
//! struct HeapInteger {
//!     value: Cell<i32>,
//! }
//!
//! let orig = make_refptr!(HeapInteger { value: Cell::new(10) });
//! let copy = orig.clone();
//! orig.value.set(20);
//! assert_eq!(copy.value.get(), 20);
//! ```
//!
//! # Allocating
//!
//! Structs declared with `#[refcounted]` are constructed on the heap using the
//! [`make_refptr!`] macro. This macro accepts struct literal syntax, but
//! constructs the value onto the heap.
//!
//! This is required in order to ensure that the type always lives on the heap
//! for invasive reference counting.
//!
//! ## Example
//!
//! ```
//! # use refptr::*;
//! # #[refcounted] struct HeapPair<T, U> { t: T, u: U }
//! let ptr = make_refptr!(HeapPair { t: 10, u: 20 });
//! assert_eq!(ptr.t, 10);
//! assert_eq!(ptr.u, 20);
//! ```
//!
//! # Finalization and `Drop`
//!
//! Types annotated with `#[refcounted]` cannot manually implement `Drop`, as it
//! would allow recovering a `RefPtr<Self>` while the object is being dropped,
//! leading to a use-after-free.
//!
//! If a finalization method is needed, the `#[refcounted(finalize)]` attribute
//! provides support for custom finalization. If finalization is enabled, a `fn
//! finalize(&self)` method is called before dropping any fields.
//!
//! It is possible for code to acquire a new strong reference during the
//! `finalize` method, which may cause the struct to not be dropped after it
//! returns. Because of this, `finalize` may be called on the same struct
//! multiple times over it's lifetime.
//!
//! # Configuration
//!
//! ## `#[refcounted(atomic)]` and `#[refcounted(nonatomic)]`
//!
//! Select between atomic reference counting, like [`Arc`], or nonatomic
//! reference counting, like [`Rc`]. Atomically refcounted types may be shared
//! between threads, so long as all fields are also sharable.
//!
//! The default behaviour is to use nonatomic reference counts.
//!
//! ### Example
//!
//! ```
//! # use refptr::*;
//! # use std::thread;
//! #[refcounted(atomic)]
//! struct HeapInt { i: i32 }
//!
//! let here = make_refptr!(HeapInt { i: 10 });
//! let thread = thread::spawn(move || here.i);
//! assert_eq!(thread.join().unwrap(), 10);
//! ```
//!
//! [`Arc`]: alloc::sync::Arc
//! [`Rc`]: alloc::rc::Rc
//!
//! ## `#[refcounted(weak)]`
//!
//! Adds support for weak reference counts and the [`WeakPtr`] smart pointer
//! type. This annotation may be combined with other annotations.
//!
//! ### Example
//!
//! ```
//! # use refptr::*;
//! # use std::thread;
//! #[refcounted(atomic, weak)]
//! struct HeapInt { i: i32 }
//!
//! let here = make_refptr!(HeapInt { i: 10 });
//! let weak = WeakPtr::new(&*here);
//! assert_eq!(weak.upgrade().unwrap().i, 10);
//! drop(here);
//! assert!(weak.upgrade().is_none());
//! ```
//!
//! ## `#[refcounted(finalize)]`
//!
//! Calls a `fn finalize(&self)` method on the struct before attempting to
//! destroy it. See the "Finalization" section for more details. This annotation
//! may be combined with other annotations.
//!
//! Structs which support being referenced using [`RefPtr`] are annotated with the
//! `#[refcounted(...)]` attribute. This attribute generates the necessary unsafe
//! code, extra members, and trait implementations required.
//!
//! ```
//! # use refptr::*;
//! # use std::sync::atomic::{AtomicBool, Ordering::SeqCst};
//! #[refcounted(finalize)]
//! struct FinalizeExample {}
//!
//! static FINALIZED: AtomicBool = AtomicBool::new(false);
//! impl FinalizeExample {
//!     fn finalize(&self) {
//!         FINALIZED.store(true, SeqCst);
//!     }
//! }
//!
//! let orig = make_refptr!(FinalizeExample {});
//! assert_eq!(FINALIZED.load(SeqCst), false);
//! let copy = orig.clone();
//! assert_eq!(FINALIZED.load(SeqCst), false);
//! drop(orig);
//! assert_eq!(FINALIZED.load(SeqCst), false);
//! drop(copy);
//! assert_eq!(FINALIZED.load(SeqCst), true);
//! ```

#![no_std]

extern crate alloc;

use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::mem;
use core::ops::Deref;
use core::ptr::NonNull;

pub mod control;

/// Attribute for declaring [`Refcounted`] structs.
///
/// See the [module level documentation](self) for usage.
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

    /// Gets the number of strong references to this allocation.
    pub fn strong_count(this: &T) -> usize {
        unsafe { T::strong_count(this) }
    }

    /// Gets the number of weak references to this allocation.
    ///
    /// If there are no remaining strong references, this will
    /// return `0`.
    pub fn weak_count(this: &T) -> usize
    where
        T: WeakRefcounted,
    {
        unsafe { T::weak_count(this) }
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

/// Weak reference to a [`WeakRefcounted`] object.
pub struct WeakPtr<T: ?Sized + WeakRefcounted> {
    ptr: NonNull<T>,
    _marker: PhantomData<T>,
}

impl<T: ?Sized + WeakRefcounted> WeakPtr<T> {
    /// Obtain a new weak reference to a refcounted object.
    pub fn new(val: &T) -> WeakPtr<T> {
        unsafe {
            T::weak_addref(val);

            WeakPtr {
                ptr: val.into(),
                _marker: PhantomData,
            }
        }
    }

    /// Attempt to upgrade this weak reference into a strong reference,
    /// returning it.
    pub fn upgrade(&self) -> Option<RefPtr<T>> {
        unsafe {
            let action = T::upgrade(self.ptr.as_ptr());
            action.take_action(self.ptr.as_ptr())
        }
    }

    /// Gets the number of strong references to this allocation.
    pub fn strong_count(&self) -> usize {
        unsafe { T::strong_count(self.ptr.as_ptr()) }
    }

    /// Gets the number of weak references to this allocation.
    ///
    /// If there are no remaining strong references, this will
    /// return `0`.
    pub fn weak_count(&self) -> usize {
        unsafe { T::weak_count(self.ptr.as_ptr()) }
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

/// An invasively reference counted type.
///
/// Objects implementing this trait are always allocated on the heap, and have
/// their lifecycle managed using the [`RefPtr`] smart pointer.
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

    /// Gets the number of strong references to this allocation.
    unsafe fn strong_count(this: *const Self) -> usize;
}

/// An invasively reference counted type supporting weak references.
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

    /// Gets the number of weak references to this allocation.
    ///
    /// If there are no remaining strong references, this will
    /// return `0`.
    unsafe fn weak_count(this: *const Self) -> usize;
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
    use alloc::boxed::Box;
    pub use core::mem::ManuallyDrop;

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
