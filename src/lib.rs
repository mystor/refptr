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
//! #[refcounted(local)]
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
//! # #[refcounted(local)] struct HeapPair<T, U> { t: T, u: U }
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
//! ## `#[refcounted(atomic)]` and `#[refcounted(local)]`
//!
//! Select between atomic reference counting, like [`Arc`], or thread local
//! reference counting, like [`Rc`]. Atomically refcounted types may be shared
//! between threads, so long as all fields are also sharable.
//!
//! The atomicity of the refcount must be specified.
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
//! #[refcounted(atomic, finalize)]
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

pub mod refcnt;
use refcnt::{Inner, Refcount, WeakRefcount};

// Not public API.
#[doc(hidden)]
#[path = "runtime.rs"]
pub mod __rt;

/// Attribute for declaring [`Refcounted`] structs.
///
/// See the [module level documentation](self) for usage.
pub use refptr_macros::refcounted;

/// An invasively reference counted type.
///
/// Objects implementing this trait are always allocated on the heap, and have
/// their lifecycle managed using the [`RefPtr`] smart pointer.
///
/// ## Safety
///
///  * `Refcounted` objects are always heap-allocated
///  * Only shared references may exist to `Refcounted` objects
pub unsafe trait Refcounted {
    /// Reference count used by this type.
    type Rc: Refcount;

    /// Metadata used internally by `Refcount` implementations.
    ///
    /// This metadata can be used to implement dynamic extensions to the
    /// refcount type, such as `finalize` support or RTTI.
    unsafe fn refcount_metadata(&self) -> <Self::Rc as Refcount>::Metadata;
}

/// Strong reference to a [`Refcounted`] object.
pub struct RefPtr<T: ?Sized + Refcounted> {
    ptr: NonNull<Inner<T>>,
    _marker: PhantomData<T>,
}

impl<T: ?Sized + Refcounted> RefPtr<T> {
    /// Obtain a strong reference to a `Refcounted` object.
    pub fn new(val: &T) -> RefPtr<T> {
        unsafe {
            let ptr = Inner::cast(val as *const T as *mut T);
            T::Rc::inc_strong(ptr);
            RefPtr::from_inner(ptr)
        }
    }

    /// Recover a `RefPtr` from a raw pointer which was previously returned from
    /// `into_raw`. This does not increment the reference count.
    pub unsafe fn from_raw(val: *const T) -> RefPtr<T> {
        RefPtr::from_inner(Inner::cast(val as *mut T))
    }

    /// Acquire a raw pointer to the allocation, consuming the `RefPtr`.
    pub fn into_raw(this: Self) -> *const T {
        let ptr = this.deref() as *const T;
        mem::forget(this);
        ptr
    }

    unsafe fn from_inner(ptr: *mut Inner<T>) -> RefPtr<T> {
        RefPtr {
            ptr: NonNull::new_unchecked(ptr),
            _marker: PhantomData,
        }
    }
}

impl<T: ?Sized + Refcounted> Deref for RefPtr<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &(*self.ptr.as_ptr()).data }
    }
}

impl<T: ?Sized + Refcounted> Clone for RefPtr<T> {
    fn clone(&self) -> Self {
        unsafe {
            T::Rc::inc_strong(self.ptr.as_ptr());
            RefPtr::from_inner(self.ptr.as_ptr())
        }
    }
}

impl<T: ?Sized + Refcounted> Drop for RefPtr<T> {
    fn drop(&mut self) {
        unsafe { T::Rc::dec_strong(self.ptr.as_ptr()) }
    }
}

unsafe impl<T: ?Sized + Refcounted + Sync + Send> Send for RefPtr<T> {}
unsafe impl<T: ?Sized + Refcounted + Sync + Send> Sync for RefPtr<T> {}

/// Weak reference to a [`Refcounted`] object.
///
/// Weak pointers can only be used on objects which have refcounts supporting
/// weak references.
///
/// # Example
///
/// ```
/// # use refptr::*;
/// # use std::thread;
/// #[refcounted(atomic, weak)]
/// struct HeapInt { i: i32 }
///
/// let here = make_refptr!(HeapInt { i: 10 });
/// let weak = WeakPtr::new(&*here);
/// assert_eq!(weak.upgrade().unwrap().i, 10);
/// drop(here);
/// assert!(weak.upgrade().is_none());
/// ```
pub struct WeakPtr<T: ?Sized>
where
    T: Refcounted,
    T::Rc: WeakRefcount,
{
    ptr: NonNull<Inner<T>>,
    _marker: PhantomData<T>,
}

impl<T: ?Sized> WeakPtr<T>
where
    T: Refcounted,
    T::Rc: WeakRefcount,
{
    /// Obtain a new weak reference to a refcounted object.
    pub fn new(val: &T) -> WeakPtr<T> {
        unsafe {
            let ptr = Inner::cast(val as *const T as *mut T);
            T::Rc::inc_weak(ptr);
            WeakPtr::from_inner(ptr)
        }
    }

    /// Attempt to upgrade this weak reference into a strong reference,
    /// returning it.
    pub fn upgrade(&self) -> Option<RefPtr<T>> {
        unsafe {
            if T::Rc::upgrade(self.ptr.as_ptr()) {
                Some(RefPtr::from_inner(self.ptr.as_ptr()))
            } else {
                None
            }
        }
    }

    /// Gets the number of strong references to this allocation.
    pub fn strong_count(&self) -> usize {
        unsafe { T::Rc::strong_count(self.ptr.as_ptr()) }
    }

    /// Gets the number of weak references to this allocation.
    ///
    /// If there are no remaining strong references, this will
    /// return `0`.
    pub fn weak_count(&self) -> usize {
        unsafe { T::Rc::weak_count(self.ptr.as_ptr()) }
    }

    unsafe fn from_inner(ptr: *mut Inner<T>) -> WeakPtr<T> {
        WeakPtr {
            ptr: NonNull::new_unchecked(ptr),
            _marker: PhantomData,
        }
    }
}

impl<T: ?Sized> Clone for WeakPtr<T>
where
    T: Refcounted,
    T::Rc: WeakRefcount,
{
    fn clone(&self) -> Self {
        unsafe {
            T::Rc::inc_weak(self.ptr.as_ptr());
            WeakPtr::from_inner(self.ptr.as_ptr())
        }
    }
}

impl<T: ?Sized> Drop for WeakPtr<T>
where
    T: Refcounted,
    T::Rc: WeakRefcount,
{
    fn drop(&mut self) {
        unsafe { T::Rc::dec_weak(self.ptr.as_ptr()) }
    }
}

impl<T: ?Sized> fmt::Debug for WeakPtr<T>
where
    T: Refcounted,
    T::Rc: WeakRefcount,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(WeakPtr)")
    }
}

unsafe impl<T: ?Sized> Send for WeakPtr<T>
where
    T: Refcounted + Send + Sync,
    T::Rc: WeakRefcount,
{
}
unsafe impl<T: ?Sized> Sync for WeakPtr<T>
where
    T: Refcounted + Send + Sync,
    T::Rc: WeakRefcount,
{
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

/// Allocate a new instance of a [`Refcounted`] struct using a struct literal.
///
/// Returns a `RefPtr<T>` strong reference to the newly allocated struct.
///
/// # Example
///
/// ```
/// # use refptr::*;
/// #[refcounted(local)]
/// struct HeapInt { value: i32 }
///
/// let ptr = make_refptr!(HeapInt { value: 10 });
/// ```
#[macro_export]
macro_rules! make_refptr {
    ($($seg:ident $(::<$($t:ty),*>)?)::+ { $($f:tt)* }) => {
        {
            let value = $crate::__rt::ManuallyDrop::new($($seg $(::<$($t),*>)?)::+ {
                _refcnt_marker: unsafe { $crate::__rt::PhantomRefcnt::new() },
                $($f)*
            });

            unsafe { $crate::__rt::alloc(value) }
        }
    }
}
