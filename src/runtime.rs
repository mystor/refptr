//! Not Public API

use crate::refcnt::{Inner, Refcount};
use crate::{RefPtr, Refcounted};
use alloc::boxed::Box;
use alloc::rc::Rc;
use core::marker::PhantomData;

pub use core::mem::ManuallyDrop;

#[cfg(feature = "atomic")]
use alloc::sync::Arc;

/// Helper function used by the `make_refptr!` macro.
pub unsafe fn alloc<T: Refcounted>(value: ManuallyDrop<T>) -> RefPtr<T> {
    RefPtr::from_inner(Box::into_raw(Box::new(Inner {
        refcnt: T::Rc::new(),
        data: value,
    })))
}

/// Helper trait used by the `make_refptr!` macro.
pub unsafe trait PhantomRefcnt {
    unsafe fn new() -> Self;
}

/// Marker type for a non-atomically refcounted type.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PhantomLocalRefcnt<T: ?Sized + Refcounted>(PhantomData<Rc<Inner<T>>>);

unsafe impl<T: ?Sized + Refcounted> PhantomRefcnt for PhantomLocalRefcnt<T> {
    unsafe fn new() -> Self {
        PhantomLocalRefcnt(PhantomData)
    }
}

/// Marker type for an atomically refcounted type.
#[cfg(feature = "atomic")]
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PhantomAtomicRefcnt<T: ?Sized + Refcounted>(PhantomData<Arc<Inner<T>>>);

#[cfg(feature = "atomic")]
unsafe impl<T: ?Sized + Refcounted> PhantomRefcnt for PhantomAtomicRefcnt<T> {
    unsafe fn new() -> Self {
        PhantomAtomicRefcnt(PhantomData)
    }
}
