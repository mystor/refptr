//! Objects which act as control headers for refcounted objects. All of these
//! objects are wildly unsafe to create outside of autogenerated code, and
//! generally shouldn't be used by users of this crate.

use std::cell::Cell;
use std::process::abort;
use std::sync::atomic::Ordering::{Acquire, Relaxed, Release, SeqCst};
use std::sync::atomic::{self, AtomicUsize};
use std::fmt;
use std::mem::ManuallyDrop;

use crate::Refcounted;

unsafe fn drop_fields<T: Refcounted>(ptr: &T) {
    ptr.drop_fields();
}

unsafe fn free_storage<T: Refcounted>(ptr: &T) {
    drop(Box::from_raw(ptr as *const T as *mut ManuallyDrop<T>));
}


/// A soft limit on the amount of references that may be made to an AtomicRefcnt.
///
/// Going above this limit will abort your program (although not necessarily) at
/// _exactly_ `MAX_REFCOUNT + 1` references.
const MAX_REFCOUNT: usize = isize::max_value() as usize;

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
            val: AtomicUsize::new(1),
        }
    }

    #[inline]
    pub fn get_strong(&self) -> usize {
        self.val.load(SeqCst)
    }

    #[inline]
    pub unsafe fn inc_strong(&self) {
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
    pub unsafe fn dec_strong<T: Refcounted>(&self, ptr: &T) {
        // If we've decreased to a non-zero value, we can immediately return.
        let old_count = self.val.fetch_sub(1, Release);
        if old_count != 1 {
            return;
        }

        // We're about to drop the object in question due to the count reaching
        // zero. As documented in the `Arc` implementation, we need to be sure
        // that any other writes (e.g. in interior mutability) are visible to
        // our thread before we release, so we need to perform an `Acquire`
        // fence here.
        atomic::fence(Acquire);

        // Drop any data currently stored in this actor, and free the backing
        // memory.
        drop_fields(ptr);
        free_storage(ptr);
    }
}

impl fmt::Debug for AtomicRefcnt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("AtomicRefcnt")
            .field(&self.get_strong())
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
        Refcnt { val: Cell::new(1) }
    }

    #[inline]
    pub fn get_strong(&self) -> usize {
        self.val.get()
    }

    #[inline]
    pub unsafe fn inc_strong(&self) {
        if self.val.get() == usize::max_value() {
            abort();
        }
        self.val.set(self.val.get() + 1);
    }

    #[inline]
    pub unsafe fn dec_strong<T: Refcounted>(&self, ptr: &T) {
        self.val.set(self.val.get() - 1);

        if self.val.get() == 0 {
            // Drop any data currently stored in this actor, and free the
            // backing memory.
            drop_fields(ptr);
            free_storage(ptr);
        }
    }
}

impl fmt::Debug for Refcnt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Refcnt")
            .field(&self.get_strong())
            .finish()
    }
}

/// Atomic internal reference count.
pub struct AtomicRefcntWeak {
    strong: AtomicUsize,
    weak: AtomicUsize,
}

impl AtomicRefcntWeak {
    /// Create a new `AtomicRefcnt`
    ///
    /// This method is unsafe to prevent creation of `Refcounted` types outside
    /// of generated methods.
    #[inline]
    pub unsafe fn new() -> Self {
        AtomicRefcntWeak {
            strong: AtomicUsize::new(1),

            // The weak reference starts at 1, as the strong refcount holds a
            // weak reference to keep the object alive.
            weak: AtomicUsize::new(1),
        }
    }

    #[inline]
    pub fn get_strong(&self) -> usize {
        self.strong.load(SeqCst)
    }

    #[inline]
    pub unsafe fn inc_strong(&self) {
        // Using a relaxed ordering is OK here, as knowledge of an existing
        // reference prevents other threads from erroneously deleting the object
        // while we're incrementing.
        //
        // As we have a reference to `self`, we know there is a live reference
        // to our containing object which this count is referring to.
        //
        // This is documented in more detail in the rustc source code for `Arc`.
        let old_count = self.strong.fetch_add(1, Relaxed);

        // Guard against massive reference counts by aborting if you exceed
        // `isize::MAX`.
        if old_count > MAX_REFCOUNT {
            abort();
        }
    }

    #[inline]
    pub unsafe fn dec_strong<T: Refcounted>(&self, ptr: &T) {
        // If we've decreased to a non-zero value, we can immediately return.
        let old_count = self.strong.fetch_sub(1, Release);
        if old_count != 1 {
            return;
        }

        // We're about to drop the object in question due to the count reaching
        // zero. As documented in the `Arc` implementation, we need to be sure
        // that any other writes (e.g. in interior mutability) are visible to
        // our thread before we release, so we need to perform an `Acquire`
        // fence here.
        atomic::fence(Acquire);

        // Drop any remaining fields, and then drop our self-weak-reference,
        // which may cause the backing storage to also be freed.
        drop_fields(ptr);
        self.dec_weak(ptr);
    }

    #[inline]
    pub fn get_weak(&self) -> usize {
        self.weak.load(SeqCst)
    }

    #[inline]
    pub unsafe fn inc_weak(&self) {
        // See the documentation for inc_strong for more details on why this is
        // OK to be `Relaxed`.
        let old_count = self.weak.fetch_add(1, Relaxed);

        // Guard against massive reference counts by aborting if you exceed
        // `isize::MAX`.
        if old_count > MAX_REFCOUNT {
            abort();
        }
    }

    #[inline]
    pub unsafe fn dec_weak<T: Refcounted>(&self, ptr: &T) {
        // If we've decreased to a non-zero value, we can immediately return.
        let old_count = self.weak.fetch_sub(1, Release);
        if old_count != 1 {
            return;
        }

        // We're about to free the object in question due to the count reaching
        // zero. As documented in the `Arc` implementation, we need to be sure
        // that any other writes (e.g. in interior mutability) are visible to
        // our thread before we free the memory, so we need to perform an
        // `Acquire` fence here.
        atomic::fence(Acquire);

        // If we reach zero, our strong reference count must have already
        // reached 0, so we can free any backing storage.
        debug_assert!(self.get_strong() == 0);
        free_storage(ptr);
    }

    #[inline]
    pub unsafe fn upgrade(&self) -> bool {
        // We use a CAS loop to increment the strong count instead of a
        // fetch_add because once the count hits 0 it must never be above 0.

        // Relaxed load because any write of 0 that we can observe
        // leaves the field in a permanently zero state (so a
        // "stale" read of 0 is fine), and any other value is
        // confirmed via the CAS below.
        let mut count = self.strong.load(Relaxed);
        loop {
            if count == 0 {
                return false;
            }

            if count > MAX_REFCOUNT - 1 {
                abort();
            }

            // Relaxed is valid for the same reason it is on `inc_strong`.
            match self.strong.compare_exchange_weak(count, count + 1, Relaxed, Relaxed) {
                Ok(_) => return true,
                Err(old) => count = old,
            }
        }
    }
}

impl fmt::Debug for AtomicRefcntWeak {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AtomicRefcntWeak")
            .field("strong", &self.get_strong())
            .field("weak", &self.get_weak())
            .finish()
    }
}


/// Non-atomic internal reference count with weak ref support.
pub struct RefcntWeak {
    strong: Cell<usize>,
    weak: Cell<usize>,
}

impl RefcntWeak {
    /// Create a new `RefcntWeak`
    ///
    /// This method is unsafe to prevent creation of `Refcounted` types outside
    /// of generated methods.
    #[inline]
    pub unsafe fn new() -> Self {
        RefcntWeak {
            strong: Cell::new(1),
            weak: Cell::new(1),
        }
    }

    #[inline]
    pub fn get_strong(&self) -> usize {
        self.strong.get()
    }

    #[inline]
    pub unsafe fn inc_strong(&self) {
        if self.strong.get() == usize::max_value() {
            abort();
        }
        self.strong.set(self.strong.get() + 1);
    }

    #[inline]
    pub unsafe fn dec_strong<T: Refcounted>(&self, ptr: &T) {
        self.strong.set(self.strong.get() - 1);

        if self.strong.get() == 0 {
            // Drop any data currently stored in this actor, and remove the
            // implicit self weak-reference.
            drop_fields(ptr);
            self.dec_weak(ptr)
        }
    }

    #[inline]
    pub fn get_weak(&self) -> usize {
        self.weak.get()
    }

    #[inline]
    pub unsafe fn inc_weak(&self) {
        if self.weak.get() == usize::max_value() {
            abort();
        }
        self.weak.set(self.weak.get() + 1);
    }

    #[inline]
    pub unsafe fn dec_weak<T: Refcounted>(&self, ptr: &T) {
        self.weak.set(self.weak.get() - 1);

        if self.weak.get() == 0 {
            free_storage(ptr);
        }
    }

    #[inline]
    pub unsafe fn upgrade(&self) -> bool {
        let strong = self.strong.get();
        if strong == 0 {
            return false;
        }

        if strong == usize::max_value() {
            abort();
        }
        self.strong.set(strong + 1);
        true
    }
}

impl fmt::Debug for RefcntWeak {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RefcntWeak")
            .field("strong", &self.get_strong())
            .field("weak", &self.get_weak())
            .finish()
    }
}
