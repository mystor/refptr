# refptr

[![Latest Version](https://img.shields.io/crates/v/refptr.svg)](https://crates.io/crates/refptr)
[![Documentation](https://docs.rs/refptr/badge.svg)](https://docs.rs/refptr)
[![Build Status](https://travis-ci.org/mystor/refptr.svg?branch=master)](https://travis-ci.org/mystor/refptr)

Macros, attributes, and traits for invasively reference-counted structs in Rust.
See the documentation for more details.

## Usage

Structs which support being referenced using `RefPtr` are annotated with the
`#[refcounted(...)]` attribute. This attribute generates the necessary unsafe
code, extra members, and trait implementations required.

### Configuration

The reference counting behaviour of the target type is controlled using flags
passed to `#[refcounted(...)]`. The following are the currently supported config
options:

* `atomic` - Use an atomic reference count, like
  [`std::sync::Arc`](https://doc.rust-lang.org/std/sync/struct.Arc.html). This
  is the default.

* `nonatomic` - Use a non-atomic reference count, like
  [`std::rc::Rc`](https://doc.rust-lang.org/std/rc/struct.Rc.html).

* `weak` - Adds support for weak references, which can be used to break cycles,
  like [`std::sync::Weak`](https://doc.rust-lang.org/std/sync/struct.Weak.html).
  
* `finalize` - Types with this attribute cannot manually implement `Drop`, as
  doing so would be unsound. If set, this will invoke a sound `finalize` method
  instead.
  
  ```rust
  impl MyType {
      fn finalize(&self) {
          // Perform pre-field-dropping cleanup here.
      }
  }
  ```
  
  Unlike `drop`, `finalize` may be called multiple times if it causes a
  reference to the object to escape. Fields will be dropped as-usual.

