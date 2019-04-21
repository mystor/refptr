#![recursion_limit = "128"]

extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, AttributeArgs, Error, Field, Fields, ItemStruct, Meta,
    NestedMeta, Visibility, Type,
};

macro_rules! fail {
    ($ts:expr, $err:expr) => {
        return Err(Error::new($ts.span(), $err));
    };
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum RcKind {
    Atomic,
    Nonatomic,
}

#[derive(Debug)]
struct Config {
    rc_kind: RcKind,
}

fn parse_config(args: AttributeArgs) -> Result<Config, Error> {
    let mut rc_kind: Option<RcKind> = None;

    for arg in args {
        match arg {
            NestedMeta::Meta(Meta::Word(word)) => {
                let name = word.to_string();
                match &name[..] {
                    "atomic" => {
                        if rc_kind.is_some() {
                            fail!(word, "duplicate atomicity argument");
                        }
                        rc_kind = Some(RcKind::Atomic);
                    }
                    "nonatomic" => {
                        if rc_kind.is_some() {
                            fail!(word, "duplicate atomicity argument");
                        }
                        rc_kind = Some(RcKind::Nonatomic);
                    }
                    _ => fail!(word, "unexpected refcounted argument"),
                }
            }
            meta => fail!(meta, "unexpected refcounted argument"),
        }
    }

    Ok(Config {
        rc_kind: rc_kind.unwrap_or(RcKind::Nonatomic),
    })
}

fn refcounted_impl(args: AttributeArgs, mut item: ItemStruct) -> Result<TokenStream, Error> {
    let cfg = parse_config(args)?;

    let name = item.ident.clone();
    let rc_field_ty: Type = match cfg.rc_kind {
        RcKind::Nonatomic => parse_quote!(::refcounted::Refcnt),
        RcKind::Atomic => parse_quote!(::refcounted::AtomicRefcnt),
    };

    // Add our _refcnt field, and extract the original fields
    let orig_fields;
    match &mut item.fields {
        Fields::Named(fields) => {
            orig_fields = fields.named.clone();

            let rc_field = Field {
                attrs: Vec::new(),
                vis: Visibility::Inherited,
                ident: parse_quote!(_refcnt),
                colon_token: parse_quote!(:),
                ty: rc_field_ty.clone(),
            };
            fields.named.insert(0, rc_field);
        }
        _ => fail!(item, "refcounted struct must have named fields"),
    }

    let (impl_generics, ty_generics, where_clause) = item.generics.split_for_impl();

    let impl_refcounted = quote!{
        unsafe impl #impl_generics ::refcounted::Refcounted for #name #ty_generics #where_clause {
            #[inline]
            unsafe fn addref(&self) {
                self._refcnt.inc();
            }

            #[inline]
            unsafe fn release(&self) {
                if self._refcnt.dec() {
                    // XXX(nika): We can't allow `drop` to be directly
                    // implemented on the type!
                    Box::from_raw(self as *const Self as *mut Self);
                }
            }
        }
    };

    let params = orig_fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        quote!(#name : #ty,)
    }).collect::<TokenStream>();
    let inits = orig_fields.iter().map(|field| {
        let name = &field.ident;
        quote!(#name,)
    }).collect::<TokenStream>();

    // Expose an inherent `alloc` method which allocates the object onto the heap.
    let impl_alloc = quote!{
        impl #impl_generics #name #ty_generics #where_clause {
            #[inline]
            fn alloc(#params) -> ::refcounted::RefPtr<Self> {
                let raw = Box::into_raw(Box::new(#name {
                    _refcnt: unsafe { #rc_field_ty::new() },
                    #inits
                }));

                unsafe { ::refcounted::RefPtr::new(&*raw) }
            }
        }
    };

    // Prevent any other implementations of `Drop`, as they can be unsound (due
    // to being able to create a `RefPtr` to a dropped struct).
    let impl_drop = quote!{
        impl #impl_generics Drop for #name #ty_generics #where_clause {
            fn drop(&mut self) {
                debug_assert!(self._refcnt.get() == 0);
            }
        }
    };

    Ok(quote!{
        #item
        #impl_refcounted
        #impl_alloc
        #impl_drop
    })
}

#[test]
fn test_refcounted() {
    let args = Vec::new();
    let input = parse_quote! {
        struct Foo {
            a: i32,
            b: u32,
        }
    };

    println!("{}", refcounted_impl(args, input).unwrap());
    panic!();
}

#[proc_macro_attribute]
pub fn refcounted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let input = parse_macro_input!(input as ItemStruct);

    match refcounted_impl(args, input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
