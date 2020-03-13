extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, AttributeArgs, Data, DataStruct, DeriveInput, Error, Field,
    Fields, Type, Visibility,
};

macro_rules! fail {
    ($ts:expr, $err:expr) => {
        return Err(Error::new_spanned($ts, $err));
    };
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum RcKind {
    Atomic,
    Local,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum WeakKind {
    NonWeak,
    Weak,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum FinalizeKind {
    Finalize,
    Pod,
}

#[derive(Debug)]
struct Config {
    rc_kind: RcKind,
    weak_kind: WeakKind,
    finalize_kind: FinalizeKind,
}

fn parse_config(args: AttributeArgs) -> Result<Config, Error> {
    let mut rc_kind: Option<RcKind> = None;
    let mut weak_kind = WeakKind::NonWeak;
    let mut finalize_kind = FinalizeKind::Pod;

    for arg in args {
        use syn::{Meta::*, NestedMeta::*};
        match arg {
            Meta(Path(path)) if path.is_ident("atomic") => {
                if rc_kind.is_some() {
                    fail!(path, "duplicate atomicity argument");
                }
                rc_kind = Some(RcKind::Atomic);
            }
            Meta(Path(path)) if path.is_ident("local") => {
                if rc_kind.is_some() {
                    fail!(path, "duplicate atomicity argument");
                }
                rc_kind = Some(RcKind::Local);
            }
            Meta(Path(path)) if path.is_ident("weak") => {
                if weak_kind == WeakKind::Weak {
                    fail!(path, "duplicate weak argument");
                }
                weak_kind = WeakKind::Weak;
            }
            Meta(Path(path)) if path.is_ident("finalize") => {
                if finalize_kind == FinalizeKind::Finalize {
                    fail!(path, "duplicate finalize argument");
                }
                finalize_kind = FinalizeKind::Finalize;
            }
            meta => fail!(meta, "unexpected refcounted argument"),
        }
    }

    if let Some(rc_kind) = rc_kind {
        Ok(Config {
            rc_kind,
            weak_kind,
            finalize_kind,
        })
    } else {
        return Err(Error::new(
            Span::call_site(),
            "must specify either `local` or `atomic` atomicity",
        ));
    }
}

fn refcounted_impl(args: AttributeArgs, mut item: DeriveInput) -> Result<TokenStream, Error> {
    let cfg = parse_config(args)?;

    use {FinalizeKind::*, RcKind::*, WeakKind::*};
    let rc_ty = match (cfg.rc_kind, cfg.weak_kind, cfg.finalize_kind) {
        (Local, NonWeak, Pod) => quote!(Local),
        (Atomic, NonWeak, Pod) => quote!(Atomic),
        (Local, Weak, Pod) => quote!(LocalWeak),
        (Atomic, Weak, Pod) => quote!(AtomicWeak),
        (Local, NonWeak, Finalize) => quote!(LocalFinalize),
        (Atomic, NonWeak, Finalize) => quote!(AtomicFinalize),
        (Local, Weak, Finalize) => quote!(LocalWeakFinalize),
        (Atomic, Weak, Finalize) => quote!(AtomicWeakFinalize),
    };

    // Add our refcnt field, and extract the original fields
    match &mut item.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => {
            let ty: Type = match cfg.rc_kind {
                RcKind::Atomic => parse_quote!(::refptr::__rt::PhantomAtomicRefcnt<Self>),
                RcKind::Local => parse_quote!(::refptr::__rt::PhantomLocalRefcnt<Self>),
            };

            let rc_field = Field {
                attrs: Vec::new(),
                vis: Visibility::Inherited,
                ident: parse_quote!(_refcnt_marker),
                colon_token: Default::default(),
                ty,
            };
            fields.named.insert(0, rc_field);
        }
        _ => fail!(
            item,
            "refcounted must be used on a struct with named fields"
        ),
    }

    let ident = &item.ident;
    let (impl_generics, ty_generics, where_clause) = item.generics.split_for_impl();

    let metadata_func = match cfg.finalize_kind {
        Pod => quote! {
            unsafe fn refcount_metadata(&self) { }
        },
        Finalize => quote! {
            unsafe fn refcount_metadata(&self) -> unsafe fn (*const u8) {
                unsafe fn finalize_helper #impl_generics (this: *const u8) #where_clause {
                    let _assert_finalize_type: fn (&#ident #ty_generics) =
                        <#ident #ty_generics>::finalize;
                    <#ident #ty_generics>::finalize(&*(this as *const #ident #ty_generics));
                }
                finalize_helper #ty_generics
            }
        },
    };

    let impl_refcounted = quote! {
        unsafe impl #impl_generics ::refptr::Refcounted for #ident #ty_generics #where_clause {
            type Rc = ::refptr::refcnt::#rc_ty;

            #metadata_func
        }
    };

    // Prevent any other implementations of `Drop`, as they can be unsound (due
    // to being able to create a `RefPtr` to a dropped struct).
    let impl_drop = quote! {
        impl #impl_generics Drop for #ident #ty_generics #where_clause {
            fn drop(&mut self) { /* no-op */ }
        }
    };

    Ok(quote! {
        #item
        #impl_refcounted
        #impl_drop
    })
}

#[proc_macro_attribute]
pub fn refcounted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let input = parse_macro_input!(input as DeriveInput);

    match refcounted_impl(args, input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
