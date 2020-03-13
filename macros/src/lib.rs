extern crate proc_macro;

use proc_macro2::TokenStream;
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
    Nonatomic,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum WeakKind {
    NonWeak,
    Weak,
}

#[derive(Debug)]
struct Config {
    rc_kind: RcKind,
    weak_kind: WeakKind,
    has_finalize: bool,
}

fn parse_config(args: AttributeArgs) -> Result<Config, Error> {
    let mut rc_kind: Option<RcKind> = None;
    let mut weak_kind = WeakKind::NonWeak;
    let mut has_finalize = false;

    for arg in args {
        use syn::{Meta::*, NestedMeta::*};
        match arg {
            Meta(Path(path)) if path.is_ident("atomic") => {
                if rc_kind.is_some() {
                    fail!(path, "duplicate atomicity argument");
                }
                rc_kind = Some(RcKind::Atomic);
            }
            Meta(Path(path)) if path.is_ident("nonatomic") => {
                if rc_kind.is_some() {
                    fail!(path, "duplicate atomicity argument");
                }
                rc_kind = Some(RcKind::Nonatomic);
            }
            Meta(Path(path)) if path.is_ident("weak") => {
                if weak_kind == WeakKind::Weak {
                    fail!(path, "duplicate weak argument");
                }
                weak_kind = WeakKind::Weak;
            }
            Meta(Path(path)) if path.is_ident("finalize") => {
                if has_finalize {
                    fail!(path, "duplicate finalize argument");
                }
                has_finalize = true;
            }
            meta => fail!(meta, "unexpected refcounted argument"),
        }
    }

    let rc_kind = rc_kind.unwrap_or(RcKind::Nonatomic);
    Ok(Config {
        rc_kind,
        weak_kind,
        has_finalize,
    })
}

fn refcounted_impl(args: AttributeArgs, mut item: DeriveInput) -> Result<TokenStream, Error> {
    let cfg = parse_config(args)?;

    // FIXME: Handle `finalize`.
    let rc_ty = match (cfg.rc_kind, cfg.weak_kind) {
        (RcKind::Nonatomic, WeakKind::NonWeak) => quote!(Refcnt),
        (RcKind::Atomic, WeakKind::NonWeak) => quote!(AtomicRefcnt),
        (RcKind::Nonatomic, WeakKind::Weak) => quote!(RefcntWeak),
        (RcKind::Atomic, WeakKind::Weak) => quote!(AtomicRefcntWeak),
    };

    // Add our refcnt field, and extract the original fields
    match &mut item.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => {
            let ty: Type = match cfg.rc_kind {
                RcKind::Atomic => parse_quote!(::refptr::__rt::PhantomAtomicRefcnt<Self>),
                RcKind::Nonatomic => parse_quote!(::refptr::__rt::PhantomLocalRefcnt<Self>),
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
    let impl_refcounted = quote! {
        unsafe impl #impl_generics ::refptr::Refcounted for #ident #ty_generics #where_clause {
            type Rc = ::refptr::refcnt::#rc_ty;
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
