extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, AttributeArgs, Error, Field, Fields, ItemStruct, Meta,
    NestedMeta, Type, Visibility,
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
                    "weak" => {
                        if weak_kind == WeakKind::Weak {
                            fail!(word, "duplicate weak argument");
                        }
                        weak_kind = WeakKind::Weak;
                    }
                    "finalize" => {
                        if has_finalize {
                            fail!(word, "duplicate finalize argument");
                        }
                        has_finalize = true;
                    }
                    _ => fail!(word, "unexpected refcounted argument"),
                }
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

fn refcounted_impl(args: AttributeArgs, mut item: ItemStruct) -> Result<TokenStream, Error> {
    let cfg = parse_config(args)?;

    let name = item.ident.clone();
    let rc_field_ty: Type = match (cfg.rc_kind, cfg.weak_kind) {
        (RcKind::Nonatomic, WeakKind::NonWeak) => parse_quote!(::rcptr::control::Refcnt<Self>),
        (RcKind::Atomic, WeakKind::NonWeak) => parse_quote!(::rcptr::control::AtomicRefcnt<Self>),
        (RcKind::Nonatomic, WeakKind::Weak) => parse_quote!(::rcptr::control::RefcntWeak<Self>),
        (RcKind::Atomic, WeakKind::Weak) => parse_quote!(::rcptr::control::AtomicRefcntWeak<Self>),
    };

    // Add our refcnt field, and extract the original fields
    let orig_fields;
    match &mut item.fields {
        Fields::Named(fields) => {
            orig_fields = fields.named.clone();

            let rc_field = Field {
                attrs: Vec::new(),
                vis: Visibility::Inherited,
                ident: parse_quote!(refcnt),
                colon_token: parse_quote!(:),
                ty: rc_field_ty.clone(),
            };
            fields.named.insert(0, rc_field);
        }
        _ => fail!(item, "refcounted struct must have named fields"),
    }

    let (impl_generics, ty_generics, where_clause) = item.generics.split_for_impl();

    // drop_in_place each non-added field in the struct.
    let drop_fields = orig_fields
        .iter()
        .map(|field| {
            let name = &field.ident;
            let ty = &field.ty;
            quote! {
                ::std::ptr::drop_in_place((&(*this).#name) as *const #ty as *mut #ty);
            }
        })
        .collect::<TokenStream>();

    let release = if cfg.has_finalize {
        quote! {
            (*this).refcnt.dec_strong_finalize(
                || { #drop_fields },
                || (*this).finalize(),
            )
        }
    } else {
        quote! {
            (*this).refcnt.dec_strong(|| { #drop_fields })
        }
    };

    let impl_refcounted = quote! {
        unsafe impl #impl_generics ::rcptr::Refcounted for #name #ty_generics #where_clause {
            #[inline]
            unsafe fn addref(&self) {
                self.refcnt.inc_strong()
            }

            #[inline]
            unsafe fn release(this: *const Self) {
                #release.take_action(this)
            }
        }
    };

    let impl_weak = if cfg.weak_kind == WeakKind::Weak {
        quote! {
            unsafe impl #impl_generics ::rcptr::WeakRefcounted for #name #ty_generics #where_clause {
                #[inline]
                unsafe fn weak_addref(this: *const Self) {
                    (*this).refcnt.inc_weak()
                }

                #[inline]
                unsafe fn weak_release(this: *const Self) {
                    (*this).refcnt.dec_weak().take_action(this)
                }

                #[inline]
                unsafe fn upgrade(this: *const Self) -> ::rcptr::control::UpgradeAction {
                    (*this).refcnt.upgrade()
                }
            }
        }
    } else {
        quote!()
    };

    // Prevent any other implementations of `Drop`, as they can be unsound (due
    // to being able to create a `RefPtr` to a dropped struct).
    let impl_drop = quote! {
        impl #impl_generics Drop for #name #ty_generics #where_clause {
            fn drop(&mut self) {
                unreachable!("Drop never called on Refcounted types");
            }
        }
    };

    Ok(quote! {
        #item
        #impl_refcounted
        #impl_weak
        #impl_drop
    })
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
