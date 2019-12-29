extern crate proc_macro;

use inflector::Inflector;
use pmutil::{smart_quote, Quote, ToTokensExt};
use proc_macro2::Span;
use quote::quote;
use syn::punctuated::{Pair, Punctuated};
use syn::spanned::Spanned;
use syn::{
    parse, Data, DataEnum, DeriveInput, Field, Fields, Generics, Ident, ImplItem, ItemImpl, Path,
    Token, Type, TypePath, TypeTuple, WhereClause,
};

/// A proc macro to generate methods like is_varaiant / expect_variant.
///
///
/// # Example
///
/// ```rust
///
/// use is_macro::Is;
/// #[derive(Debug, Is)]
/// pub enum Enum<T> {
///     A,
///     B(T),
///     C(Option<T>),
/// }
///
/// // Rust's type inference cannot handle this.
/// assert!(Enum::<()>::A.is_a());
///
/// assert_eq!(Enum::B(String::from("foo")).expect_b(), String::from("foo"));
/// ```
#[proc_macro_derive(Is)]
pub fn is(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse(input).expect("failed to parse derive input");
    let generics: Generics = input.generics.clone();

    let items = match input.data {
        Data::Enum(e) => expand(e),
        _ => panic!("`Is` can be applied only on enums"),
    };

    ItemImpl {
        attrs: vec![],
        defaultness: None,
        unsafety: None,
        impl_token: Default::default(),
        generics: Default::default(),
        trait_: None,
        self_ty: Box::new(Type::Path(TypePath {
            qself: None,
            path: Path::from(input.ident),
        })),
        brace_token: Default::default(),
        items,
    }
    .with_generics(generics)
    .dump()
    .into()
}

fn expand(input: DataEnum) -> Vec<ImplItem> {
    let mut items = vec![];

    for v in &input.variants {
        let name = v.ident.to_string().to_snake_case();
        {
            let name_of_is = Ident::new(&format!("is_{}", name), v.ident.span());
            let docs_of_is = format!(
                "Returns `true` if `self` is of variant [`{variant}`].\n\n\

                [`{variant}`]: #variant.{variant}",
                variant = v.ident,
            );

            items.extend(
                Quote::new_call_site()
                    .quote_with(smart_quote!(
                        Vars {
                            docs_of_is,
                            name_of_is,
                            Variant: &v.ident
                        },
                        {
                            impl Type {
                                #[doc = docs_of_is]
                                #[inline]
                                pub fn name_of_is(&self) -> bool {
                                    match *self {
                                        Self::Variant { .. } => true,
                                        _ => false,
                                    }
                                }
                            }
                        }
                    ))
                    .parse::<ItemImpl>()
                    .items,
            );
        }

        {
            let name_of_expect = Ident::new(&format!("expect_{}", name), v.ident.span());
            let name_of_take = Ident::new(&name, v.ident.span());

            let docs_of_expect = format!(
                "Unwraps the value, yielding the content of [`{variant}`].\n\n\

                # Panics\n\n\

                Panics if the value is not [`{variant}`], with a panic message including \
                the content of `self`.\n\n\

                [`{variant}`]: #variant.{variant}",
                variant = v.ident,
            );
            let docs_of_take = format!(
                "Returns `Some` if `self` is of variant [`{variant}`], and `None` otherwise.\n\n\

                [`{variant}`]: #variant.{variant}",
                variant = v.ident,
            );

            match &v.fields {
                Fields::Unnamed(fields) => {
                    let ty = if fields.unnamed.len() == 1 {
                        let mut ty = fields.unnamed.iter();
                        let name = ty.next().expect("names len is 1").ty.clone();
                        Quote::new_call_site()
                            .quote_with(smart_quote!(Vars { name }, { name }))
                            .parse()
                    } else {
                        Type::Tuple(TypeTuple {
                            paren_token: Default::default(),
                            elems: fields
                                .unnamed
                                .clone()
                                .into_pairs()
                                .map(|pair| {
                                    let handle = |f: Field| {
                                        //
                                        f.ty
                                    };
                                    match pair {
                                        Pair::Punctuated(v, p) => Pair::Punctuated(handle(v), p),
                                        Pair::End(v) => Pair::End(handle(v)),
                                    }
                                })
                                .collect(),
                        })
                    };

                    let fields: Punctuated<Ident, Token![,]> = fields
                        .unnamed
                        .clone()
                        .into_pairs()
                        .enumerate()
                        .map(|(i, pair)| {
                            let handle = |f: Field| {
                                //
                                Ident::new(&format!("v{}", i), f.span())
                            };
                            match pair {
                                Pair::Punctuated(v, p) => Pair::Punctuated(handle(v), p),
                                Pair::End(v) => Pair::End(handle(v)),
                            }
                        })
                        .collect();

                    items.extend(
                        Quote::new_call_site()
                            .quote_with(smart_quote!(
                                Vars {
                                    docs_of_expect,
                                    docs_of_take,
                                    name_of_expect,
                                    name_of_take,
                                    Variant: &v.ident,
                                    Type: ty,
                                    v: &fields,
                                },
                                {
                                    impl Type {
                                        #[doc = docs_of_expect]
                                        #[inline]
                                        pub fn name_of_expect(self) -> Type
                                        where
                                            Self: ::std::fmt::Debug,
                                        {
                                            match self {
                                                Self::Variant(v) => (v),
                                                _ => panic!("called expect on {:?}", self),
                                            }
                                        }

                                        #[doc = docs_of_take]
                                        #[inline]
                                        pub fn name_of_take(self) -> Option<Type> {
                                            match self {
                                                Self::Variant(v) => Some((v)),
                                                _ => None,
                                            }
                                        }
                                    }
                                }
                            ))
                            .parse::<ItemImpl>()
                            .items,
                    );
                }
                _ => {}
            }
        }
    }

    items
}

/// Extension trait for `ItemImpl` (impl block).
trait ItemImplExt {
    /// Instead of
    ///
    /// ```rust,ignore
    /// let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    ///
    /// let item: Item = Quote::new(def_site::<Span>())
    ///     .quote_with(smart_quote!(
    /// Vars {
    /// Type: type_name,
    /// impl_generics,
    /// ty_generics,
    /// where_clause,
    /// },
    /// {
    /// impl impl_generics ::swc_common::AstNode for Type ty_generics
    /// where_clause {}
    /// }
    /// )).parse();
    /// ```
    ///
    /// You can use this like
    ///
    /// ```rust,ignore
    // let item = Quote::new(def_site::<Span>())
    ///     .quote_with(smart_quote!(Vars { Type: type_name }, {
    ///         impl ::swc_common::AstNode for Type {}
    ///     }))
    ///     .parse::<ItemImpl>()
    ///     .with_generics(input.generics);
    /// ```
    fn with_generics(self, generics: Generics) -> Self;
}

impl ItemImplExt for ItemImpl {
    fn with_generics(mut self, mut generics: Generics) -> Self {
        // TODO: Check conflicting name

        let need_new_punct = !generics.params.empty_or_trailing();
        if need_new_punct {
            generics
                .params
                .push_punct(syn::token::Comma(Span::call_site()));
        }

        // Respan
        if let Some(t) = generics.lt_token {
            self.generics.lt_token = Some(t)
        }
        if let Some(t) = generics.gt_token {
            self.generics.gt_token = Some(t)
        }

        let ty = self.self_ty;

        // Handle generics defined on struct, enum, or union.
        let mut item: ItemImpl = {
            let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
            let item = if let Some((ref polarity, ref path, ref for_token)) = self.trait_ {
                quote! {
                    impl #impl_generics #polarity #path #for_token #ty #ty_generics #where_clause {}
                }
            } else {
                quote! {
                    impl #impl_generics #ty #ty_generics #where_clause {}

                }
            };
            parse(item.dump().into())
                .unwrap_or_else(|err| panic!("with_generics failed: {}\n{}", err, item.dump()))
        };

        // Handle generics added by proc-macro.
        item.generics
            .params
            .extend(self.generics.params.into_pairs());
        match self.generics.where_clause {
            Some(WhereClause {
                ref mut predicates, ..
            }) => {
                println!("FOO");
                predicates.extend(
                    generics
                        .where_clause
                        .into_iter()
                        .flat_map(|wc| wc.predicates.into_pairs()),
                )
            }
            ref mut opt @ None => *opt = generics.where_clause,
        }

        ItemImpl {
            attrs: self.attrs,
            defaultness: self.defaultness,
            unsafety: self.unsafety,
            impl_token: self.impl_token,
            brace_token: self.brace_token,
            items: self.items,
            ..item
        }
    }
}
