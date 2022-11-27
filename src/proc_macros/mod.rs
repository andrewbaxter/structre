use std::collections::{HashMap, HashSet};

use litrs::StringLit;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use regex_syntax::hir::Hir;
use syn::{self, parse_macro_input, DataStruct, Ident, Type};

struct Data<'a> {
    captures: Vec<&'a Hir>,
    named_captures: HashMap<String, usize>,
}

impl<'a> Data<'a> {
    fn walk_re(&mut self, r: &'a Hir) {
        match r.kind() {
            regex_syntax::hir::HirKind::Empty => (),
            regex_syntax::hir::HirKind::Literal(_) => (),
            regex_syntax::hir::HirKind::Class(_) => (),
            regex_syntax::hir::HirKind::Anchor(_) => (),
            regex_syntax::hir::HirKind::WordBoundary(_) => (),
            regex_syntax::hir::HirKind::Repetition(e) => self.walk_re(&e.hir),
            regex_syntax::hir::HirKind::Group(g) => match &g.kind {
                regex_syntax::hir::GroupKind::CaptureIndex(i) => {
                    let i = *i as usize - 1;
                    if self.captures.len() != i {
                        panic!("ASSERTION cap len {} but index {}", self.captures.len(), i)
                    }
                    self.captures.push(&g.hir);
                }
                regex_syntax::hir::GroupKind::CaptureName { name, index } => {
                    let index = *index as usize - 1;
                    if self.captures.len() != index {
                        panic!(
                            "ASSERTION cap len {} but index {}",
                            self.captures.len(),
                            index
                        )
                    }
                    self.captures.push(&g.hir);
                    self.named_captures.insert(name.clone(), index);
                }
                regex_syntax::hir::GroupKind::NonCapturing => self.walk_re(&g.hir),
            },
            regex_syntax::hir::HirKind::Concat(c) => {
                for c in c {
                    self.walk_re(c);
                }
            }
            regex_syntax::hir::HirKind::Alternation(_) => (),
        }
    }

    fn gen_struct_tuple(
        &self,
        i: &mut usize,
        fields: &mut dyn Iterator<Item = &Type>,
    ) -> Vec<TokenStream> {
        let mut out = vec![];
        for ty in fields {
            match ty {
                Type::Tuple(t) => {
                    let child = self.gen_struct_tuple(i, &mut t.elems.iter());
                    out.push(quote!((#(#child),*)));
                }
                ty => {
                    let err_lit = format!("Failed to parse field {}", i);
                    *i += 1;
                    out.push(
                        quote!(#ty::from_str(caps_.get(#i).map(|m| m.as_str()).unwrap_or("")).context(#err_lit)?),
                    );
                }
            }
        }
        out
    }

    fn gen_struct(&self, ident: &Ident, d: &DataStruct) -> TokenStream {
        match &d.fields {
            syn::Fields::Named(n) => {
                let mut field_tokens = vec![];
                let mut keys = self
                    .named_captures
                    .keys()
                    .into_iter()
                    .map(&String::to_string)
                    .collect::<HashSet<String>>();
                for field in &n.named {
                    let name = field.ident.as_ref().unwrap();
                    let i = match self.named_captures.get(&name.to_string()) {
                        Some(c) => *c,
                        None => panic!("No named capture for field {}", name),
                    };
                    keys.remove(&name.to_string());
                    let ty = &field.ty;
                    let err_lit = format!("Failed to parse field {}", name);
                    let i = i + 1;
                    field_tokens.push(
                        quote!(#name: #ty::from_str(caps_.get(#i).map(|m| m.as_str()).unwrap_or("")).context(#err_lit)?),
                    );
                }
                if !keys.is_empty() {
                    panic!("No fields for named captures: {:?}", keys);
                }
                if self.captures.len() > self.named_captures.len() {
                    panic!("This is a struct with named fields but there are some unused unnamed captures");
                }
                quote!(Ok(#ident {
                    #(#field_tokens),*
                }))
            }
            syn::Fields::Unnamed(u) => {
                if !self.named_captures.is_empty() {
                    panic!(
                        "Tuples must have only unnamed captures, but named captures are present"
                    );
                }
                let mut i = 0usize;
                let field_tokens =
                    self.gen_struct_tuple(&mut i, &mut u.unnamed.iter().map(|e| &e.ty));
                if i != self.captures.len() {
                    panic!(
                        "Struct has {} fields but only {} captures",
                        u.unnamed.len(),
                        self.captures.len()
                    );
                }
                quote!(Ok(#ident (
                    #(#field_tokens),*
                )))
            }
            syn::Fields::Unit => {
                if !self.captures.is_empty() {
                    panic!("This is an empty struct but regex has captures")
                }
                quote!(Ok(#ident ()))
            }
        }
    }
}

fn gen_value(regex_raw: &str, ast: &syn::DeriveInput) -> TokenStream {
    let regex = regex_syntax::Parser::new().parse(regex_raw).unwrap();
    let mut data = Data {
        captures: Default::default(),
        named_captures: Default::default(),
    };
    data.walk_re(&regex);
    match &ast.data {
        syn::Data::Struct(d) => data.gen_struct(&ast.ident, d),
        syn::Data::Enum(_) => panic!("enum not supported yet"),
        syn::Data::Union(_) => panic!("union not supported"),
    }
}

fn gen_impls(regex_raw: &str, ast: syn::DeriveInput) -> TokenStream {
    let value = gen_value(regex_raw, &ast);
    let name = &ast.ident;
    let vis = &ast.vis;
    let name_parser = format_ident!("{}FromRegex", name);
    let mut out = vec![ast.to_token_stream()];
    #[cfg(feature = "unicode")]
    out.push(quote! {
        #vis struct #name_parser(structre::UnicodeRegex);

        impl #name_parser {
            #vis fn new() -> Self {
                Self(structre::UnicodeRegex::new(#regex_raw).unwrap())
            }

            #vis fn parse(&self, input: &str) -> structre::Result<#name> {
                #[allow(unused_imports)]
                use std::str::FromStr;
                #[allow(unused_imports)]
                use structre::Context;
                let caps_ = self.0.captures(input).ok_or_else(|| structre::Error::msg("No match"))?;
                #value
            }
        }
    });
    TokenStream::from_iter(out)
}

#[proc_macro_attribute]
pub fn structre(
    args: proc_macro::TokenStream,
    body: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut args = proc_macro2::TokenStream::from(args).into_iter();
    let regex_raw = match args.next().unwrap() {
        proc_macro2::TokenTree::Literal(l) => match StringLit::try_from(&l) {
            Ok(l) => l.value().to_string(),
            Err(_) => panic!("First arg must be literal string, got {}", l),
        },
        t => panic!("First arg must be literal, got {}", t),
    };
    if args.next().is_some() {
        panic!("Only takes one arg, got more than one");
    }
    let ast = parse_macro_input!(body as syn::DeriveInput);
    gen_impls(&regex_raw, ast).into()
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use proc_macro2::TokenStream;

    use crate::gen_value;
    use quote::quote;

    #[test]
    fn newtype_string() {
        assert_eq!(
            gen_value(
                "(a)",
                &syn::parse2(TokenStream::from_str("struct Parsed(String);").unwrap()).unwrap(),
            )
            .to_string(),
            quote!(Ok(Parsed(
                String::from_str(caps_.get(1usize).map(|m| m.as_str()).unwrap_or(""))
                    .context("Failed to parse field 0")?
            )))
            .to_string()
        );
    }

    #[test]
    fn tuple() {
        assert_eq!(
            gen_value(
                "(a)(b)",
                &syn::parse2(TokenStream::from_str("struct Parsed((String, u32));").unwrap())
                    .unwrap(),
            )
            .to_string(),
            quote!(Ok(Parsed((
                String::from_str(caps_.get(1usize).map(|m| m.as_str()).unwrap_or(""))
                    .context("Failed to parse field 0")?,
                u32::from_str(caps_.get(2usize).map(|m| m.as_str()).unwrap_or(""))
                    .context("Failed to parse field 1")?
            ))))
            .to_string()
        );
    }

    #[test]
    fn struct_() {
        assert_eq!(
            gen_value(
                "(?P<a>a)(?P<b>b)",
                &syn::parse2(TokenStream::from_str("struct Parsed { b: u32, a: String }").unwrap())
                    .unwrap(),
            )
            .to_string(),
            quote!(Ok(Parsed {
                b: u32::from_str(caps_.get(2usize).map(|m| m.as_str()).unwrap_or(""))
                    .context("Failed to parse field b")?,
                a: String::from_str(caps_.get(1usize).map(|m| m.as_str()).unwrap_or(""))
                    .context("Failed to parse field a")?
            }))
            .to_string()
        );
    }
}
