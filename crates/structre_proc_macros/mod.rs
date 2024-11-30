use {
    flowcontrol::{
        shed,
        superif,
    },
    litrs::StringLit,
    proc_macro2::{
        Span,
        TokenStream,
    },
    quote::{
        quote,
        ToTokens,
    },
    regex_syntax::ast::{
        Ast as ReAst,
        GroupKind,
    },
    std::{
        collections::HashMap,
    },
    syn::{
        self,
        parse_macro_input,
        spanned::Spanned,
        DataEnum,
        DataStruct,
        Field,
        Ident,
        Type,
    },
};

struct ReUnnamedCapture {
    optional: bool,
}

struct ReNamedCapture {
    index: usize,
    optional: bool,
}

#[derive(Default)]
struct ReFlatData {
    unnamed_captures: HashMap<usize, ReUnnamedCapture>,
    named_captures: HashMap<String, ReNamedCapture>,
}

fn flatten_re(out: &mut ReFlatData, re: &ReAst, optional_context: bool) {
    match re {
        ReAst::Flags(_) => (),
        ReAst::Dot(_) => (),
        ReAst::Assertion(_) => (),
        ReAst::Empty(_) => (),
        ReAst::Literal(_) => (),
        ReAst::ClassUnicode(_) => (),
        ReAst::ClassPerl(_) => (),
        ReAst::ClassBracketed(_) => (),
        ReAst::Repetition(e) => flatten_re(out, &e.ast, optional_context || match &e.op.kind {
            regex_syntax::ast::RepetitionKind::ZeroOrOne => true,
            regex_syntax::ast::RepetitionKind::ZeroOrMore => true,
            regex_syntax::ast::RepetitionKind::OneOrMore => false,
            regex_syntax::ast::RepetitionKind::Range(r) => match r {
                regex_syntax::ast::RepetitionRange::Exactly(x) => *x == 0,
                regex_syntax::ast::RepetitionRange::AtLeast(x) => *x == 0,
                regex_syntax::ast::RepetitionRange::Bounded(x, _) => *x == 0,
            },
        }),
        ReAst::Group(g) => match &g.kind {
            regex_syntax::ast::GroupKind::CaptureIndex(index) => {
                out.unnamed_captures.insert(*index as usize, ReUnnamedCapture { optional: optional_context });
            },
            regex_syntax::ast::GroupKind::CaptureName { name, .. } => {
                out.named_captures.insert(name.name.clone(), ReNamedCapture {
                    index: name.index as usize,
                    optional: optional_context,
                });
            },
            regex_syntax::ast::GroupKind::NonCapturing(_) => flatten_re(out, &g.ast, optional_context),
        },
        ReAst::Concat(c) => {
            for c in &c.asts {
                flatten_re(out, c, optional_context);
            }
        },
        ReAst::Alternation(a) => {
            for child in &a.asts {
                flatten_re(out, child, true);
            }
        },
    }
}

// Someone please save me
#[derive(Clone)]
enum SimpleSimpleTypeType {
    TryFrom,
    FromStr,
}

#[derive(Clone)]
struct SimpleSimpleType {
    span: Span,
    type_: TokenStream,
    typetype: SimpleSimpleTypeType,
}

#[derive(Clone)]
enum SimpleType {
    Simple(SimpleSimpleType),
    Option(SimpleSimpleType),
    Tuple(Vec<SimpleType>),
}

fn simple_simple_type(ty: &Type) -> SimpleSimpleType {
    let ty_tokens = ty.to_token_stream();
    let typetype;
    match ty_tokens.to_string().as_str() {
        "u8" |
        "u16" |
        "u32" |
        "u64" |
        "u128" |
        "usize" |
        "i8" |
        "i16" |
        "i32" |
        "i64" |
        "i128" |
        "isize" |
        "f8" |
        "f16" |
        "f32" |
        "f64" |
        "f128" |
        "bool" |
        "char" |
        "std::net::IpAddr" |
        "net::IpAddr" |
        "IpAddr" |
        "std::net::Ipv4Addr" |
        "net::Ipv4Addr" |
        "Ipv4Addr" |
        "std::net::Ipv6Addr" |
        "net::Ipv6Addr" |
        "Ipv6Addr" |
        "std::net::SocketAddr" |
        "net::SocketAddr" |
        "SocketAddr" |
        "std::net::SocketAddrV4" |
        "net::SocketAddrV4" |
        "SocketAddrV4" |
        "std::net::SocketAddrV6" |
        "net::SocketAddrV6" |
        "SocketAddrV6" |
        "std::ffi::OsString" |
        "ffi::OsString" |
        "OsString" |
        "std::num::NonZero<u8>" |
        "std::num::NonZero<u16>" |
        "std::num::NonZero<u32>" |
        "std::num::NonZero<u64>" |
        "std::num::NonZero<u128>" |
        "std::num::NonZero<usize>" |
        "std::num::NonZero<i8>" |
        "std::num::NonZero<i16>" |
        "std::num::NonZero<i32>" |
        "std::num::NonZero<i64>" |
        "std::num::NonZero<i128>" |
        "std::num::NonZero<isize>" |
        "num::NonZero<u8>" |
        "num::NonZero<u16>" |
        "num::NonZero<u32>" |
        "num::NonZero<u64>" |
        "num::NonZero<u128>" |
        "num::NonZero<usize>" |
        "num::NonZero<i8>" |
        "num::NonZero<i16>" |
        "num::NonZero<i32>" |
        "num::NonZero<i64>" |
        "num::NonZero<i128>" |
        "num::NonZero<isize>" |
        "NonZero<u8>" |
        "NonZero<u16>" |
        "NonZero<u32>" |
        "NonZero<u64>" |
        "NonZero<u128>" |
        "NonZero<usize>" |
        "NonZero<i8>" |
        "NonZero<i16>" |
        "NonZero<i32>" |
        "NonZero<i64>" |
        "NonZero<i128>" |
        "NonZero<isize>" => {
            typetype = SimpleSimpleTypeType::FromStr;
        },
        _ => {
            typetype = SimpleSimpleTypeType::TryFrom;
        },
    }
    return SimpleSimpleType {
        span: ty.span(),
        type_: ty_tokens,
        typetype: typetype,
    };
}

/// Reduce a type to a handleable, semantically meaningful type (i.e. 1-tuples are
/// unwrapped, parens unwrapped, etc).
fn simple_type(ty: &Type) -> Result<SimpleType, syn::Error> {
    match ty {
        Type::Path(p) => {
            let opt = shed!{
                if p.qself.is_some() {
                    break None;
                }
                if p.path.segments.len() != 1 {
                    break None;
                };
                let seg = p.path.segments.get(0).unwrap();
                if seg.ident != "Option" {
                    break None;
                };
                let syn::PathArguments::AngleBracketed(args) = &seg.arguments else {
                    unreachable!();
                };
                let syn::GenericArgument::Type(arg_type) = args.args.get(0).unwrap() else {
                    unreachable!();
                };
                break Some(arg_type);
            };
            if let Some(opt_inner) = opt {
                return Ok(SimpleType::Option(simple_simple_type(opt_inner)));
            } else {
                return Ok(SimpleType::Simple(simple_simple_type(ty)));
            }
        },
        Type::Paren(t) => {
            return Ok(simple_type(&t.elem)?);
        },
        Type::Group(t) => {
            return Ok(simple_type(&t.elem)?);
        },
        Type::Tuple(t) => {
            let mut children = vec![];
            for e in &t.elems {
                children.push(simple_type(e)?);
            }
            return Ok(SimpleType::Tuple(children));
        },
        Type::Reference(_) => {
            return Ok(SimpleType::Simple(simple_simple_type(ty)));
        },
        _ => { },
    }
    return Err(syn::Error::new(ty.span(), "This type does not support parsing from regex"));
}

fn gen_from_capture(str_lifetime: &TokenStream, path: &str, t: &SimpleSimpleType, cap: TokenStream) -> TokenStream {
    let p = &t.type_;
    match &t.typetype {
        SimpleSimpleTypeType::TryFrom => {
            return quote!(
                < #p as std:: convert:: TryFrom <& #str_lifetime str >>:: try_from(
                    #cap
                ).map_err(| e | structre:: Error:: Field {
                    field: #path,
                    error: e.to_string()
                }) ?
            );
        },
        SimpleSimpleTypeType::FromStr => {
            return quote!(< #p as std:: str:: FromStr >:: from_str(#cap).map_err(| e | structre:: Error:: Field {
                field: #path,
                error: e.to_string()
            }) ?);
        },
    }
}

fn gen_named_fields(
    str_lifetime: &TokenStream,
    re_flat: &mut ReFlatData,
    next_unnamed_index: &mut usize,
    path: &str,
    fields: &mut dyn Iterator<Item = &Field>,
) -> Result<TokenStream, syn::Error> {
    let mut field_tokens = vec![];
    for field in fields {
        let name = field.ident.as_ref().unwrap();
        let name_str = name.to_string();
        let path = format!("{}.{}", path, name_str);
        match simple_type(&field.ty)? {
            SimpleType::Option(p) => {
                let Some(cap) = re_flat.named_captures.remove(&name.to_string()) else {
                    return Err(
                        syn::Error::new(p.span, format!("No named capture `{}` for field `{}`", name_str, path)),
                    );
                };
                let cap_index = cap.index;
                if !cap.optional {
                    return Err(
                        syn::Error::new(p.span, "Field is optional but corresponding capture always matches"),
                    );
                }
                let from_cap = gen_from_capture(str_lifetime, &path, &p, quote!(m.as_str()));
                field_tokens.push(quote!(#name: match captures.get(#cap_index) {
                    Some(m) => Some(#from_cap),
                    None => None,
                }));
            },
            SimpleType::Tuple(p) => {
                let parse_tuple =
                    gen_unnamed_fields(
                        str_lifetime,
                        re_flat,
                        next_unnamed_index,
                        &path,
                        &mut p.iter().map(|x| x).cloned(),
                    )?;
                field_tokens.push(quote!(#name:(#parse_tuple)));
            },
            SimpleType::Simple(p) => {
                let Some(cap) = re_flat.named_captures.remove(&name.to_string()) else {
                    return Err(
                        syn::Error::new(field.span(), format!("No named capture `{}` for field `{}`", name_str, path)),
                    );
                };
                let cap_index = cap.index;
                if cap.optional {
                    return Err(
                        syn::Error::new(
                            field.span(),
                            "Field is not optional but corresponding capture optionally matches",
                        ),
                    );
                }
                let from_cap =
                    gen_from_capture(str_lifetime, &path, &p, quote!(captures.get(#cap_index).unwrap().as_str()));
                field_tokens.push(quote!(#name: #from_cap));
            },
        }
    }
    return Ok(quote!(#(#field_tokens,) *));
}

fn gen_unnamed_fields(
    str_lifetime: &TokenStream,
    re_flat: &mut ReFlatData,
    next_unnamed_index: &mut usize,
    path: &str,
    fields: &mut dyn Iterator<Item = SimpleType>,
) -> Result<TokenStream, syn::Error> {
    let mut out = vec![];
    for (field_index, ty) in fields.enumerate() {
        let path = format!("{}.{}", path, field_index);
        match ty {
            SimpleType::Option(p) => {
                let cap_index = *next_unnamed_index;
                *next_unnamed_index += 1;
                let Some(cap) = re_flat.unnamed_captures.remove(&cap_index) else {
                    return Err(syn::Error::new(p.span, format!("Missing unnamed capture for `{}`", path)));
                };
                if !cap.optional {
                    return Err(
                        syn::Error::new(p.span, "Field is optional but corresponding capture always matches"),
                    );
                }
                let from_cap = gen_from_capture(str_lifetime, &path, &p, quote!(m.as_str()));
                out.push(quote!(match captures.get(#cap_index) {
                    Some(m) => {
                        Some(#from_cap)
                    },
                    None => None,
                }));
            },
            SimpleType::Tuple(p) => {
                let child =
                    gen_unnamed_fields(str_lifetime, re_flat, next_unnamed_index, &path, &mut p.iter().cloned())?;
                out.push(quote!((#child)));
            },
            SimpleType::Simple(p) => {
                let cap_index = *next_unnamed_index;
                *next_unnamed_index += 1;
                let Some(cap) = re_flat.unnamed_captures.remove(&cap_index) else {
                    return Err(syn::Error::new(p.span, format!("Missing unnamed capture for `{}`", path)));
                };
                if cap.optional {
                    return Err(
                        syn::Error::new(p.span, "Field is not optional but corresponding capture optionally matches"),
                    );
                }
                out.push(
                    gen_from_capture(str_lifetime, &path, &p, quote!(captures.get(#cap_index).unwrap().as_str())),
                );
            },
        }
    }
    return Ok(quote!(#(#out,) *));
}

fn gen_struct(
    str_lifetime: &TokenStream,
    re: &ReAst,
    ident: &Ident,
    struct_: &DataStruct,
) -> Result<TokenStream, syn::Error> {
    let mut re_flat = ReFlatData::default();
    flatten_re(&mut re_flat, re, false);
    let mut next_unnamed_index = 1;
    let path = ident.to_string();
    let out = match &struct_.fields {
        syn::Fields::Named(n) => {
            let fields =
                gen_named_fields(str_lifetime, &mut re_flat, &mut next_unnamed_index, &path, &mut n.named.iter())?;
            quote!({
                #fields
            })
        },
        syn::Fields::Unnamed(u) => {
            if !re_flat.named_captures.is_empty() {
                return Err(
                    syn::Error::new(
                        struct_.struct_token.span,
                        "Tuples must have only unnamed captures, but named captures are present in the regex",
                    ),
                );
            }
            let field_tokens = gen_unnamed_fields(str_lifetime, &mut re_flat, &mut next_unnamed_index, &path, &mut {
                let mut out = vec![];
                for n in &u.unnamed {
                    out.push(simple_type(&n.ty)?);
                }
                out
            }.into_iter())?;
            quote!((#field_tokens))
        },
        syn::Fields::Unit => {
            quote!()
        },
    };
    if !re_flat.named_captures.is_empty() {
        return Err(
            syn::Error::new(
                struct_.struct_token.span,
                format!("Named captures never used: {:?}", re_flat.named_captures.keys()),
            ),
        );
    }
    if !re_flat.unnamed_captures.is_empty() {
        return Err(
            syn::Error::new(
                struct_.struct_token.span,
                format!("Unnamed captures never used: {} remain", re_flat.unnamed_captures.len()),
            ),
        );
    }
    return Ok(quote!(return Ok(#ident #out);));
}

fn gen_enum(
    str_lifetime: &TokenStream,
    re: &ReAst,
    ident: &Ident,
    enum_: &DataEnum,
) -> Result<TokenStream, syn::Error> {
    // Find the topmost re alternatives (variants)
    fn find_re_variants<'a>(re: &'a ReAst) -> Result<Option<&'a Vec<ReAst>>, String> {
        match re {
            ReAst::Repetition(x) => {
                if find_re_variants(&x.ast)?.is_some() {
                    return Err(format!("Reptition with regex alternates isn't supported: `{}`", re));
                } else {
                    return Ok(None);
                }
            },
            ReAst::Group(x) => {
                match x.kind {
                    GroupKind::CaptureIndex(_) | GroupKind::CaptureName { .. } => {
                        return Err(
                            format!(
                                "All capturing groups must occur within alternates in an enum, but found a capturing group above an enum: `{}`",
                                re
                            ),
                        );
                    },
                    GroupKind::NonCapturing(_) => {
                        return find_re_variants(&x.ast);
                    },
                }
            },
            ReAst::Concat(x) => {
                let mut found = None;
                for x in &x.asts {
                    if let Some(new_found) = find_re_variants(x)? {
                        if found.is_some() {
                            return Err(
                                format!(
                                    "Enums support only a single alternation in the corresponding regex but found multiple parallel alternates in regex: second = `{}`",
                                    re
                                ),
                            );
                        } else {
                            found = Some(new_found);
                        }
                    }
                }
                return Ok(found);
            },
            ReAst::Alternation(x) => {
                return Ok(Some(&x.asts));
            },
            ReAst::Empty(_) => return Ok(None),
            ReAst::Flags(_) => return Ok(None),
            ReAst::Literal(_) => return Ok(None),
            ReAst::Dot(_) => return Ok(None),
            ReAst::Assertion(_) => return Ok(None),
            ReAst::ClassUnicode(_) => Ok(None),
            ReAst::ClassPerl(_) => Ok(None),
            ReAst::ClassBracketed(_) => Ok(None),
        }
    }

    let Some(re_variants) = find_re_variants(re).map_err(|e| syn::Error::new(enum_.enum_token.span, e))? else {
        return Err(syn::Error::new(enum_.enum_token.span, "Regex doesn't contain any alternates (|)"));
    };
    if re_variants.len() != enum_.variants.len() {
        return Err(
            syn::Error::new(
                enum_.enum_token.span,
                format!(
                    "Regex alternate count and enum variant counts don't match: found {} regex alternates but {} enum variants",
                    re_variants.len(),
                    enum_.variants.len()
                ),
            ),
        );
    }

    // Generate each variant code.
    let mut code_variants = vec![];
    for re_variant in re_variants {
        let mut re_flat = ReFlatData::default();
        flatten_re(&mut re_flat, re_variant, false);

        // Identify a key field (a named capture that will always be present) in each
        // variant to identify it.
        let (key, key_index) = shed!{
            'found_key _;
            for (k, v) in &re_flat.named_captures {
                if !v.optional {
                    break 'found_key (k.clone(), v.index);
                }
            }
            return Err(
                syn::Error::new(
                    enum_.enum_token.span,
                    format!(
                        "Regex alternatives must have at least one non-optional named capture to use as a key when parsing; this alternative has none: {}",
                        re_variant
                    ),
                ),
            );
        };

        // Find the corresponding enum variant and generate the field parser code
        let mut next_unnamed_index = 1;
        let parse_variant;
        shed!{
            'matched_enum_variant _;
            for enum_variant in &enum_.variants {
                let variant_ident = &enum_variant.ident;
                match &enum_variant.fields {
                    syn::Fields::Named(fields) => {
                        superif!({
                            for f in &fields.named {
                                if f.ident.as_ref().unwrap().to_string() == key && 
                                    // A tuple is actually multiple unnamed fields - the parent field name is unused
                                    !matches!(f.ty, Type::Tuple(_)) {
                                    break 'matched_named;
                                }
                            }
                        } 'matched_named {
                            let parse_fields =
                                gen_named_fields(
                                    str_lifetime,
                                    &mut re_flat,
                                    &mut next_unnamed_index,
                                    &format!("{}::{}", ident, variant_ident),
                                    &mut fields.named.iter(),
                                )?;
                            parse_variant = quote!(#ident:: #variant_ident {
                                #parse_fields
                            });
                            break 'matched_enum_variant;
                        });
                    },
                    syn::Fields::Unnamed(fields) => {
                        superif!({
                            if fields.unnamed.len() == 1 {
                                let root = simple_type(&fields.unnamed.get(0).unwrap().ty)?;
                                let mut at = &root;
                                loop {
                                    match at {
                                        SimpleType::Option(_) => break,
                                        SimpleType::Tuple(t) => {
                                            if t.len() == 1 {
                                                at = t.get(0).unwrap();
                                            } else {
                                                break;
                                            }
                                        },
                                        SimpleType::Simple(_) => {
                                            if key == variant_ident.to_string() {
                                                break 'match_unnamed;
                                            } else {
                                                break;
                                            }
                                        },
                                    }
                                }
                            } else {
                                return Err(
                                    syn::Error::new(
                                        enum_variant.span(),
                                        "Multi-field unnamed variants have no fields that can be used to discriminate when parsing",
                                    ),
                                );
                            }
                        } 'match_unnamed {
                            let cap = re_flat.named_captures.remove(&key).unwrap();
                            let cap_index = cap.index;
                            let field_ty = &fields.unnamed.get(0).as_ref().unwrap().ty;
                            let path = format!("{}::{}", ident, variant_ident);
                            parse_variant =
                                quote!(
                                    #ident:: #variant_ident(
                                        < #field_ty as std:: convert:: TryFrom <& #str_lifetime str >>:: try_from(
                                            captures.get(#cap_index).unwrap().as_str()
                                        ).map_err(| e | structre:: Error:: Field {
                                            field: #path,
                                            error: e.to_string()
                                        }) ?
                                    )
                                );
                            break 'matched_enum_variant;
                        });
                    },
                    syn::Fields::Unit => return Err(
                        syn::Error::new(
                            enum_variant.span(),
                            "Unit variants have no fields that can be used to discriminate when parsing",
                        ),
                    ),
                };
            }
            return Err(
                syn::Error::new(
                    enum_.enum_token.span,
                    format!("No enum variant found matching key field [{}] in regex alternative: {}", key, re_variant),
                ),
            );
        };

        // Assemble code
        if !re_flat.named_captures.is_empty() {
            return Err(
                syn::Error::new(
                    enum_.enum_token.span,
                    format!("Named captures never used: {:?}", re_flat.named_captures.keys()),
                ),
            );
        }
        if !re_flat.unnamed_captures.is_empty() {
            return Err(
                syn::Error::new(
                    enum_.enum_token.span,
                    format!("Unnamed captures never used: {} remain", re_flat.unnamed_captures.len()),
                ),
            );
        }
        code_variants.push(quote!{
            if captures.get(#key_index).is_some() {
                return Ok(#parse_variant);
            }
        });
    }

    // Generate
    return Ok(quote!{
        #(#code_variants) * unreachable !();
    });
}

fn parse_re(regex_raw: &str) -> Result<ReAst, regex_syntax::ast::Error> {
    return regex_syntax::ast::parse::Parser::new().parse(regex_raw);
}

fn gen_root(regex_span: Span, regex_raw: &str, ast: syn::DeriveInput) -> Result<TokenStream, syn::Error> {
    let re = parse_re(regex_raw).map_err(|e| syn::Error::new(regex_span, e.to_string()))?;
    let type_generics;
    if ast.generics.lt_token.is_some() {
        type_generics = ast.generics.to_token_stream();
    } else {
        type_generics = quote!();
    };
    let no_lifetime;
    let str_lifetime;
    let impl_generics;
    superif!({
        if !ast.generics.lt_token.is_some() {
            break 'no_lifetime;
        };
        let Some(l) = ast.generics.lifetimes().next() else {
            break 'no_lifetime;
        };
        no_lifetime = false;
        str_lifetime = l.to_token_stream();
        impl_generics = ast.generics.to_token_stream();
    } 'no_lifetime {
        no_lifetime = true;
        str_lifetime = quote!('structre);
        impl_generics = quote!(< #str_lifetime >);
    });
    let root;
    match &ast.data {
        syn::Data::Struct(d) => {
            root = gen_struct(&str_lifetime, &re, &ast.ident, d)?;
        },
        syn::Data::Enum(d) => {
            root = gen_enum(&str_lifetime, &re, &ast.ident, d)?;
        },
        syn::Data::Union(_) => return Err(syn::Error::new(ast.span(), "Union not supported")),
    };
    let name = &ast.ident;
    let mut out = vec![ast.to_token_stream()];
    out.push(quote!{
        impl #impl_generics std:: convert:: TryFrom <& #str_lifetime str > for #name #type_generics {
            type Error = structre::Error;
            fn try_from(input:& #str_lifetime str) -> Result < Self,
            Self:: Error > {
                static RE: std::sync::OnceLock<structre::regex::Regex> = std::sync::OnceLock::new();
                let captures = RE.get_or_init(
                    || structre:: regex:: Regex:: new(#regex_raw).unwrap()
                ).captures(input).ok_or(structre::Error::NoMatch) ?;
                #root
            }
        }
    });
    if no_lifetime {
        out.push(quote!{
            impl #impl_generics std:: str:: FromStr for #name #type_generics {
                type Err = structre::Error;

                fn from_str(input: &str) -> Result<Self, Self::Err> {
                    return Self::try_from(input);
                }
            }
        })
    }
    return Ok(TokenStream::from_iter(out));
}

#[proc_macro_attribute]
pub fn structre(args: proc_macro::TokenStream, body: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // (Outside body because this macro actually generates compile_error `return`s and
    // needs to be in a function returning `proc_macro::TokenStream` directly)
    let ast = parse_macro_input!(body as syn::DeriveInput);
    match move || -> Result<TokenStream, syn::Error> {
        let mut args = proc_macro2::TokenStream::from(args).into_iter();
        let Some(first_arg) = args.next() else {
            panic!("structre() proc macro call missing regex argument!");
        };
        let regex_span = first_arg.span();
        let regex_raw = match first_arg {
            proc_macro2::TokenTree::Literal(l) => match StringLit::try_from(&l) {
                Ok(l) => l.value().to_string(),
                Err(_) => panic!("First arg must be literal string, got {}", l),
            },
            t => panic!("First arg must be literal, got {}", t),
        };
        if let Some(next_arg) = args.next() {
            return Err(syn::Error::new(next_arg.span(), "Only takes one arg, got more than one"));
        }
        return Ok(gen_root(regex_span, &regex_raw, ast)?);
    }() {
        Ok(t) => {
            return t.into();
        },
        Err(e) => {
            return e.into_compile_error().into();
        },
    }
}

#[cfg(test)]
mod tests {
    use {
        crate::{
            gen_enum,
            gen_struct,
            parse_re,
        },
        genemichaels_lib::FormatConfig,
        proc_macro2::TokenStream,
        quote::{
            format_ident,
            quote,
        },
    };

    fn comp(got: TokenStream, expected: TokenStream) {
        let cfg = FormatConfig::default();
        let try_format = |t: TokenStream| -> String {
            match genemichaels_lib::format_str(&quote!(fn x() {
                #t
            }).to_string(), &cfg) {
                Ok(s) => return s.rendered,
                Err(_) => return t.to_string(),
            }
        };
        let got = try_format(got);
        let expected = try_format(expected);
        assert_eq!(got, expected, "Mismatch:\n\nGot:\n{}\n\nExpected:\n{}", got, expected);
    }

    fn comp_struct(re: &str, ident: &str, rust: TokenStream, expected: TokenStream) {
        comp(
            gen_struct(
                &quote!('zzz),
                &parse_re(re).unwrap(),
                &format_ident!("{}", ident),
                &match syn::parse2::<syn::DeriveInput>(rust).unwrap().data {
                    syn::Data::Struct(d) => d,
                    _ => unreachable!(),
                },
            ).unwrap(),
            expected,
        );
    }

    fn comp_enum(re: &str, ident: &str, rust: TokenStream, expected: TokenStream) {
        comp(
            gen_enum(
                &quote!('zzz),
                &parse_re(re).unwrap(),
                &format_ident!("{}", ident),
                &match syn::parse2::<syn::DeriveInput>(rust).unwrap().data {
                    syn::Data::Enum(d) => d,
                    _ => unreachable!(),
                },
            ).unwrap(),
            expected,
        );
    }

    #[test]
    fn test_struct_unit() {
        comp_struct(
            //. .
            "a",
            "Parsed",
            quote!{
                struct Parsed;
            },
            quote!{
                return Ok(Parsed);
            },
        );
    }

    #[test]
    fn test_struct_unnamed_1() {
        comp_struct(
            //. .
            "(a)",
            "Parsed",
            quote!{
                struct Parsed(String);
            },
            quote!{
                return Ok(
                    Parsed(
                        <String as std::convert::TryFrom<&'zzz str>>::try_from(
                            captures.get(1usize).unwrap().as_str(),
                        ).map_err(|e| structre::Error::Field {
                            field: "Parsed.0",
                            error: e.to_string(),
                        })?,
                    ),
                );
            },
        );
    }

    #[test]
    fn test_struct_unnamed_2() {
        comp_struct(
            //. .
            "(a)(b)",
            "Parsed",
            quote!(
                struct Parsed(String, usize);
            ),
            quote!{
                return Ok(
                    Parsed(
                        <String as std::convert::TryFrom<&'zzz str>>::try_from(
                            captures.get(1usize).unwrap().as_str(),
                        ).map_err(|e| structre::Error::Field {
                            field: "Parsed.0",
                            error: e.to_string(),
                        })?,
                        <usize as std::str::FromStr>::from_str(
                            captures.get(2usize).unwrap().as_str(),
                        ).map_err(|e| structre::Error::Field {
                            field: "Parsed.1",
                            error: e.to_string(),
                        })?,
                    ),
                );
            },
        );
    }

    #[test]
    fn test_struct_unnamed_tuple() {
        comp_struct(
            //. .
            "(a)(b)",
            "Parsed",
            quote!(
                struct Parsed((String, usize));
            ),
            quote!{
                return Ok(
                    Parsed(
                        (
                            <String as std::convert::TryFrom<&'zzz str>>::try_from(
                                captures.get(1usize).unwrap().as_str(),
                            ).map_err(|e| structre::Error::Field {
                                field: "Parsed.0.0",
                                error: e.to_string(),
                            })?,
                            <usize as std::str::FromStr>::from_str(
                                captures.get(2usize).unwrap().as_str(),
                            ).map_err(|e| structre::Error::Field {
                                field: "Parsed.0.1",
                                error: e.to_string(),
                            })?,
                        ),
                    ),
                );
            },
        );
    }

    #[test]
    fn test_struct_named() {
        comp_struct(
            //. .
            "(?P<a>a)(?P<b>b)",
            "Parsed",
            quote!(
                struct Parsed {
                    b: usize,
                    a: String,
                }
            ),
            quote!{
                return Ok(Parsed {
                    b: <usize as std::str::FromStr>::from_str(
                        captures.get(2usize).unwrap().as_str(),
                    ).map_err(|e| structre::Error::Field {
                        field: "Parsed.b",
                        error: e.to_string(),
                    })?,
                    a: <String as std::convert::TryFrom<&'zzz str>>::try_from(
                        captures.get(1usize).unwrap().as_str(),
                    ).map_err(|e| structre::Error::Field {
                        field: "Parsed.a",
                        error: e.to_string(),
                    })?,
                });
            },
        );
    }

    #[test]
    fn test_enum() {
        comp_enum(
            //. .
            "(?P<A>a)|(?P<b>b)",
            "Parsed",
            quote!(
                enum Parsed {
                    A(String),
                    B {
                        b: String,
                    },
                }
            ),
            quote!{
                if captures.get(1usize).is_some() {
                    return Ok(
                        Parsed::A(
                            <String as std::convert::TryFrom<&'zzz str>>::try_from(
                                captures.get(1usize).unwrap().as_str(),
                            ).map_err(|e| structre::Error::Field {
                                field: "Parsed::A",
                                error: e.to_string(),
                            })?,
                        ),
                    );
                }
                if captures.get(2usize).is_some() {
                    return Ok(
                        Parsed::B {
                            b: <String as std::convert::TryFrom<&'zzz str>>::try_from(
                                captures.get(2usize).unwrap().as_str(),
                            ).map_err(|e| structre::Error::Field {
                                field: "Parsed::B.b",
                                error: e.to_string(),
                            })?,
                        },
                    );
                }
                unreachable!();
            },
        );
    }

    #[test]
    fn test_borrowed() {
        comp_struct(
            //. .
            "(?<x>.*)",
            "Parsed",
            quote!(
                struct Parsed<'a> {
                    x: &'a str,
                }
            ),
            quote!{
                return Ok(
                    Parsed {
                        x: <&'a str as std::convert::TryFrom<&'zzz str>>::try_from(
                            captures.get(1usize).unwrap().as_str(),
                        ).map_err(|e| structre::Error::Field {
                            field: "Parsed.x",
                            error: e.to_string(),
                        })?,
                    },
                );
            },
        )
    }
}
