use core::panic;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, ExprAssign, Ident, ItemEnum};
use syn::{Attribute, DeriveInput, Expr, ExprLit, ExprPath, Lit, Meta, Path, Token};

fn parse_expr_to_ident(expr: &Expr) -> &syn::Ident {
    match expr {
        Expr::Path(ExprPath {
            attrs: _,
            qself: _,
            path: Path {
                leading_colon: _,
                segments,
            },
        }) => &segments.last().unwrap().ident,
        _ => panic!("Expected identifier"),
    }
}

fn parse_command(attr: &Attribute) -> (Option<String>, Option<Ident>) {
    if !attr.path().is_ident("command") {
        return (None, None);
    }

    let mut prefix: Option<String> = None;
    let mut argument_type: Option<Ident> = None;

    match &attr.meta {
        Meta::NameValue(name) => {
            if let Expr::Lit(ExprLit {
                attrs: _,
                lit: Lit::Str(lit_str),
            }) = &name.value
            {
                prefix = Some(lit_str.value());
            } else {
                panic!("command expects a string");
            }
        }
        Meta::List(list) => {
            for expr in list
                .parse_args_with(Punctuated::<ExprAssign, Token![,]>::parse_terminated)
                .unwrap()
            {
                match parse_expr_to_ident(&expr.left).to_string().as_str() {
                    "prefix" => {
                        if prefix.is_some() {
                            panic!("Duplicate prefix");
                        }

                        prefix = match &*expr.right {
                            Expr::Lit(ExprLit {
                                attrs: _,
                                lit: Lit::Str(s),
                            }) => Some(s.value()),
                            _ => panic!("Prefix should be a string"),
                        }
                    }
                    "arg_type" => {
                        if argument_type.is_some() {
                            panic!("Duplicate arg_type");
                        }

                        argument_type = Some(parse_expr_to_ident(&expr.right).clone());
                    }
                    _ => {
                        panic!("Unknown option")
                    }
                }
            }
        }
        _ => {
            panic!("command supports only a string value or a list of assignmets")
        }
    }

    (prefix, argument_type)
}

#[proc_macro_derive(HyprlandDataWithArgument, attributes(command))]
pub fn hyprland_data_with_argument_derive(input: TokenStream) -> TokenStream {
    let derive = parse_macro_input!(input as DeriveInput);
    let name = &derive.ident;

    let mut name_str = None;
    let mut arg_type = None;

    for attr in derive.attrs {
        (name_str, arg_type) = parse_command(&attr);
        if name_str.is_some() || arg_type.is_some() {
            break;
        }
    }

    let name_str = match name_str {
        Some(name) => name,
        None => name.to_string().to_lowercase(),
    };

    let arg_type = match arg_type {
        Some(arg_type) => arg_type,
        None => Ident::new("String", Span::call_site()),
    };

    quote! {
        impl HyprlandDataWithArgument for #name {
            type Argument = #arg_type;

            fn get_command(arg: String) -> String {
                return format!("{} {}", #name_str, arg).to_owned();
            }
        }
    }
    .into()
}

#[proc_macro_derive(HyprlandData, attributes(command))]
pub fn hyprland_data_derive(input: TokenStream) -> TokenStream {
    let derive = parse_macro_input!(input as DeriveInput);
    let name = &derive.ident;

    let mut name_str = None;
    for attr in derive.attrs {
        (name_str, _) = parse_command(&attr);
        if name_str.is_some() {
            break;
        }
    }

    let name_str = match name_str {
        Some(name) => name,
        None => name.to_string().to_lowercase(),
    };

    quote! {
        impl HyprlandData for #name {
            fn get_command() -> &'static str {
                return #name_str;
            }
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn generate_enum_types(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let enum_ = parse_macro_input!(item as ItemEnum);

    let name = format_ident!("{}Type", enum_.ident.to_string());

    let (types, type_conversion): (Vec<_>, Vec<_>) = enum_
        .variants
        .iter()
        .map(|v| {
            let type_str_name = &v.ident.to_string().to_lowercase();
            let ident = &v.ident;
            (
                ident,
                quote! {
                    #name::#ident => #type_str_name
                },
            )
        })
        .unzip();

    quote! {
        #enum_
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum #name {
            #(#types),*
        }
        impl #name {
            pub fn get_name(&self) -> &'static str {
                match self {
                    #(#type_conversion),*
                }
            }
        }
        impl AsRef<#name> for #name {
            fn as_ref(&self) -> &#name {
                &self
            }
        }
    }
    .into()
}
