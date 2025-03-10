use core::panic;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Attribute, DeriveInput, Expr, ExprLit, Lit, Meta};
use syn::{ItemEnum, parse_macro_input};

fn parse_command(derive: &DeriveInput) -> Option<String> {
    let mut attr: Option<&Attribute> = None;
    for a in &derive.attrs {
        if let Meta::NameValue(name) = &a.meta {
            if name.path.is_ident("command") {
                attr = Some(a);
            }
        }
    }

    if let Some(attr) = attr {
        if let Meta::NameValue(name) = &attr.meta {
            if let Expr::Lit(ExprLit {
                attrs: _,
                lit: Lit::Str(lit_str),
            }) = &name.value
            {
                return Some(lit_str.value());
            } else {
                panic!("command expects a string");
            }
        } else {
            panic!("The command attribute is of the form #[command = \"name\"]");
        }
    };

    None
}

#[proc_macro_derive(HyprlandDataWithArgument, attributes(command))]
pub fn hyprland_data_with_argument_derive(input: TokenStream) -> TokenStream {
    let derive = parse_macro_input!(input as DeriveInput);
    let name = &derive.ident;

    let name_str = match parse_command(&derive) {
        Some(name) => name,
        None => name.to_string().to_lowercase(),
    };

    quote! {
        impl HyprlandDataWithArgument for #name {
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

    let name_str = match parse_command(&derive) {
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
