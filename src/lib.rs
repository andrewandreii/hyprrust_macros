use core::panic;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::{Attribute, DeriveInput, Expr, ExprLit, Lit, Meta};

fn parse_command(derive: &DeriveInput) -> Option<String> {
    let mut attr: Option<&Attribute> = None;
    for a in &derive.attrs {
        if let Meta::NameValue(name) = &a.meta {
            if name.path.is_ident("command") {
                attr = Some(&a);
            }
        }
    }

    if let Some(attr) = attr {
        if let Meta::NameValue(name) = &attr.meta {
            if let Expr::Lit(ExprLit { attrs: _, lit }) = &name.value {
                if let Lit::Str(lit_str) = lit {
                    return Some(lit_str.value());
                } else {
                    panic!("command expects a string");
                }
            } else {
                panic!("command expects a string");
            }
        } else {
            panic!("The command attribute is of the form #[command = \"name\"]");
        }
    };

    return None;
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
