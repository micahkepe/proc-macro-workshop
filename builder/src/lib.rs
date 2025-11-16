use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = Ident::new(&builder_name, name.span());
    let expanded = quote! {
        pub struct #builder_ident {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }

        impl #builder_ident {
             pub fn executable(&mut self, executable: String) -> &mut Self {
                 self.executable = Some(executable);
                 self
             }

             pub fn args(&mut self, args: Vec<String>) -> &mut Self {
                 self.args = Some(args);
                 self
             }

             pub fn env(&mut self, env: Vec<String>) -> &mut Self {
                 self.env = Some(env);
                 self
             }

             pub fn current_dir(&mut self, current_dir: String) -> &mut Self {
                 self.current_dir = Some(current_dir);
                 self
             }
        }

        impl #name {
            fn builder() -> #builder_ident {
                #builder_ident {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }
    };
    TokenStream::from(expanded)
}
