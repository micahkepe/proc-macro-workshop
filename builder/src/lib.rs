//! Adapted from: <https://www.youtube.com/watch?v=geovSK3wMB8>
use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

fn ty_is_option(ty: &syn::Type) -> std::option::Option<&syn::Type> {
    if let syn::Type::Path(p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != "Option" {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if !inner_ty.args.len() == 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

fn has_each_attribute(field: &syn::Field) -> bool {
    for attr in &field.attrs {
        if let Some(ident) = attr.path().get_ident()
            && ident == "builder"
        {
            let mut has_each = false;
            let _ = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("each") {
                    has_each = true
                }
                Ok(())
            });
            if has_each {
                return true;
            }
        }
    }
    false
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    //eprintln!("{:#?}", ast);

    let name = &ast.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = Ident::new(&builder_name, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        // Don't support on enums or other types
        unimplemented!()
    };

    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_is_option(ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        let mut each_name: std::option::Option<syn::Ident> = None;
        let mut has_each = false;

        for attr in &f.attrs {
            if let Some(ident) = attr.path().get_ident() {
                match ident.to_string().as_str() {
                    "builder" => {
                        if let Err(e) = attr.parse_nested_meta(|meta| {
                            if meta.path.is_ident("each") {
                                let lit: syn::LitStr = meta.value()?.parse()?;
                                each_name = Some(syn::Ident::new(&lit.value(), lit.span()));
                                has_each = true;
                                Ok(())
                            } else {
                                Err(meta.error("expected `builder(each = \"...\")`"))
                            }
                        }) {
                            return syn::Error::new_spanned(&attr.meta, e.to_string())
                                .to_compile_error();
                        }
                    }
                    _ => panic!("unrecognized attribute"),
                }
            }
        }

        if let Some(each) = each_name {
            let inner_ty = if let syn::Type::Path(ty_path) = ty {
                if let Some(s) = ty_path.path.segments.last() {
                    if s.ident == "Vec" {
                        if let syn::PathArguments::AngleBracketed(args) = &s.arguments {
                            if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                                inner
                            } else {
                                panic!("Vec must havea type argument")
                            }
                        } else {
                            panic!("Vec must have angle brackets")
                        }
                    } else {
                        panic!("#[builder(each = ... )] can only be used on Vec fields")
                    }
                } else {
                    panic!("Invalid type path")
                }
            } else {
                panic!("#[builder(each = ... )] can only be used on Vec fields")
            };

            quote! {
                pub fn #each(&mut self, #each: #inner_ty) -> &mut Self {
                    if let Some(ref mut vec) = self.#name {
                        vec.push(#each);
                    } else {
                        self.#name = Some(vec![#each])
                    }
                    self
                }
            }
        } else if let Some(inner_ty) = ty_is_option(ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if has_each_attribute(f) {
            quote! {
                #name: self.#name.clone().unwrap_or_else(Vec::new)
            }
        } else if ty_is_option(ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: std::option::Option::None
        }
    });

    let expanded = quote! {
        pub struct #builder_ident {
            #(#optionized,)*
        }

        impl #builder_ident {
            #(#methods)*

             pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                 Ok(#name {
                     #(#build_fields,)*
                 })
             }
        }

        impl #name {
            fn builder() -> #builder_ident {
                #builder_ident {
                    #(#build_empty,)*
                }
            }
        }
    };
    TokenStream::from(expanded)
}
