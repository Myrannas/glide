extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::__private::TokenStream2;
use syn::parse::{ParseBuffer, ParseStream};
use syn::Lit;
use syn::{
    parse_macro_input, FnArg, ImplItem, ImplItemMethod, ItemImpl, Meta, NestedMeta, ReturnType,
    Signature, Type,
};

fn is_result_type(output_type: &Type) -> bool {
    if let Type::Path(type_path) = output_type {
        let last_seqment = &type_path.path.segments.last().unwrap();

        last_seqment.ident.to_string() == "JsResult"
    } else {
        panic!("Not a supported output type");
    }
}

enum BindingReturnType {
    None,
    Value,
    Result,
}

impl BindingReturnType {
    fn as_tokens(&self, call: TokenStream2) -> TokenStream2 {
        match self {
            BindingReturnType::None => quote! {
                #call;
                Ok(None)
            },
            BindingReturnType::Value => quote! {
                let result = #call;
                Ok(Some(result.into()))
            },
            BindingReturnType::Result => quote! {
                let result = #call?;
                Ok(Some(result.into()))
            },
        }
    }
}

struct Constructor {
    type_name: syn::Ident,
    parameters: usize,
}

impl ToTokens for Constructor {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let parameters = self.parameters;

        let args: Vec<TokenStream2> = (0..self.parameters)
            .map(|i| quote! { args[#i].as_ref() })
            .collect();

        let output = quote! {
            object.define_value("constructor", crate::JsObject::new().callable(crate::BuiltIn {
                context: Some(Box::new(object.clone().into())),
                desired_args: #parameters,
                op: |args, thread, receiver, context| {
                    let constructed = crate::JsObject::new()
                        .with_prototype(context.unwrap().as_object()?.clone());

                    let mut obj = <#type_name>::new(&constructed, thread);

                    obj.constructor(#(#args),*);

                    Ok(Some(constructed.into()))
                }
            }));
        };

        output.to_tokens(tokens);
    }
}

struct StaticMethod {
    js_name: String,
    type_name: syn::Ident,
    method_name: syn::Ident,
    parameters: usize,
    return_type: BindingReturnType,
}

impl ToTokens for StaticMethod {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let method_name_string = &self.js_name;
        let parameters = self.parameters;

        let args: Vec<TokenStream2> = (0..self.parameters)
            .map(|i| quote! { args[#i].as_ref() })
            .collect();

        let call = self.return_type.as_tokens(quote! {
            <#type_name>::#method_name(#(#args),*)
        });

        let output = quote! {
            object.define_value(#method_name_string, crate::JsObject::new().callable(crate::BuiltIn {
                context: None,
                desired_args: #parameters,
                op: |args, _, receiver, context| {
                    #call
                }
            }));
        };

        output.to_tokens(tokens);
    }
}

struct Method {
    js_name: String,
    type_name: syn::Ident,
    method_name: syn::Ident,
    parameters: usize,
    return_type: BindingReturnType,
}

impl ToTokens for Method {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let method_name_string = &self.js_name;
        let parameters = self.parameters;

        let args: Vec<TokenStream2> = (0..self.parameters)
            .map(|i| quote! { args[#i].as_ref() })
            .collect();

        let call = self.return_type.as_tokens(quote! {
            <#type_name>::new(receiver, thread).#method_name(#(#args),*)
        });

        let output = quote! {
            object.define_value(#method_name_string, crate::JsObject::new().callable(crate::BuiltIn {
                context: None,
                desired_args: #parameters,
                op: |args, thread, receiver, context| {
                    #call
                }
            }));
        };

        output.to_tokens(tokens);
    }
}

struct Getter {
    js_name: String,
    type_name: syn::Ident,
    method_name: syn::Ident,
    return_type: BindingReturnType,
}

impl ToTokens for Getter {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let method_name_string = &self.js_name;

        let call = self.return_type.as_tokens(quote! {
            <#type_name>::new(receiver, thread).#method_name()
        });

        let output = quote! {
            object.define_property(
                std::rc::Rc::new(#method_name_string.to_owned()),
                Some(crate::BuiltIn {
                    context: None,
                    desired_args: 0,
                    op: |args, thread, receiver, context| {
                        #call
                    }
                }.into()),
                None
            );
        };

        output.to_tokens(tokens);
    }
}

struct Callable {
    type_name: syn::Ident,
    method_name: syn::Ident,
    return_type: BindingReturnType,
    parameters: usize,
}

impl ToTokens for Callable {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let parameters = self.parameters - 1;

        let args: Vec<TokenStream2> = (0..(self.parameters - 1))
            .map(|i| quote! { args[#i].as_ref() })
            .collect();

        let call = self.return_type.as_tokens(quote! {
            <#type_name>::#method_name(thread, #(#args),*)
        });

        let output = quote! {
            object = object.callable(
                crate::BuiltIn {
                    context: None,
                    desired_args: #parameters,
                    op: |args, thread, receiver, context| {
                        #call
                    }
                });
        };

        output.to_tokens(tokens);
    }
}

enum Item {
    Constructor(Constructor),
    StaticMethod(StaticMethod),
    Method(Method),
    Getter(Getter),
    Callable(Callable),
}

impl ToTokens for Item {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Item::Constructor(constructor) => constructor.to_tokens(tokens),
            Item::Method(constructor) => constructor.to_tokens(tokens),
            Item::StaticMethod(constructor) => constructor.to_tokens(tokens),
            Item::Getter(constructor) => constructor.to_tokens(tokens),
            Item::Callable(constructor) => constructor.to_tokens(tokens),
        }
    }
}

#[proc_macro_attribute]
pub fn named(_attr: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_attribute]
pub fn getter(_attr: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_attribute]
pub fn callable(_attr: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_attribute]
pub fn prototype(_attr: TokenStream, mut input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as ItemImpl);

    if item.trait_.is_some() {
        return TokenStream::from(quote! {
            compile_error!("Can only use prototype on 'Associated methods'");
        });
    }

    let self_type = &item.self_ty;

    let type_name = if let Type::Path(type_path) = self_type.as_ref() {
        let first_segment = &type_path.path.segments[0];
        first_segment.ident.clone()
    } else {
        unreachable!();
    };

    let mut has_new = false;
    let mut methods: Vec<Item> = Vec::new();
    for item in item.items.iter() {
        if let ImplItem::Method(ImplItemMethod {
            sig:
                Signature {
                    ident,
                    inputs,
                    output,
                    ..
                },
            attrs,
            ..
        }) = item
        {
            let mut identifier = ident.clone().to_string();

            if identifier == "new" {
                has_new = true;
                continue;
            }

            let mut uses_self = false;
            let mut is_getter = false;
            let mut is_callable = false;
            let mut remaining_args = 0;
            for input in inputs.iter() {
                match input {
                    FnArg::Receiver(_) => {
                        uses_self = true;
                    }
                    FnArg::Typed(_) => {
                        remaining_args += 1;
                    }
                    _ => {}
                }
            }

            for attribute in attrs.iter() {
                if let Meta::List(meta) = attribute.parse_meta().unwrap() {
                    if meta.path.get_ident().unwrap() == "named" {
                        match meta.nested.first().unwrap() {
                            NestedMeta::Lit(Lit::Str(value)) => identifier = value.value(),
                            _ => {}
                        }
                    }
                }

                if let Meta::Path(path) = attribute.parse_meta().unwrap() {
                    let path = path.get_ident().unwrap();
                    if path == "getter" {
                        if remaining_args > 0 {
                            return TokenStream::from(quote! {
                                compile_error!("Cannot use args for getters");
                            });
                        }
                        is_getter = true;
                    } else if path == "callable" {
                        if uses_self {
                            return TokenStream::from(quote! {
                                compile_error!("Callable cant reference self");
                            });
                        }
                        is_callable = true;
                    }
                }
            }

            if identifier == "constructor" {
                if !uses_self {
                    return TokenStream::from(quote! {
                        compile_error!("Must reference self in a constructor");
                    });
                } else {
                    methods.push(Item::Constructor(Constructor {
                        type_name: type_name.clone(),
                        parameters: remaining_args,
                    }));
                    continue;
                }
            } else {
                let return_type = if let ReturnType::Type(.., tpe) = output {
                    if is_result_type(tpe.as_ref()) {
                        BindingReturnType::Result
                    } else {
                        BindingReturnType::Value
                    }
                } else {
                    BindingReturnType::None
                };

                if is_callable {
                    methods.push(Item::Callable(Callable {
                        type_name: type_name.clone(),
                        parameters: remaining_args,
                        method_name: ident.clone(),
                        return_type,
                    }))
                } else if is_getter {
                    methods.push(Item::Getter(Getter {
                        type_name: type_name.clone(),
                        method_name: ident.clone(),
                        return_type,
                        js_name: identifier,
                    }))
                } else if uses_self {
                    methods.push(Item::Method(Method {
                        type_name: type_name.clone(),
                        parameters: remaining_args,
                        method_name: ident.clone(),
                        return_type,
                        js_name: identifier,
                    }))
                } else {
                    methods.push(Item::StaticMethod(StaticMethod {
                        type_name: type_name.clone(),
                        parameters: remaining_args,
                        method_name: ident.clone(),
                        return_type,
                        js_name: identifier,
                    }))
                }
            }
        }
    }

    let constructor = if !has_new {
        quote! {
            impl<'a, 'b> #self_type {
                fn new(object: &'b crate::JsObject<'a>, thread: &'b mut JsThread<'a>) -> Self {
                    Self {
                        object,
                        thread
                    }
                }
            }
        }
    } else {
        quote! {}
    };

    let name = type_name.to_string();

    TokenStream::from(quote! {
        #item
        #constructor

        impl <'a, 'b> crate::builtins::prototype::Prototype<'a> for #self_type {
            fn bind<'c>(prototype: Option<&JsObject<'a>>) -> crate::JsObject<'a> {
                let mut object = crate::JsObject::new();

                if let Some(prototype) = prototype {
                    object = object.with_prototype(prototype.clone());
                }

                #(#methods)*

                object.define_value("name", crate::RuntimeValue::String(std::rc::Rc::new(#name.to_owned())));

                object
            }
        }
    })
}
