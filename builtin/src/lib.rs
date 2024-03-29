extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::Lit;
use syn::__private::TokenStream2;
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
    method_name: syn::Ident,
    arguments: Arguments,
    return_type: BindingReturnType,
}

impl ToTokens for Constructor {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let setup = &self.arguments.setup();
        let arguments = &self.arguments.call();
        let pretty_name = method_name.clone().to_string();

        let call_type = match self.return_type {
            BindingReturnType::None => quote! {
                obj.#method_name(#arguments)
            },
            BindingReturnType::Value => quote! {
                obj.#method_name(#arguments)
            },
            BindingReturnType::Result => quote! {
                obj.#method_name(#arguments)?
            },
        };

        let output = quote! {
            let constructor: crate::values::function::FunctionReference = crate::BuiltIn {
                context: None,
                op: |args, thread, receiver, context| {
                    #setup

                    let mut obj = <#type_name>::new(receiver, thread);

                    #call_type;

                    Ok(None)
                },
                name: Some(strings.intern_native(#pretty_name))
            }.into();

            constructor_object.set_construct(pool, constructor.clone());
            prototype.define_value_property(pool, strings.intern_native("constructor"), Value::from(constructor_object), false, false, true)?;
        };

        output.to_tokens(tokens);
    }
}

struct StaticMethod {
    js_name: String,
    type_name: syn::Ident,
    method_name: syn::Ident,
    arguments: Arguments,
    return_type: BindingReturnType,
    is_global: bool,
}

impl ToTokens for StaticMethod {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let method_name_string = &self.js_name;
        let setup = &self.arguments.setup();
        let arguments = &self.arguments.call();
        let args_len = self.arguments.len();

        let call = self.return_type.as_tokens(quote! {
            <#type_name>::#method_name(thread, #arguments)
        });

        let global_assignment = if self.is_global {
            quote! {
                global_this.define_value_property(pool, name, Value::from(method), true, false, true)?;
            }
        } else {
            quote! {}
        };

        println!("Global Assignment {}!!", self.is_global);

        let output = quote! {
            let name: crate::JsPrimitiveString = strings.intern_native(#method_name_string);
            let method = crate::JsObject::builder(pool).with_callable(crate::BuiltIn {
                context: None,
                op: |args, thread, receiver, context| {
                    #setup
                    #call
                },
                name: Some(name)
            }).with_prototype(function_prototype).build();

            method.define_value_property(pool, strings.intern_native("name"), Value::from(name), false, false, true)?;
            method.define_value_property(pool, strings.intern_native("length"), Value::from(#args_len), false, false, true)?;
            constructor_object.define_value_property(pool, name, Value::from(method), true, false, true)?;
            #global_assignment
        };

        output.to_tokens(tokens);
    }
}

struct Method {
    js_name: String,
    type_name: syn::Ident,
    method_name: syn::Ident,
    return_type: BindingReturnType,
    arguments: Arguments,
}

impl ToTokens for Method {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let method_name_string = &self.js_name;
        let setup = &self.arguments.setup();
        let arguments = &self.arguments.call();
        let args_len = self.arguments.len();

        let call = self.return_type.as_tokens(quote! {
            <#type_name>::new(receiver, thread).#method_name(#arguments)
        });

        let output = quote! {
            let name: crate::JsPrimitiveString = strings.intern_native(#method_name_string);
            let method = crate::JsObject::builder(pool).with_callable(crate::BuiltIn {
                context: None,
                op: |args, thread, receiver, context| {
                    #setup
                    #call
                },
                name: Some(name)
            }).with_prototype(function_prototype).build();
            method.define_value_property(pool, strings.intern_native("length"), Value::from(#args_len), false, false, true)?;
            method.define_value_property(pool, strings.intern_native("name"), Value::from(name), false, false, true)?;
            prototype.define_value_property(pool, name, Value::from(method), true, false, true)?;
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
            let name = strings.intern_native(#method_name_string);
            prototype.define_property(
                pool,
                name,
                Some(crate::BuiltIn {
                    context: None,
                    op: |args, thread, receiver, context| {
                        #call
                    },
                    name: Some(name)
                }.into()),
                None,
                false,
                true
            );
        };

        output.to_tokens(tokens);
    }
}

struct Callable {
    type_name: syn::Ident,
    method_name: syn::Ident,
    return_type: BindingReturnType,
    arguments: Arguments,
}

#[derive(Clone, Copy)]
enum ArgumentType {
    ByVal,
    ByRef,
}

enum Arguments {
    List(Vec<ArgumentType>),
    Varargs,
}

impl Arguments {
    fn setup(&self) -> TokenStream2 {
        match self {
            Arguments::List(args) => {
                let args: Vec<TokenStream2> = args
                    .iter()
                    .enumerate()
                    .rev()
                    .map(|(i, _)| {
                        let arg_id = format_ident!("arg_{}", i);
                        quote! { let #arg_id = thread.read_arg(args, #i).map_or(Ok(None), |r| r.map(Some))?; }
                    })
                    .collect();

                quote! {
                    #(#args)*
                }
            }
            Arguments::Varargs => {
                quote! {
                    let stack_len = thread.stack.len();
                    let mut args = thread.stack[(stack_len - args)..stack_len].to_vec();

                    for arg in &mut args {
                        *arg = arg.resolve(thread)?;
                    }
                }
            }
        }
    }

    fn call(&self) -> TokenStream2 {
        match self {
            Arguments::List(args) => {
                let args: Vec<TokenStream2> = args
                    .iter()
                    .enumerate()
                    .map(|(i, argument)| {
                        let arg_id = format_ident!("arg_{}", i);

                        match argument {
                            ArgumentType::ByRef => {
                                quote! { &#arg_id.into() }
                            }
                            ArgumentType::ByVal => {
                                quote! { #arg_id.into() }
                            }
                        }
                    })
                    .collect();

                quote! {
                    #(#args),*
                }
            }
            Arguments::Varargs => {
                quote! {
                    args
                }
            }
        }
    }

    fn len(&self) -> f64 {
        match self {
            Arguments::List(args) => args.len() as f64,
            Arguments::Varargs => 0.0,
        }
    }
}

impl ToTokens for Callable {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let method_name_string = type_name.clone().to_string();
        let setup = &self.arguments.setup();
        let arguments = &self.arguments.call();

        let call = self.return_type.as_tokens(quote! {
            <#type_name>::#method_name(thread, #arguments)
        });

        let output = quote! {
            constructor_object.set_callable(
                pool,
                crate::BuiltIn {
                    context: None,
                    op: |args, thread, receiver, context| {
                        #setup
                        #call
                    },
                    name: Some(strings.intern_native(#method_name_string))
                });
        };

        output.to_tokens(tokens);
    }
}

struct Global {
    js_name: String,
    type_name: syn::Ident,
    method_name: syn::Ident,
}

impl ToTokens for Global {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let js_name = &self.js_name;

        let output = quote! {
            // Constant #js_name
            let value = <#type_name>::#method_name(pool, strings, symbols);
            let name = #js_name;
            global_this.define_value_property(
                pool,
                strings.intern_native(name),
                value,
                false,
                false,
                false
            )?;
        };

        output.to_tokens(tokens);
    }
}

struct Constant {
    js_name: String,
    type_name: syn::Ident,
    method_name: syn::Ident,
    is_global: bool,
}

impl ToTokens for Constant {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = &self.type_name;
        let method_name = &self.method_name;
        let js_name = &self.js_name;

        let global_assignment = if self.is_global {
            quote! {
                constructor_object.define_value_property(
                pool,
                interned_name,
                value,
                false,
                false,
                false
            )?;
            }
        } else {
            quote! {}
        };

        let output = quote! {
            // Constant #js_name
            let value = <#type_name>::#method_name(pool, strings, symbols);
            let name = #js_name;
            let interned_name = strings.intern_native(name);
            constructor_object.define_value_property(
                pool,
                interned_name,
                value,
                false,
                false,
                false
            )?;
            #global_assignment
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
    Constant(Constant),
    Global(Global),
}

impl ToTokens for Item {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Item::Constructor(constructor) => constructor.to_tokens(tokens),
            Item::Method(constructor) => constructor.to_tokens(tokens),
            Item::StaticMethod(constructor) => constructor.to_tokens(tokens),
            Item::Getter(constructor) => constructor.to_tokens(tokens),
            Item::Callable(constructor) => constructor.to_tokens(tokens),
            Item::Constant(constant) => constant.to_tokens(tokens),
            Item::Global(global) => global.to_tokens(tokens),
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
pub fn varargs(_attr: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_attribute]
pub fn constructor(_attr: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_attribute]
pub fn constant(_attr: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_attribute]
pub fn target(_attr: TokenStream, input: TokenStream) -> TokenStream {
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

    let mut type_name = if let Type::Path(type_path) = self_type.as_ref() {
        let first_segment = &type_path.path.segments[0];
        first_segment.ident.clone()
    } else {
        unreachable!();
    };

    let mut type_identifier = type_name.to_string();
    for attribute in item.attrs.iter() {
        if let Meta::List(meta) = attribute.parse_meta().unwrap() {
            if meta.path.get_ident().unwrap() == "named" {
                if let NestedMeta::Lit(Lit::Str(value)) = meta.nested.first().unwrap() {
                    type_identifier = value.value();
                }
            }
        }
    }

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
            let mut is_varargs = false;
            let mut is_constructor = false;
            let mut is_constant = false;
            let mut is_global = false;
            let mut args = Vec::new();
            for input in inputs.iter() {
                match input {
                    FnArg::Receiver(_) => {
                        uses_self = true;
                    }
                    FnArg::Typed(tpe) => match *tpe.ty {
                        Type::Reference(..) => args.push(ArgumentType::ByRef),
                        _ => args.push(ArgumentType::ByVal),
                    },
                    _ => {}
                }
            }

            for attribute in attrs.iter() {
                if let Meta::List(meta) = attribute.parse_meta().unwrap() {
                    if meta.path.get_ident().unwrap() == "named" {
                        if let Some(NestedMeta::Lit(Lit::Str(value))) = meta.nested.first() {
                            identifier = value.value()
                        }
                    } else if meta.path.get_ident().unwrap() == "target" {
                        is_global = true;
                    }
                }

                if let Meta::Path(path) = attribute.parse_meta().unwrap() {
                    let path = path.get_ident().unwrap();
                    if path == "getter" {
                        is_getter = true;
                    } else if path == "callable" {
                        is_callable = true;
                    } else if path == "varargs" {
                        is_varargs = true;
                    } else if path == "constructor" {
                        is_constructor = true;
                    } else if path == "constant" {
                        is_constant = true;
                    }
                }
            }

            if uses_self && is_callable {
                return TokenStream::from(quote! {
                    compile_error!("Callable cant reference self");
                });
            }

            if is_getter && (is_varargs || args.len() > 0) {
                return TokenStream::from(quote! {
                    compile_error!("Cannot use args for getters");
                });
            }

            let arguments = if is_varargs {
                Arguments::Varargs
            } else {
                Arguments::List(args)
            };

            let return_type = if let ReturnType::Type(.., tpe) = output {
                if is_result_type(tpe.as_ref()) {
                    BindingReturnType::Result
                } else {
                    BindingReturnType::Value
                }
            } else {
                BindingReturnType::None
            };

            if is_constant {
                methods.push(Item::Constant(Constant {
                    type_name: type_name.clone(),
                    js_name: identifier,
                    method_name: ident.clone(),
                    is_global,
                }))
            } else if is_constructor {
                if !uses_self {
                    return TokenStream::from(quote! {
                        compile_error!("Must reference self in a constructor");
                    });
                } else {
                    methods.push(Item::Constructor(Constructor {
                        type_name: type_name.clone(),
                        arguments,
                        method_name: ident.clone(),
                        return_type,
                    }));
                    continue;
                }
            } else if is_callable {
                let arguments = match arguments {
                    Arguments::Varargs => Arguments::Varargs,
                    Arguments::List(size) => Arguments::List(size[1..].to_vec()),
                };

                methods.push(Item::Callable(Callable {
                    type_name: type_name.clone(),
                    arguments,
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
                    arguments,
                    method_name: ident.clone(),
                    return_type,
                    js_name: identifier,
                }))
            } else {
                let arguments = match arguments {
                    Arguments::Varargs => Arguments::Varargs,
                    Arguments::List(size) => Arguments::List(size[1..].to_vec()),
                };

                methods.push(Item::StaticMethod(StaticMethod {
                    type_name: type_name.clone(),
                    arguments,
                    method_name: ident.clone(),
                    return_type,
                    js_name: identifier,
                    is_global,
                }))
            }
        }
    }

    let constructor = if !has_new {
        quote! {
            impl<'a, 'b> #self_type {
                fn new(target: crate::values::nan::Value<'a>, thread: &'b mut JsThread<'a>) -> Self {
                    Self {
                        target,
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
            fn bind<'c>(
                pool: &mut crate::object_pool::ObjectPool<'a>,
                strings: &mut crate::string_pool::StringPool,
                symbols: &mut crate::values::symbols::SymbolRegistry<'a>,
                constructor_object: crate::object_pool::ObjectPointer<'a>,
                prototype: crate::object_pool::ObjectPointer<'a>,
                function_prototype: crate::object_pool::ObjectPointer<'a>,
                global_this: crate::object_pool::ObjectPointer<'a>
            ) -> crate::result::JsResult<'a, crate::JsPrimitiveString> {
                #(#methods)*

                let type_name: crate::JsPrimitiveString = strings.intern_native(#name);
                constructor_object.define_value_property(pool, strings.intern_native("name"), Value::from(strings.intern_native(#type_identifier)), false, false, true)?;
                prototype.define_value_property(pool, strings.intern_native("name"), Value::from(strings.intern_native(#type_identifier)), false, false, true)?;

                Ok(strings.intern_native(#type_identifier))
            }
        }
    })
}
