use super::builtins::prototype::Prototype;
use super::builtins::{array, errors, function, number, objects, promise, string};
use crate::object_pool::{JsObjectPool, ObjectPointer, ObjectPool};
use crate::values::function::FunctionReference;
use crate::values::object::{JsObject, Property};
use crate::values::string::JsPrimitiveString;
use crate::values::value::RuntimeValue;
use crate::BuiltIn;

trait Helpers<'a> {
    fn define_readonly_value<S: Into<String>, V: Into<RuntimeValue<'a>>>(
        &mut self,
        key: S,
        value: V,
    );
}

impl<'a> Helpers<'a> for JsObject<'a> {
    fn define_readonly_value<S: Into<String>, V: Into<RuntimeValue<'a>>>(
        &mut self,
        key: S,
        value: V,
    ) {
        self.define_property(
            key.into(),
            Some(FunctionReference::BuiltIn(BuiltIn {
                context: Some(Box::new(value.into())),
                op: |_, _thread, _, context| Ok(Some(context.unwrap().clone())),
            })),
            None,
            true,
            true,
        )
    }
}

#[derive(Clone)]
pub(crate) struct Errors<'a> {
    reference_error: ObjectPointer<'a>,
    syntax_error: ObjectPointer<'a>,
    type_error: ObjectPointer<'a>,
    error: ObjectPointer<'a>,
}

#[derive(Clone)]
pub(crate) struct Primitives<'a> {
    string: ObjectPointer<'a>,
    function: ObjectPointer<'a>,
    number: ObjectPointer<'a>,
    object: ObjectPointer<'a>,
    array: ObjectPointer<'a>,
    arguments: ObjectPointer<'a>,
}

#[derive(Clone)]
pub struct Realm<'a> {
    pub(crate) global_this: ObjectPointer<'a>,
    pub(crate) errors: Errors<'a>,
    pub(crate) wrappers: Primitives<'a>,
    pub(crate) objects: JsObjectPool<'a>,
}

impl<'a> Default for Realm<'a> {
    fn default() -> Self {
        Realm::new()
    }
}

impl<'a> Realm<'a> {
    pub fn new() -> Realm<'a> {
        let mut global_this = JsObject::new();
        let mut object_pool = JsObjectPool::new();

        let primitives = Primitives::init(&mut global_this, &mut object_pool);
        let errors = Errors::init(&mut global_this, &primitives.object, &mut object_pool);

        global_this.define_value(
            "Math",
            super::builtins::math::JsMath::bind_thread(&mut object_pool, None),
        );

        #[cfg(feature = "eval")]
        {
            global_this.define_readonly_value(
                "eval",
                primitives.wrap_function(
                    &mut object_pool,
                    "eval",
                    BuiltIn {
                        context: None,
                        op: super::builtins::eval::eval,
                    },
                ),
            );
        }
        global_this.define_readonly_value("undefined", RuntimeValue::Undefined);
        global_this.define_readonly_value("NaN", f64::NAN);

        Realm {
            global_this: object_pool.allocate(global_this),
            wrappers: primitives,
            errors,
            objects: object_pool,
        }
    }
}

impl<'a> Primitives<'a> {
    fn init(
        global_this: &mut JsObject<'a>,
        object_pool: &mut impl ObjectPool<'a>,
    ) -> Primitives<'a> {
        let object_prototype = objects::JsObjectBase::bind_thread(object_pool, None);

        let string_prototype = string::JsString::bind_thread(object_pool, Some(&object_prototype));
        let number_prototype: ObjectPointer<'a> =
            number::JsNumber::bind_thread(object_pool, Some(&object_prototype));
        let array_prototype = array::JsArray::bind_thread(object_pool, Some(&object_prototype));

        let function_prototype =
            function::JsFunctionObject::bind_thread(object_pool, Some(&object_prototype));
        let promise_prototype =
            promise::JsPromise::bind_thread(object_pool, Some(&object_prototype));

        let primitives = Primitives {
            string: string_prototype.clone(),
            function: function_prototype.clone(),
            object: object_prototype.clone(),
            array: array_prototype.clone(),
            number: number_prototype.clone(),
            arguments: array_prototype.clone(),
        };

        let parse_int = number_prototype
            .get_property(object_pool, &"parseInt".into())
            .and_then(|p| match p {
                Property::DataDescriptor { value, .. } => Some(value),
                _ => None,
            });

        global_this.define_value("String", string_prototype);
        global_this.define_value("Array", array_prototype);
        global_this.define_value("Object", object_prototype);
        global_this.define_value("Function", function_prototype);
        global_this.define_value("Promise", promise_prototype);
        global_this.define_value("Number", number_prototype);
        global_this.define_value("parseInt", parse_int);

        primitives
    }

    pub(crate) fn wrap_string(
        &self,
        pool: &mut impl ObjectPool<'a>,
        string: JsPrimitiveString,
    ) -> ObjectPointer<'a> {
        JsObject::builder()
            .with_wrapped_value(RuntimeValue::String(string))
            .with_prototype(self.string.clone())
            .build(pool)
    }

    pub(crate) fn wrap_number(
        &self,
        pool: &mut impl ObjectPool<'a>,
        number: f64,
    ) -> ObjectPointer<'a> {
        JsObject::builder()
            .with_wrapped_value(RuntimeValue::Float(number))
            .with_prototype(self.number.clone())
            .build(pool)
    }

    pub(crate) fn wrap_boolean(
        &self,
        pool: &mut impl ObjectPool<'a>,
        number: bool,
    ) -> ObjectPointer<'a> {
        JsObject::builder()
            .with_wrapped_value(RuntimeValue::Boolean(number))
            .with_prototype(self.number.clone()) //todo: fixme
            .build(pool)
    }

    pub(crate) fn wrap_function(
        &self,
        thread: &mut impl ObjectPool<'a>,
        name: impl Into<String>,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a> {
        let function = function.into();
        JsObject::builder()
            .with_callable(function)
            .with_prototype(self.function.clone())
            .with_property("name", name.into())
            .build(thread)
    }

    pub(crate) fn wrap_arguments(
        &self,
        thread: &mut impl ObjectPool<'a>,
        arguments: Vec<RuntimeValue<'a>>,
    ) -> ObjectPointer<'a> {
        JsObject::builder()
            .with_indexed_properties(arguments)
            .with_prototype(self.arguments.clone())
            .build(thread)
    }

    pub fn new_object(&self, thread: &mut impl ObjectPool<'a>) -> ObjectPointer<'a> {
        JsObject::builder()
            .with_prototype(self.object.clone())
            .build(thread)
    }
}

impl<'a> Errors<'a> {
    #[allow(dead_code)]
    pub(crate) fn new_reference_error(
        &self,
        pool: &mut impl ObjectPool<'a>,
        message: impl Into<String>,
    ) -> RuntimeValue<'a> {
        self.new_error(
            pool,
            &self.reference_error,
            "ReferenceError",
            message.into(),
        )
        .into()
    }

    #[allow(dead_code)]
    pub(crate) fn new_syntax_error(
        &self,
        pool: &mut impl ObjectPool<'a>,
        message: impl Into<String>,
    ) -> RuntimeValue<'a> {
        self.new_error(pool, &self.syntax_error, "SyntaxError", message.into())
            .into()
    }

    pub(crate) fn new_type_error(
        &self,
        pool: &mut impl ObjectPool<'a>,
        message: impl Into<String>,
    ) -> RuntimeValue<'a> {
        self.new_error(pool, &self.type_error, "TypeError", message.into())
            .into()
    }

    fn new_error(
        &self,
        pool: &mut impl ObjectPool<'a>,
        prototype: &ObjectPointer<'a>,
        name: &str,
        message: impl Into<String>,
    ) -> ObjectPointer<'a> {
        JsObject::builder()
            .with_prototype(prototype.clone())
            .with_name(name)
            .with_property("message", message.into())
            .build(pool)
    }

    fn init(
        global_this: &mut JsObject<'a>,
        object_prototype: &ObjectPointer<'a>,
        pool: &mut impl ObjectPool<'a>,
    ) -> Errors<'a> {
        let error = errors::JsError::bind_thread(pool, Some(object_prototype));

        let syntax_error = JsObject::builder()
            .with_prototype(error.clone())
            .build(pool);

        let type_error = errors::TypeError::bind_thread(pool, Some(&error));
        let reference_error = errors::ReferenceError::bind_thread(pool, Some(&error));

        global_this.define_readonly_value("ReferenceError", reference_error.clone());
        global_this.define_readonly_value("SyntaxError", syntax_error.clone());
        global_this.define_readonly_value("TypeError", type_error.clone());
        global_this.define_readonly_value("Error", error.clone());

        Errors {
            syntax_error,
            reference_error,
            type_error,
            error,
        }
    }
}
