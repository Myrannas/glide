use super::builtins::prototype::Prototype;
use super::builtins::{array, errors, function, number, objects, promise, set, string};
use crate::builtins::{boolean, console, date, map, regexp, symbol};
use crate::debugging::X;
use crate::object_pool::{ObjectPointer, ObjectPool};
use crate::result::JsResult;
use crate::string_pool::{StringPointer, StringPool};
use crate::values::function::FunctionReference;
use crate::values::nan::Value;
use crate::values::object::{JsObject, Property};
use crate::values::string::JsPrimitiveString;
use crate::values::symbols::SymbolRegistry;
use crate::{BuiltIn, JsThread, ValueType};

trait Helpers<'a> {
    fn define_readonly_value<V: Into<Value<'a>>>(&mut self, key: JsPrimitiveString, value: V);
}

impl<'a> Helpers<'a> for JsObject<'a> {
    fn define_readonly_value<V: Into<Value<'a>>>(&mut self, key: JsPrimitiveString, value: V) {
        self.define_property(
            key,
            Some(FunctionReference::BuiltIn(BuiltIn {
                context: Some(value.into()),
                op: |_, _thread, _, context| Ok(Some(context.unwrap_or_default())),
                name: Some(key),
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

    message: StringPointer,
    type_error_name: StringPointer,
    syntax_error_name: StringPointer,
    reference_error_name: StringPointer,
}

#[derive(Clone)]
pub(crate) struct Primitives<'a> {
    string: ObjectPointer<'a>,
    pub(crate) function: ObjectPointer<'a>,
    number: ObjectPointer<'a>,
    pub(crate) object: ObjectPointer<'a>,
    array: ObjectPointer<'a>,
    arguments: ObjectPointer<'a>,
    promise: ObjectPointer<'a>,

    name: JsPrimitiveString,
}

#[derive(Clone)]
pub struct Realm<'a> {
    pub(crate) global_this: ObjectPointer<'a>,
    pub(crate) errors: Errors<'a>,
    pub(crate) wrappers: Primitives<'a>,
    pub(crate) objects: ObjectPool<'a>,
    pub(crate) strings: StringPool,
    pub(crate) constants: Constants,
    pub(crate) symbols: SymbolRegistry<'a>,
}

#[derive(Clone, Copy)]
pub struct Constants {
    pub r#true: StringPointer,
    pub r#false: StringPointer,
    pub undefined: StringPointer,
    pub symbol: StringPointer,
    pub nan: StringPointer,
    pub infinity: StringPointer,
    pub null: StringPointer,
    pub to_string: StringPointer,
    pub prototype: StringPointer,
    pub proto: StringPointer,
    pub empty_string: StringPointer,
    pub name: StringPointer,
    pub parse_int: StringPointer,
    pub constructor: StringPointer,
    pub enumerable: StringPointer,
    pub configurable: StringPointer,
    pub writable: StringPointer,
    pub value: StringPointer,
    pub get: StringPointer,
    pub set: StringPointer,

    pub number: StringPointer,
    pub boolean: StringPointer,
    pub string: StringPointer,
    pub object: StringPointer,
    pub function: StringPointer,
    pub message: StringPointer,
}

impl Constants {
    fn new(string_pool: &mut StringPool) -> Self {
        Constants {
            r#true: string_pool.intern_native("true"),
            r#false: string_pool.intern_native("false"),
            null: string_pool.intern_native("null"),
            undefined: string_pool.intern_native("undefined"),
            to_string: string_pool.intern_native("toString"),
            prototype: string_pool.intern_native("prototype"),
            proto: string_pool.intern_native("__proto__"),
            empty_string: string_pool.intern_native(""),
            name: string_pool.intern_native("name"),
            parse_int: string_pool.intern_native("parseInt"),
            nan: string_pool.intern_native("NaN"),
            constructor: string_pool.intern_native("constructor"),
            enumerable: string_pool.intern_native("enumerable"),
            configurable: string_pool.intern_native("configurable"),
            writable: string_pool.intern_native("writable"),
            value: string_pool.intern_native("value"),
            number: string_pool.intern_native("number"),
            boolean: string_pool.intern_native("boolean"),
            string: string_pool.intern_native("string"),
            object: string_pool.intern_native("object"),
            function: string_pool.intern_native("function"),
            message: string_pool.intern_native("message"),
            symbol: string_pool.intern_native("symbol"),
            infinity: string_pool.intern_native("Infinity"),
            get: string_pool.intern_native("get"),
            set: string_pool.intern_native("set"),
        }
    }
}

pub(crate) fn get_prototype_property<'a>(
    realm: &Realm<'a>,
    object: ObjectPointer<'a>,
) -> ObjectPointer<'a> {
    if let Some(Property::DataDescriptor { value, .. }) =
        object.get_property(&realm.objects, realm.constants.prototype)
    {
        if let ValueType::Object(obj) = value.get_type() {
            obj
        } else {
            realm.wrappers.object
        }
    } else {
        realm.wrappers.object
    }
}

impl<'a> Default for Realm<'a> {
    fn default() -> Self {
        Realm::new().unwrap()
    }
}

impl<'a> Realm<'a> {
    #[must_use]
    pub fn new() -> JsResult<'a, Realm<'a>> {
        let mut object_pool = ObjectPool::default();
        let global_this = object_pool.put(JsObject::new());
        let mut string_pool = StringPool::new();
        let constants = Constants::new(&mut string_pool);
        let mut symbols = SymbolRegistry::new();

        let primitives = Primitives::init(
            global_this,
            &mut object_pool,
            &mut string_pool,
            &constants,
            &mut symbols,
        )?;
        let errors = Errors::init(
            global_this,
            primitives.object,
            primitives.function,
            &mut object_pool,
            &mut string_pool,
            &mut symbols,
        )?;

        super::builtins::math::JsMath::bind_thread(
            global_this,
            &mut object_pool,
            &mut string_pool,
            &mut symbols,
            primitives.object,
            primitives.function,
        )?;

        #[cfg(feature = "eval")]
        {
            let eval_string = string_pool.intern_native("eval");

            let value = primitives
                .wrap_function_primordial(
                    &mut object_pool,
                    &constants,
                    eval_string,
                    BuiltIn {
                        context: None,
                        op: super::builtins::eval::eval,
                        name: Some(eval_string),
                    },
                )
                .into();

            global_this.define_value_property(
                &mut object_pool,
                eval_string,
                value,
                false,
                false,
                false,
            )?;
        }
        global_this.define_value_property(
            &mut object_pool,
            constants.undefined,
            Value::UNDEFINED,
            false,
            false,
            false,
        )?;
        global_this.define_value_property(
            &mut object_pool,
            constants.nan,
            Value::NAN,
            false,
            false,
            false,
        )?;
        global_this.define_value_property(
            &mut object_pool,
            constants.infinity,
            f64::INFINITY.into(),
            false,
            false,
            false,
        )?;

        Ok(Realm {
            global_this,
            wrappers: primitives,
            errors,
            objects: object_pool,
            strings: string_pool,
            constants,
            symbols,
        })
    }

    pub fn intern_string(&mut self, string: impl AsRef<str>) -> StringPointer {
        self.strings.intern(string)
    }

    #[must_use]
    pub fn get_string(&self, pointer: StringPointer) -> &str {
        self.strings.get(pointer).as_ref()
    }

    #[must_use]
    pub fn get_object(&self, pointer: ObjectPointer<'a>) -> &JsObject<'a> {
        &self.objects[pointer]
    }

    #[must_use]
    pub fn get_object_mut(&mut self, pointer: ObjectPointer<'a>) -> &mut JsObject<'a> {
        &mut self.objects[pointer]
    }

    pub(crate) fn wrap_function(
        &mut self,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a> {
        self.wrappers
            .wrap_function(&mut self.objects, &self.constants, name, function)
    }
}

impl<'a> Primitives<'a> {
    fn init(
        global_this: ObjectPointer<'a>,
        object_pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        constants: &Constants,
        symbols: &mut SymbolRegistry<'a>,
    ) -> JsResult<'a, Primitives<'a>> {
        let function_prototype_base = JsObject::builder(object_pool).build();
        let object_prototype_base = JsObject::builder(object_pool).build();

        objects::JsObjectBase::bind_thread_with_prototype(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        function_prototype_base.set_prototype(object_pool, object_prototype_base);

        let (_, function_prototype) = function::JsFunctionObject::bind_thread_with_prototype(
            global_this,
            object_pool,
            strings,
            symbols,
            function_prototype_base,
            function_prototype_base,
        )?;

        let (_, string_prototype) = string::JsString::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        let (symbol_contructor, _) = symbol::JsSymbol::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        let (number_constructor, ..) = number::JsNumber::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        let (_, array_prototype) = array::JsArray::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        let (promise_constructor, ..) = promise::JsPromise::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        set::JsSet::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        map::JsMap::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        regexp::RegExp::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        console::JsConsole::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        #[cfg(feature = "runtime_time")]
        date::JsDate::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        boolean::JsBoolean::bind_thread(
            global_this,
            object_pool,
            strings,
            symbols,
            object_prototype_base,
            function_prototype_base,
        )?;

        let name = strings.intern_native("name");

        let primitives = Primitives {
            string: string_prototype,
            function: function_prototype,
            object: object_prototype_base,
            array: array_prototype,
            number: number_constructor,
            arguments: array_prototype,
            promise: promise_constructor,
            name,
        };

        let parse_int = number_constructor
            .get_property(object_pool, constants.parse_int)
            .and_then(|p| match p {
                Property::DataDescriptor { value, .. } => Some(*value),
                _ => None,
            })
            .unwrap();

        global_this.define_value_property(
            object_pool,
            constants.parse_int,
            parse_int,
            false,
            false,
            false,
        )?;

        Ok(primitives)
    }

    pub(crate) fn wrap_string(
        &self,
        pool: &mut ObjectPool<'a>,
        string: JsPrimitiveString,
    ) -> ObjectPointer<'a> {
        JsObject::builder(pool)
            .with_wrapped_value(Value::from(string))
            .with_prototype(self.string)
            .build()
    }

    pub(crate) fn wrap_number(&self, pool: &mut ObjectPool<'a>, number: f64) -> ObjectPointer<'a> {
        JsObject::builder(pool)
            .with_wrapped_value(Value::from(number))
            .with_prototype(self.number)
            .build()
    }

    pub(crate) fn wrap_boolean(
        &self,
        pool: &mut ObjectPool<'a>,
        number: bool,
    ) -> ObjectPointer<'a> {
        JsObject::builder(pool)
            .with_wrapped_value(Value::from(number))
            .with_prototype(self.number) //todo: fixme
            .build()
    }

    #[allow(dead_code)]
    pub(crate) fn wrap_function(
        &self,
        pool: &mut ObjectPool<'a>,
        constants: &Constants,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a> {
        let function = function.into();
        JsObject::builder(pool)
            .with_callable(function.clone())
            .with_construct(function)
            .with_prototype(self.function)
            .with_property(constants.name, name)
            .build()
    }

    pub(crate) fn wrap_function_primordial(
        &self,
        objects: &mut ObjectPool<'a>,
        constants: &Constants,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a> {
        let function = function.into();
        let prototype_property = self.new_object(objects);

        JsObject::builder(objects)
            .with_callable(function.clone())
            .with_construct(function)
            .with_prototype(self.function)
            .with_property(constants.name, name)
            .with_property(constants.prototype, prototype_property)
            .build()
    }

    pub(crate) fn wrap_arguments(
        &self,
        pool: &mut ObjectPool<'a>,
        arguments: Vec<Value<'a>>,
    ) -> ObjectPointer<'a> {
        JsObject::builder(pool)
            .with_indexed_properties(arguments)
            .with_prototype(self.arguments)
            .build()
    }

    pub(crate) fn wrap_array(
        &self,
        pool: &mut ObjectPool<'a>,
        array: Vec<Value<'a>>,
    ) -> ObjectPointer<'a> {
        JsObject::builder(pool)
            .with_indexed_properties(array)
            .with_prototype(self.array)
            .build()
    }

    pub fn new_object(&self, pool: &mut ObjectPool<'a>) -> ObjectPointer<'a> {
        JsObject::builder(pool).with_prototype(self.object).build()
    }
}

pub(crate) trait RuntimeHelpers<'a> {
    fn new_reference_error(&mut self, message: impl AsRef<str>) -> Value<'a>;
    fn new_syntax_error(&mut self, message: impl AsRef<str>) -> Value<'a>;
    fn new_type_error(&mut self, message: impl AsRef<str>) -> Value<'a>;
    fn new_function(
        &mut self,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a>;
    fn new_function_no_prototype(
        &mut self,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a>;
}

impl<'a> RuntimeHelpers<'a> for JsThread<'a> {
    fn new_syntax_error(&mut self, message: impl AsRef<str>) -> Value<'a> {
        self.realm.new_syntax_error(message)
    }

    fn new_type_error(&mut self, message: impl AsRef<str>) -> Value<'a> {
        self.realm.new_type_error(message)
    }

    fn new_reference_error(&mut self, message: impl AsRef<str>) -> Value<'a> {
        self.realm.new_reference_error(message)
    }

    fn new_function(
        &mut self,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a> {
        self.realm.new_function(name, function)
    }

    fn new_function_no_prototype(
        &mut self,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a> {
        self.realm.new_function_no_prototype(name, function)
    }
}

impl<'a> RuntimeHelpers<'a> for Realm<'a> {
    fn new_reference_error(&mut self, message: impl AsRef<str>) -> Value<'a> {
        let message = self.strings.intern(message);

        self.errors
            .clone()
            .new_error(
                self,
                self.errors.reference_error,
                self.errors.reference_error_name,
                message,
            )
            .into()
    }

    #[allow(dead_code)]
    fn new_syntax_error(&mut self, message: impl AsRef<str>) -> Value<'a> {
        let message = self.strings.intern(message);

        self.errors
            .clone()
            .new_error(
                self,
                self.errors.syntax_error,
                self.errors.syntax_error_name,
                message,
            )
            .into()
    }

    fn new_type_error(&mut self, message: impl AsRef<str>) -> Value<'a> {
        let message = self.strings.intern(message);

        self.errors
            .clone()
            .new_error(
                self,
                self.errors.type_error,
                self.errors.type_error_name,
                message,
            )
            .into()
    }

    // fn new_reference_error(&mut self, message: impl AsRef<str>) -> Value<'a> {
    //     let message = self.strings.intern(message);
    //
    //     self.errors
    //         .clone()
    //         .new_error(
    //             self,
    //             self.errors.type_error,
    //             self.errors.type_error_name,
    //             message,
    //         )
    //         .into()
    // }

    fn new_function(
        &mut self,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a> {
        let function = function.into();
        let prototype_property = self.wrappers.new_object(&mut self.objects);

        JsObject::builder(&mut self.objects)
            .with_callable(function.clone())
            .with_construct(function)
            .with_prototype(self.wrappers.function)
            .with_property(self.constants.name, name)
            .with_property(self.constants.prototype, prototype_property)
            .build()
    }

    fn new_function_no_prototype(
        &mut self,
        name: JsPrimitiveString,
        function: impl Into<FunctionReference<'a>>,
    ) -> ObjectPointer<'a> {
        let function = function.into();

        JsObject::builder(&mut self.objects)
            .with_callable(function.clone())
            .with_property(self.constants.name, name)
            .build()
    }
}

impl<'a> Errors<'a> {
    fn init(
        global_this: ObjectPointer<'a>,
        object_prototype: ObjectPointer<'a>,
        function_prototype: ObjectPointer<'a>,
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> JsResult<'a, Errors<'a>> {
        let (.., error_prototype) = errors::JsError::bind_thread(
            global_this,
            pool,
            strings,
            symbols,
            object_prototype,
            function_prototype,
        )?;

        let syntax_error = JsObject::builder(pool)
            .with_prototype(error_prototype)
            .build();

        let (.., type_error_prototype) = errors::TypeError::bind_thread(
            global_this,
            pool,
            strings,
            symbols,
            error_prototype,
            function_prototype,
        )?;
        let (.., reference_error_prototype) = errors::ReferenceError::bind_thread(
            global_this,
            pool,
            strings,
            symbols,
            error_prototype,
            function_prototype,
        )?;

        errors::URIError::bind_thread(
            global_this,
            pool,
            strings,
            symbols,
            error_prototype,
            function_prototype,
        )?;

        errors::RangeError::bind_thread(
            global_this,
            pool,
            strings,
            symbols,
            error_prototype,
            function_prototype,
        )?;

        errors::JsSyntaxError::bind_thread(
            global_this,
            pool,
            strings,
            symbols,
            error_prototype,
            function_prototype,
        )?;

        errors::EvalError::bind_thread(
            global_this,
            pool,
            strings,
            symbols,
            error_prototype,
            function_prototype,
        )?;

        let syntax_error_name = strings.intern_native("SyntaxError");
        let type_error_name = strings.intern_native("TypeError");
        let reference_error_name = strings.intern_native("ReferenceError");

        Ok(Errors {
            reference_error: reference_error_prototype,
            syntax_error,
            type_error: type_error_prototype,
            error: error_prototype,

            message: strings.intern_native("message"),
            syntax_error_name,
            type_error_name,
            reference_error_name,
        })
    }

    fn new_error(
        &self,
        realm: &mut Realm<'a>,
        prototype: ObjectPointer<'a>,
        name: JsPrimitiveString,
        message: JsPrimitiveString,
    ) -> ObjectPointer<'a> {
        let mut builder = JsObject::builder(&mut realm.objects);

        builder
            .with_name(name)
            .with_prototype(prototype)
            .with_property(self.message, message)
            .build()
    }
}
