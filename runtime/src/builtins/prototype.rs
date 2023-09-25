use crate::object_pool::{ObjectPointer, ObjectPool};
use crate::result::JsResult;
use crate::string_pool::StringPool;
use crate::values::symbols::SymbolRegistry;
use crate::Value;
use crate::{JsObject, JsPrimitiveString};

pub(crate) trait Prototype<'a> {
    fn bind<'b>(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
        object: ObjectPointer<'a>,
        prototype: ObjectPointer<'a>,
        function_prototype: ObjectPointer<'a>,
        global_this: ObjectPointer<'a>,
    ) -> JsResult<'a, JsPrimitiveString>;

    fn bind_thread<'b>(
        global_this: ObjectPointer<'a>,
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
        object_prototype: ObjectPointer<'a>,
        function_prototype: ObjectPointer<'a>,
    ) -> JsResult<'a, (ObjectPointer<'a>, ObjectPointer<'a>)> {
        let prototype = JsObject::builder(pool)
            .with_prototype(object_prototype)
            .build();

        let mut object = JsObject::new();

        object.set_prototype(function_prototype);

        let constructor_object = pool.put(object);
        constructor_object.define_value_property(
            pool,
            strings.intern_native("prototype"),
            prototype.into(),
            false,
            false,
            true,
        )?;

        let name = Self::bind(
            pool,
            strings,
            symbols,
            constructor_object,
            prototype,
            function_prototype,
            global_this,
        )?;

        global_this.define_value_property(
            pool,
            name,
            Value::from(constructor_object),
            true,
            false,
            false,
        )?;

        Ok((constructor_object, prototype))
    }

    fn bind_thread_with_prototype<'b>(
        global_this: ObjectPointer<'a>,
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
        prototype: ObjectPointer<'a>,
        function_prototype: ObjectPointer<'a>,
    ) -> JsResult<'a, (ObjectPointer<'a>, ObjectPointer<'a>)> {
        let mut object = JsObject::new();

        object.set_prototype(prototype);
        let constructor_object = pool.put(object);
        constructor_object.define_value_property(
            pool,
            strings.intern_native("prototype"),
            prototype.into(),
            false,
            false,
            true,
        )?;

        let name = Self::bind(
            pool,
            strings,
            symbols,
            constructor_object,
            prototype,
            function_prototype,
            global_this,
        )?;

        global_this.define_value_property(
            pool,
            name,
            Value::from(constructor_object),
            true,
            false,
            false,
        )?;

        Ok((constructor_object, prototype))
    }
}
