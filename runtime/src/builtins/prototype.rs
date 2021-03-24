use crate::object_pool::{ObjectPointer, ObjectPool};
use crate::string_pool::StringPool;
use crate::Value;
use crate::{JsObject, JsPrimitiveString};

pub(crate) trait Prototype<'a> {
    fn bind<'b>(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        prototype: Option<&'b ObjectPointer<'a>>,
        object: ObjectPointer<'a>,
        function_prototype: ObjectPointer<'a>,
    ) -> JsPrimitiveString;

    fn bind_thread<'b>(
        global_this: ObjectPointer<'a>,
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        prototype: Option<&'b ObjectPointer<'a>>,
        function_prototype: ObjectPointer<'a>,
    ) -> ObjectPointer<'a> {
        let object = JsObject::new();

        let allocated_object = pool.allocate(object);

        let name = Self::bind(
            pool,
            strings,
            prototype,
            allocated_object,
            function_prototype,
        );

        global_this.define_value_property(
            pool,
            name,
            Value::from(allocated_object),
            true,
            true,
            false,
        );

        allocated_object
    }
}
