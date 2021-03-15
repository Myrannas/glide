use crate::object_pool::{ObjectPointer, ObjectPool};
use crate::JsObject;

pub(crate) trait Prototype<'a> {
    fn bind<'b>(
        pool: &mut impl ObjectPool<'a>,
        prototype: Option<&'b ObjectPointer<'a>>,
        object: ObjectPointer<'a>,
    );

    fn bind_thread<'b>(
        pool: &mut impl ObjectPool<'a>,
        prototype: Option<&'b ObjectPointer<'a>>,
    ) -> ObjectPointer<'a> {
        let mut object = JsObject::new();

        let allocated_object = pool.allocate(object);

        Self::bind(pool, prototype, allocated_object.clone());

        allocated_object
    }
}
