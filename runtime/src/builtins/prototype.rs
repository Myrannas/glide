use crate::JsObject;

pub trait Prototype<'a> {
    fn bind<'b>(prototype: Option<&'b JsObject<'a>>) -> JsObject<'a>;
}
