use super::nan::Value;
use crate::vm::JsThread;

pub(crate) fn make_arguments<'a>(
    arguments: Vec<Value<'a>>,
    thread: &mut JsThread<'a>,
) -> Value<'a> {
    thread
        .realm
        .wrappers
        .wrap_arguments(
            &mut thread.realm.objects,
            arguments.into_iter().map(From::from).collect(),
        )
        .into()
}
