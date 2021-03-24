use crate::debugging::Unwrap;
use crate::result::JsResult;
use crate::values::nan::Value;
use crate::JsThread;
use builtin::{constructor, getter, named, prototype, varargs};

pub(crate) struct JsArray<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Array")]
impl<'a, 'b> JsArray<'a, 'b> {
    #[varargs]
    #[constructor]
    fn constructor(&mut self, args: Vec<Value<'a>>) {
        if args.len() > 1 {
            self.target
                .to_object(self.thread)
                .expect_value(
                    self.thread.get_realm(),
                    "Constructor must have an object target",
                )
                .get_mut_object(&mut self.thread.realm.objects)
                .set_indexed_properties(args)
        }
    }

    #[named("reduceRight")]
    fn reduce_right(_: &mut JsThread<'a>) -> JsResult<'a> {
        todo!("Reduce right is not supported")
    }

    fn map(&mut self, function: Value<'a>) -> JsResult<'a> {
        let mut elements = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties()
            .clone();

        for element in &mut elements {
            *element = function.call(self.thread, &[*element])?;
        }

        Ok(self
            .thread
            .realm
            .wrappers
            .wrap_array(&mut self.thread.realm.objects, elements)
            .into())
    }

    #[named("forEach")]
    fn for_each(&mut self, function: Value<'a>) -> JsResult<'a, Option<Value<'a>>> {
        let elements = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties()
            .clone();

        for element in &elements {
            function.call(self.thread, &[*element])?;
        }

        Ok(None)
    }

    fn pop(&mut self) -> JsResult<'a, Value<'a>> {
        let result = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties()
            .pop()
            .unwrap_or_default();

        Ok(result)
    }

    fn shift(&mut self) -> JsResult<'a, Value<'a>> {
        let indexed_properties = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties();

        if indexed_properties.is_empty() {
            return Ok(Value::UNDEFINED);
        }

        Ok(indexed_properties.remove(0))
    }

    fn index_of(&mut self, value: Value<'a>) -> JsResult<'a> {
        let indexed_properties = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties();

        let position = indexed_properties
            .iter()
            .position(|element| element.strict_eq(value));

        Ok(position.map_or(-1.0, |v| v as f64).into())
    }

    #[named("isArray")]
    fn is_array(_: &mut JsThread) -> bool {
        false
    }

    #[named("push")]
    #[varargs]
    fn push(&mut self, values: Vec<Value<'a>>) -> JsResult<'a, Option<Value<'a>>> {
        if self.target == Value::TRUE || self.target == Value::FALSE {
            return Ok(Some(0.0.into()));
        }

        let indexed_properties = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties();

        indexed_properties.extend(values);

        Ok(Some((indexed_properties.len() as f64).into()))
    }

    #[getter]
    fn length(&mut self) -> JsResult<'a, f64> {
        let length = self
            .target
            .to_object(self.thread)?
            .get_object(&self.thread.realm.objects)
            .get_indexed_properties()
            .len() as f64;

        Ok(length)
    }
}
