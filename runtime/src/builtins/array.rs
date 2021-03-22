use crate::result::JsResult;
use crate::{JsThread, RuntimeValue};
use builtin::{constructor, getter, named, prototype, varargs};

pub(crate) struct JsArray<'a, 'b> {
    target: RuntimeValue<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Array")]
impl<'a, 'b> JsArray<'a, 'b> {
    #[varargs]
    #[constructor]
    fn constructor(&mut self, args: Vec<RuntimeValue<'a>>) {
        if args.len() > 1 {
            self.target
                .to_object(self.thread)
                .expect("Constructor must have an object target")
                .get_mut_object(&mut self.thread.realm.objects)
                .set_indexed_properties(args)
        }
    }

    #[named("reduceRight")]
    fn reduce_right(thread: &mut JsThread<'a>) -> JsResult<'a> {
        todo!("Reduce right is not supported")
    }

    fn map(&mut self, function: &RuntimeValue<'a>) -> JsResult<'a> {
        let mut elements = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties()
            .clone();

        for element in &mut elements {
            let element_to_map = (*element).clone();
            *element = function.call(self.thread, &[element_to_map])?;
        }

        Ok(self
            .thread
            .realm
            .wrappers
            .wrap_array(&mut self.thread.realm.objects, elements)
            .into())
    }

    #[named("forEach")]
    fn for_each(&mut self, function: &RuntimeValue<'a>) -> JsResult<'a, Option<RuntimeValue<'a>>> {
        let elements = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties()
            .clone();

        for element in &elements {
            let element_to_map = (*element).clone();
            function.call(self.thread, &[element_to_map])?;
        }

        Ok(None)
    }

    fn pop(&mut self) -> JsResult<'a, RuntimeValue<'a>> {
        let result = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties()
            .pop()
            .unwrap_or_default();

        Ok(result)
    }

    fn shift(&mut self) -> JsResult<'a, RuntimeValue<'a>> {
        let indexed_properties = self
            .target
            .to_object(self.thread)?
            .get_mut_object(&mut self.thread.realm.objects)
            .get_mut_indexed_properties();

        if indexed_properties.is_empty() {
            return Ok(RuntimeValue::Undefined);
        }

        Ok(indexed_properties.remove(0))
    }

    fn index_of(&mut self, value: &RuntimeValue<'a>) -> JsResult<'a> {
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
    fn push(&mut self, value: Vec<RuntimeValue<'a>>) -> JsResult<'a, Option<RuntimeValue<'a>>> {
        match &self.target {
            RuntimeValue::Boolean(_) => Ok(Some(0.0.into())),
            other => {
                let indexed_properties = other
                    .to_object(self.thread)?
                    .get_mut_object(&mut self.thread.realm.objects)
                    .get_mut_indexed_properties();

                indexed_properties.extend(value);

                Ok(Some((indexed_properties.len() as f64).into()))
            }
        }
    }

    #[getter]
    fn length(&mut self) -> JsResult<'a, f64> {
        let length = self
            .target
            .to_object(self.thread)?
            .get_object(&mut self.thread.realm.objects)
            .get_indexed_properties()
            .len() as f64;

        Ok(length)
    }
}
