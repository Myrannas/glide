use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::ops::RuntimeFrame;
use crate::result::JsResult;
use crate::value::{BuiltIn, CustomFunctionReference, FunctionReference, RuntimeValue};
use crate::{ExecutionError, InternalError};
use std::cell::{RefCell, RefMut};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::Write;
use std::rc::Rc;

pub(crate) trait ObjectMethods<'a> {
    fn create() -> Object<'a>;
    fn create_named<S>(name: S, prototype: Option<Object<'a>>) -> Object<'a>
    where
        S: Into<String>;
    fn get<'b, 'c>(
        &self,
        key: Rc<String>,
        frame: &'c mut RuntimeFrame<'a, 'b>,
        target: &RuntimeValue<'a>,
    ) -> JsResult<'a>;
    fn set(&self, key: Rc<String>, value: RuntimeValue<'a>);
    fn set_indexed(&self, key: usize, value: RuntimeValue<'a>);
    fn define_property(
        &self,
        key: Rc<String>,
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
    );
    fn get_borrowed<'b, 'c>(
        &self,
        key: &Rc<String>,
        frame: &'c mut RuntimeFrame<'a, 'b>,
        target: &RuntimeValue<'a>,
    ) -> JsResult<'a>;
}

impl<'a> JsObject<'a> {
    fn get_property(&self, key: &Rc<String>) -> Option<Property<'a>> {
        self.properties
            .as_ref()
            .and_then(|p| p.get(key).cloned())
            .or_else(|| {
                if let Some(prototype) = &self.prototype {
                    prototype.borrow().get_property(key)
                } else {
                    None
                }
            })
    }
}

impl<'a> ObjectMethods<'a> for Object<'a> {
    fn create() -> Object<'a> {
        Rc::new(RefCell::new(JsObject {
            properties: None,
            indexed_properties: None,
            name: None,
            prototype: None,
        }))
    }

    fn create_named<S>(name: S, prototype: Option<Object<'a>>) -> Object<'a>
    where
        S: Into<String>,
    {
        Rc::new(RefCell::new(JsObject {
            properties: None,
            indexed_properties: None,
            name: Some(name.into()),
            prototype,
        }))
    }

    fn get<'b, 'c>(
        &self,
        key: Rc<String>,
        frame: &'c mut RuntimeFrame<'a, 'b>,
        target: &RuntimeValue<'a>,
    ) -> JsResult<'a> {
        self.get_borrowed(&key, frame, target)
    }

    fn set(&self, key: Rc<String>, value: RuntimeValue<'a>) {
        RefMut::map(self.borrow_mut(), |f| {
            let inner = f.properties.get_or_insert_with(HashMap::new);
            inner.insert(key, Property::Value(value));
            inner
        });
    }

    fn set_indexed(&self, key: usize, value: RuntimeValue<'a>) {
        let mut object = self.borrow_mut();

        if let Some(indexed_properties) = &mut object.indexed_properties {
            match key.cmp(&indexed_properties.len()) {
                Ordering::Less => indexed_properties[key] = value,
                Ordering::Equal => indexed_properties.push(value),
                Ordering::Greater => {
                    for _ in indexed_properties.len()..key {
                        indexed_properties.push(RuntimeValue::Undefined)
                    }
                    indexed_properties.push(value)
                }
            }
        } else {
            self.set(Rc::new(key.to_string()), value)
        }
    }

    fn define_property(
        &self,
        key: Rc<String>,
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
    ) {
        RefMut::map(self.borrow_mut(), |f| {
            let inner = f.properties.get_or_insert_with(HashMap::new);
            inner.insert(key, Property::Complex { getter, setter });
            inner
        });
    }

    fn get_borrowed<'b, 'c>(
        &self,
        key: &Rc<String>,
        frame: &'c mut RuntimeFrame<'a, 'b>,
        target: &RuntimeValue<'a>,
    ) -> Result<RuntimeValue<'a>, ExecutionError<'a>> {
        let b = self.borrow();

        if "prototype" == key.as_ref() {
            if let Some(prototype) = &b.prototype {
                return Ok(RuntimeValue::Object(prototype.clone()));
            }
        }

        let property = b.get_property(key);

        match property {
            Some(Property::Value(value)) => Ok(value),
            Some(Property::Complex {
                getter: Some(FunctionReference::Custom(CustomFunctionReference { function, .. })),
                ..
            }) => function.execute(
                None,
                &mut Vec::new(),
                0..0,
                Some(frame.call_stack.clone()),
                &frame.global_this,
                target,
            ),
            Some(Property::Complex {
                getter: Some(FunctionReference::BuiltIn(function)),
                ..
            }) => function.apply(0, frame, target),
            _ => Ok(RuntimeValue::Undefined),
        }
    }
}

impl<'a> From<Object<'a>> for RuntimeValue<'a> {
    fn from(obj: Object<'a>) -> Self {
        RuntimeValue::Object(obj)
    }
}

impl<'a> TryInto<Object<'a>> for RuntimeValue<'a> {
    type Error = ExecutionError<'a>;

    fn try_into(self) -> JsResult<'a, Object<'a>> {
        match self {
            RuntimeValue::Object(obj) => Ok(obj),
            RuntimeValue::Function(_, obj) => Ok(obj),
            RuntimeValue::String(_, obj) => Ok(obj),
            _ => InternalError::new_stackless("Incorrect object type").into(),
        }
    }
}

pub type Object<'a> = Rc<RefCell<JsObject<'a>>>;

#[derive(Clone, Debug)]
pub(crate) enum Property<'a> {
    Value(RuntimeValue<'a>),
    Complex {
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
    },
}

#[derive(Clone, Debug)]
pub struct JsObject<'a> {
    pub(crate) properties: Option<HashMap<Rc<String>, Property<'a>>>,
    pub(crate) indexed_properties: Option<Vec<RuntimeValue<'a>>>,
    pub(crate) name: Option<String>,
    pub(crate) prototype: Option<Object<'a>>,
}

impl<'a> DebugRepresentation for Object<'a> {
    fn render(&self, renderer: &mut Renderer) -> std::fmt::Result {
        let value = self.borrow();

        if let Some(name) = &value.name {
            renderer.formatter.write_fmt(format_args!("{} ", name))?;
        }

        if renderer.representation != Representation::Compact {
            renderer.formatter.write_char('{')?;
            if let Some(properties) = &value.indexed_properties {
                for (k, v) in properties.iter().enumerate() {
                    renderer.formatter.write_str(&k.to_string())?;
                    renderer.formatter.write_str(": ")?;
                    renderer.render(v)?;
                    renderer.formatter.write_str(", ")?;
                }
            }

            if let Some(properties) = &value.properties {
                for (k, v) in properties.iter() {
                    renderer.formatter.write_str(k)?;
                    renderer.formatter.write_str(": ")?;
                    match v {
                        Property::Value(v) => renderer.render(v)?,
                        Property::Complex { .. } => renderer.literal("complex")?,
                    };
                    renderer.formatter.write_str(", ")?;
                }
            }

            renderer.formatter.write_char('}')?;
        }

        Ok(())
    }
}
