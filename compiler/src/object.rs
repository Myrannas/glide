use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::result::JsResult;
use crate::value::{FunctionReference, RuntimeValue};
use crate::vm::JsThread;
use crate::{ExecutionError, InternalError};
use std::cell::{RefCell, RefMut};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::Write;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct JsObject<'a> {
    inner: Rc<RefCell<JsObjectInner<'a>>>,
}

#[derive(Clone, Debug)]
struct JsObjectInner<'a> {
    pub(crate) properties: Option<HashMap<Rc<String>, Property<'a>>>,
    pub(crate) indexed_properties: Option<Vec<RuntimeValue<'a>>>,
    pub(crate) name: Option<String>,
    pub(crate) prototype: Option<JsObject<'a>>,
}

impl<'a> JsObject<'a> {
    fn get_property(&self, key: &Rc<String>) -> Option<Property<'a>> {
        self.inner
            .borrow()
            .properties
            .as_ref()
            .and_then(|p| p.get(key).cloned())
            .or_else(|| {
                if let Some(prototype) = &self.inner.borrow().prototype {
                    prototype.get_property(key)
                } else {
                    None
                }
            })
    }

    pub fn new() -> Self {
        JsObject {
            inner: Rc::new(RefCell::new(JsObjectInner {
                properties: None,
                indexed_properties: None,
                name: None,
                prototype: None,
            })),
        }
    }

    pub fn with_prototype(self, prototype: Self) -> Self {
        self.inner.borrow_mut().prototype = Some(prototype);
        self
    }

    pub fn with_name(self, name: impl Into<String>) -> Self {
        self.inner.borrow_mut().name = Some(name.into());
        self
    }

    pub fn with_indexed_properties(self, properties: Vec<RuntimeValue<'a>>) -> Self {
        self.inner.borrow_mut().indexed_properties = Some(properties.into());
        self
    }

    pub fn get<'b, 'c>(
        &self,
        key: Rc<String>,
        frame: &'c mut JsThread<'a>,
        target: &RuntimeValue<'a>,
    ) -> JsResult<'a> {
        self.get_borrowed(&key, frame, target)
    }

    pub fn set(&self, key: Rc<String>, value: RuntimeValue<'a>) {
        RefMut::map(self.inner.borrow_mut(), |f| {
            let inner = f.properties.get_or_insert_with(HashMap::new);
            inner.insert(key, Property::Value(value));
            inner
        });
    }

    pub fn set_indexed(&self, key: usize, value: RuntimeValue<'a>) {
        let mut object = self.inner.borrow_mut();

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

    pub(crate) fn define_property(
        &self,
        key: Rc<String>,
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
    ) {
        RefMut::map(self.inner.borrow_mut(), |f| {
            let inner = f.properties.get_or_insert_with(HashMap::new);
            inner.insert(key, Property::Complex { getter, setter });
            inner
        });
    }

    fn get_borrowed<'b, 'c>(
        &self,
        key: &Rc<String>,
        thread: &'c mut JsThread<'a>,
        target: &RuntimeValue<'a>,
    ) -> JsResult<'a> {
        let b = self.inner.borrow();

        if "prototype" == key.as_ref() {
            if let Some(prototype) = &b.prototype {
                return Ok(RuntimeValue::Object(prototype.clone()));
            }
        }

        let property = self.get_property(key);

        match property {
            Some(Property::Value(value)) => Ok(value),
            // Some(Property::Complex {
            //     getter: Some(FunctionReference::Custom(CustomFunctionReference { function, .. })),
            //     ..
            // }) => function.execute(
            //     None,
            //     &mut Vec::new(),
            //     0..0,
            //     Some(thread.call_stack.clone()),
            //     &thread.global_this,
            //     target,
            // ),
            Some(Property::Complex {
                getter: Some(FunctionReference::BuiltIn(function)),
                ..
            }) => Ok(function
                .apply_return(0, thread, target)?
                .unwrap_or(RuntimeValue::Undefined)),
            _ => Ok(RuntimeValue::Undefined),
        }
    }
}

impl<'a> Default for JsObject<'a> {
    fn default() -> Self {
        JsObject::new()
    }
}

impl<'a> From<JsObject<'a>> for RuntimeValue<'a> {
    fn from(obj: JsObject<'a>) -> Self {
        RuntimeValue::Object(obj)
    }
}

impl<'a> TryInto<JsObject<'a>> for RuntimeValue<'a> {
    type Error = ExecutionError<'a>;

    fn try_into(self) -> JsResult<'a, JsObject<'a>> {
        match self {
            RuntimeValue::Object(obj) => Ok(obj),
            RuntimeValue::Function(_, obj) => Ok(obj),
            RuntimeValue::String(_, obj) => Ok(obj),
            value => {
                InternalError::new_stackless(format!("Incorrect object type {:?}", value)).into()
            }
        }
    }
}

impl<'a> PartialEq for JsObject<'a> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Property<'a> {
    Value(RuntimeValue<'a>),
    Complex {
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
    },
}

impl<'a> DebugRepresentation for JsObject<'a> {
    fn render(&self, renderer: &mut Renderer) -> std::fmt::Result {
        let value = self.inner.borrow();

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
