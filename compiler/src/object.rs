use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::result::JsResult;
use crate::value::{FunctionReference, RuntimeValue};
use crate::vm::JsThread;
use std::cell::{RefCell, RefMut};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Write;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum JsPrimitive {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(Rc<String>),
}

impl<'a> From<JsPrimitive> for RuntimeValue<'a> {
    fn from(value: JsPrimitive) -> Self {
        match value {
            JsPrimitive::Undefined => RuntimeValue::Undefined,
            JsPrimitive::Null => RuntimeValue::Null,
            JsPrimitive::Boolean(value) => RuntimeValue::Boolean(value),
            JsPrimitive::Float(value) => RuntimeValue::Float(value),
            JsPrimitive::String(str) => RuntimeValue::String(str),
        }
    }
}

impl<'a> From<RuntimeValue<'a>> for JsPrimitive {
    fn from(value: RuntimeValue<'a>) -> Self {
        match value {
            RuntimeValue::Undefined => JsPrimitive::Undefined,
            RuntimeValue::Null => JsPrimitive::Null,
            RuntimeValue::Boolean(b) => JsPrimitive::Boolean(b),
            RuntimeValue::Float(f) => JsPrimitive::Float(f),
            RuntimeValue::String(str) => JsPrimitive::String(str),
            RuntimeValue::Object(_) => panic!("Cannot be wrapped"),
            RuntimeValue::Reference(_) => panic!("Cannot be wrapped"),
            RuntimeValue::Internal(_) => panic!("Cannot be wrapped"),
        }
    }
}

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
    pub(crate) wrapped: Option<JsPrimitive>,
    pub(crate) callable: Option<FunctionReference<'a>>,
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
                wrapped: None,
                callable: None,
            })),
        }
    }

    pub fn is_callable(&self) -> bool {
        self.inner.borrow().callable.is_some()
    }

    pub fn get_callable(&self) -> Option<FunctionReference<'a>> {
        self.inner.borrow().callable.as_ref().cloned()
    }

    pub fn wrapping(self, value: impl Into<JsPrimitive>) -> Self {
        self.inner.borrow_mut().wrapped = Some(value.into());
        self
    }

    pub fn wrap(&self, value: impl Into<JsPrimitive>) {
        self.inner.borrow_mut().wrapped = Some(value.into());
    }

    pub fn callable(self, value: impl Into<FunctionReference<'a>>) -> Self {
        self.inner.borrow_mut().callable = Some(value.into());
        self
    }

    pub fn get_wrapped_value(&self) -> Option<RuntimeValue<'a>> {
        let value = self.inner.borrow();
        value.wrapped.as_ref().cloned().map(|v| v.into())
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

    pub fn prototype(&self) -> Option<JsObject<'a>> {
        self.inner.borrow().prototype.clone()
    }

    pub fn get<'b, 'c>(&self, key: Rc<String>, frame: &'c mut JsThread<'a>) -> JsResult<'a> {
        self.get_borrowed(&key, frame)
    }

    pub fn set(&self, key: Rc<String>, value: impl Into<RuntimeValue<'a>>) {
        let value = value.into();

        if matches!(value, RuntimeValue::Internal(_)) {
            panic!("Can't write internal values to an object")
        }

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

    pub(crate) fn define_value(&self, key: impl Into<String>, value: impl Into<RuntimeValue<'a>>) {
        RefMut::map(self.inner.borrow_mut(), |f| {
            let inner = f.properties.get_or_insert_with(HashMap::new);
            inner.insert(Rc::new(key.into()), Property::Value(value.into()));
            inner
        });
    }

    pub(crate) fn read_simple_property(&self, key: impl Into<String>) -> RuntimeValue<'a> {
        match self
            .inner
            .borrow()
            .properties
            .as_ref()
            .and_then(|map| map.get(&key.into()))
        {
            Some(Property::Value(value)) => value.clone(),
            _ => unreachable!(),
        }
    }

    fn get_borrowed<'b, 'c>(&self, key: &Rc<String>, thread: &'c mut JsThread<'a>) -> JsResult<'a> {
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
                .apply_return(0, thread, Some(self.clone()))?
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
