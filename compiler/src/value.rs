use crate::debugging::{DebugRepresentation, Renderer};
use crate::ops::{Context, ContextAccess, RuntimeFrame};
use crate::vm::Function;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Pointer, Write};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Reference<'a> {
    pub(crate) base: Box<RuntimeValue<'a>>,
    pub(crate) name: Rc<String>,
    pub(crate) strict: bool,
}

#[derive(Clone)]
pub enum StaticValue {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(String),
    Local(usize),
    Capture { frame: usize, local: usize },
    Jump(usize),
    Branch(usize, usize),
    Function(usize),
    Object,
    GlobalThis,
}

impl Debug for StaticValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Renderer::debug(f, 3).render(self)
    }
}

impl DebugRepresentation for StaticValue {
    fn render(&self, renderer: &mut Renderer) -> std::fmt::Result {
        match self {
            StaticValue::Undefined => renderer.literal("undefined")?,
            StaticValue::Null => renderer.literal("null")?,
            StaticValue::Boolean(true) => renderer.literal("true")?,
            StaticValue::Boolean(false) => renderer.literal("false")?,
            StaticValue::String(value) => renderer.string_literal(&value)?,
            StaticValue::Local(local) => {
                renderer.start_internal("LOCAL")?;
                renderer.internal_key("local")?;
                renderer.literal(&local.to_string())?;
                renderer.end_internal()?;
            }
            StaticValue::Capture { local, frame } => {
                renderer.start_internal("CAPTURE")?;
                renderer.internal_key("frame")?;
                renderer.literal(&frame.to_string())?;
                renderer.internal_key("local")?;
                renderer.literal(&local.to_string())?;
                renderer.end_internal()?;
            }
            StaticValue::Jump(jump) => {
                renderer.start_internal("JUMP")?;
                renderer.internal_key("frame")?;
                renderer.literal(&jump.to_string())?;
                renderer.end_internal()?;
            }
            StaticValue::Branch(left, right) => {
                renderer.start_internal("JUMP")?;
                renderer.internal_key("left")?;
                renderer.literal(&left.to_string())?;

                renderer.formatter.write_str(", ")?;

                renderer.internal_key("right")?;
                renderer.literal(&right.to_string())?;

                renderer.end_internal()?;
            }
            StaticValue::Function(function) => {
                renderer.start_internal("FUNCTION")?;
                renderer.internal_key("function")?;
                renderer.literal(&function.to_string())?;

                renderer.end_internal()?;
            }
            StaticValue::Object => {
                renderer.literal("{}")?;
            }
            StaticValue::GlobalThis => {
                renderer.literal("globalThis")?;
            }
            _ => {}
        };

        Ok(())
    }
}

impl<'a> StaticValue {
    pub(crate) fn to_runtime<'b>(&self, frame: &'b RuntimeFrame<'a, 'b>) -> RuntimeValue<'a> {
        match self {
            StaticValue::Undefined => RuntimeValue::Undefined,
            StaticValue::Null => RuntimeValue::Null,
            StaticValue::Boolean(v) => RuntimeValue::Boolean(*v),
            StaticValue::Float(f) => RuntimeValue::Float(*f),
            StaticValue::String(s) => RuntimeValue::String(Rc::new(s.clone())),
            StaticValue::Local(l) => frame.context.read(*l),
            StaticValue::Function(f) => Object::from_function(FunctionReference {
                function: &frame.function.functions[*f],
                context: frame.context.clone(),
            }),
            StaticValue::Capture {
                frame: offset,
                local,
            } => frame
                .context
                .capture(*offset, *local)
                .expect("Expected capture to work"),
            StaticValue::Branch(left, right) => {
                RuntimeValue::Internal(InternalValue::Branch(*left, *right))
            }
            StaticValue::Jump(left) => RuntimeValue::Internal(InternalValue::Jump(*left)),
            StaticValue::Object => Object::create(),
            StaticValue::GlobalThis => frame.global_this.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionReference<'a> {
    pub function: &'a Function,
    pub context: Rc<RefCell<Context<'a>>>,
}

#[derive(Clone)]
pub enum RuntimeValue<'a> {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(Rc<String>),
    Object(Object<'a>),
    Reference(Reference<'a>),
    Internal(InternalValue),
}

impl<'a> RuntimeValue<'a> {
    pub(crate) fn strict_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeValue::Undefined, RuntimeValue::Undefined) => true,
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::String(b1), RuntimeValue::String(b2)) => b1 == b2,
            (RuntimeValue::Float(b1), RuntimeValue::Float(b2)) => b1 == b2,
            (RuntimeValue::Object(b1), RuntimeValue::Object(b2)) => Rc::ptr_eq(b1, b2),
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            _ => false,
        }
    }

    pub(crate) fn non_strict_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeValue::Undefined, RuntimeValue::Undefined) => true,
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::String(b1), RuntimeValue::String(b2)) => b1 == b2,
            (RuntimeValue::Float(b1), RuntimeValue::Float(b2)) => b1 == b2,
            (RuntimeValue::Object(b1), RuntimeValue::Object(b2)) => Rc::ptr_eq(b1, b2),
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            _ => false,
        }
    }
}

impl<'a> RuntimeValue<'a> {
    pub(crate) fn resolve<'b>(self, context: &'b Rc<RefCell<Context<'a>>>) -> Self {
        match self {
            RuntimeValue::Internal(InternalValue::Local(index)) => context.read(index),
            RuntimeValue::Reference(reference) => match *reference.base {
                RuntimeValue::Object(obj) => obj.get(reference.name),
                _ => todo!("Unsupported type"),
            },
            other => other,
        }
    }
}

impl<'a> Debug for RuntimeValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Renderer::debug(f, 3).render(self)
    }
}

pub type Object<'a> = Rc<RefCell<JsObject<'a>>>;

#[derive(Clone, Debug)]
pub struct JsObject<'a> {
    properties: Option<HashMap<Rc<String>, RuntimeValue<'a>>>,
    callable: Option<FunctionReference<'a>>,
    name: Option<&'a str>,
}

impl<'a> DebugRepresentation for Object<'a> {
    fn render(&self, renderer: &mut Renderer) -> std::fmt::Result {
        let value = self.borrow();

        if let Some(callable) = &value.callable {
            renderer.function(&callable.function.name)?;
        }

        if let Some(properties) = &value.properties {
            renderer.formatter.write_char('{')?;

            for (k, v) in properties.iter() {
                renderer.formatter.write_str(k)?;
                renderer.formatter.write_str(": ")?;
                renderer.render(v)?;
                renderer.formatter.write_str(", ")?;
            }

            renderer.formatter.write_char('}')?;
        }

        Ok(())
    }
}

pub trait ObjectMethods<'a, 'b> {
    fn create() -> RuntimeValue<'a>;
    fn from_function(function_reference: FunctionReference<'a>) -> RuntimeValue<'a>;
    fn get(&self, key: Rc<String>) -> RuntimeValue<'a>;
    fn set(&self, key: Rc<String>, value: RuntimeValue<'a>);
    fn get_borrowed(&self, key: &Rc<String>) -> RuntimeValue<'a>;
    fn get_callable(&self) -> Option<FunctionReference<'a>>;
}

impl<'a, 'b> ObjectMethods<'a, 'b> for Object<'a> {
    fn create() -> RuntimeValue<'a> {
        RuntimeValue::Object(Rc::new(RefCell::new(JsObject {
            properties: None,
            callable: None,
            name: None,
        })))
    }

    fn from_function(function_reference: FunctionReference<'a>) -> RuntimeValue<'a> {
        RuntimeValue::Object(Rc::new(RefCell::new(JsObject {
            properties: None,
            callable: Some(function_reference),
            name: None,
        })))
    }

    fn get(&self, key: Rc<String>) -> RuntimeValue<'a> {
        self.get_borrowed(&key)
    }

    fn set(&self, key: Rc<String>, value: RuntimeValue<'a>) {
        RefMut::map(self.borrow_mut(), |f| {
            let inner = f.properties.get_or_insert_with(HashMap::new);
            inner.insert(key, value);
            inner
        });
    }

    fn get_borrowed(&self, key: &Rc<String>) -> RuntimeValue<'a> {
        let b = self.borrow();

        b.properties
            .as_ref()
            .and_then(|p| p.get(key).cloned())
            .unwrap_or(RuntimeValue::Undefined)
    }

    fn get_callable(&self) -> Option<FunctionReference<'a>> {
        self.borrow().callable.clone()
    }
}

#[derive(Clone, Debug)]
pub enum InternalValue {
    Local(usize),
    Branch(usize, usize),
    Jump(usize),
}

impl<'a> From<RuntimeValue<'a>> for f64 {
    fn from(value: RuntimeValue) -> Self {
        (&value).into()
    }
}

impl<'a> From<&RuntimeValue<'a>> for f64 {
    fn from(value: &RuntimeValue) -> Self {
        match value {
            RuntimeValue::Undefined => f64::NAN,
            RuntimeValue::Null => 0.0,
            RuntimeValue::Boolean(true) => 1.0,
            RuntimeValue::Boolean(false) => 0.0,
            RuntimeValue::Float(v) => *v,
            RuntimeValue::Reference(..) => todo!("References are not supported"),
            RuntimeValue::Object(..) => f64::NAN,
            RuntimeValue::String(value) => value.parse().unwrap_or(f64::NAN),
            RuntimeValue::Internal(..) => panic!("Can't convert a local runtime value to a number"),
        }
    }
}

impl<'a> From<RuntimeValue<'a>> for bool {
    fn from(value: RuntimeValue<'a>) -> Self {
        (&value).into()
    }
}

impl<'a> From<&RuntimeValue<'a>> for bool {
    fn from(value: &RuntimeValue<'a>) -> Self {
        match value {
            RuntimeValue::Float(v) => *v > 0.0,
            RuntimeValue::Boolean(bool) => *bool,
            RuntimeValue::String(str) if str.as_ref().eq("undefined") => false,
            RuntimeValue::String(..) => true,
            RuntimeValue::Undefined => false,
            value => todo!("Unsupported types {:?}", value),
        }
    }
}
