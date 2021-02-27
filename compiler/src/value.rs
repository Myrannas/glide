use crate::ops::{Context, ContextAccess, RuntimeFrame};
use crate::vm::Function;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Reference<'a> {
    pub(crate) base: Box<RuntimeValue<'a>>,
    pub(crate) name: Rc<String>,
    pub(crate) strict: bool,
}

#[derive(Clone, Debug)]
pub enum StaticValue {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(String),
    Local(usize),
    Capture(usize, usize),
    Jump(usize),
    Branch(usize, usize),
    Function(usize),
    Object,
    GlobalThis,
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
            StaticValue::Capture(offset, index) => frame
                .context
                .capture(*offset, *index)
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

#[derive(Clone, Debug)]
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

pub type Object<'a> = Rc<RefCell<JsObject<'a>>>;

#[derive(Clone, Debug)]
pub struct JsObject<'a> {
    properties: Option<HashMap<Rc<String>, RuntimeValue<'a>>>,
    callable: Option<FunctionReference<'a>>,
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
        })))
    }

    fn from_function(function_reference: FunctionReference<'a>) -> RuntimeValue<'a> {
        RuntimeValue::Object(Rc::new(RefCell::new(JsObject {
            properties: None,
            callable: Some(function_reference),
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
