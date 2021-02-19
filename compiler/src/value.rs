use crate::ops::{Context, ContextAccess, RuntimeFrame};
use crate::vm::Function;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Reference<'a> {
    base: RuntimeValue<'a>,
    name: String,
    strict: bool,
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
            StaticValue::Function(f) => JsObject::from_function(FunctionReference {
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
            StaticValue::Object => JsObject::create(),
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
    Reference(Box<Reference<'a>>),
    Internal(InternalValue),
}

type Object<'a> = Rc<RefCell<JsObject<'a>>>;

#[derive(Clone, Debug)]
pub struct JsObject<'a> {
    value: Option<HashMap<Rc<String>, RuntimeValue<'a>>>,
    callable: Option<FunctionReference<'a>>,
}

impl<'a> JsObject<'a> {
    pub fn create() -> RuntimeValue<'a> {
        RuntimeValue::Object(Rc::new(RefCell::new(JsObject {
            value: None,
            callable: None,
        })))
    }

    pub fn from_function(function_reference: FunctionReference<'a>) -> RuntimeValue<'a> {
        RuntimeValue::Object(Rc::new(RefCell::new(JsObject {
            value: None,
            callable: Some(function_reference),
        })))
    }

    pub fn set(&mut self, key: Rc<String>, value: RuntimeValue<'a>) {
        if self.value.is_none() {
            self.value = Some(HashMap::new());
        }

        self.value.as_mut().unwrap().insert(key, value);
    }

    pub fn get(&self, key: Rc<String>) -> Option<RuntimeValue<'a>> {
        self.value.as_ref().and_then(|m| m.get(&key)).cloned()
    }

    pub fn get_callable(&self) -> &Option<FunctionReference<'a>> {
        &self.callable
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
        match value {
            RuntimeValue::Undefined => f64::NAN,
            RuntimeValue::Null => 0.0,
            RuntimeValue::Boolean(true) => 1.0,
            RuntimeValue::Boolean(false) => 0.0,
            RuntimeValue::Float(v) => v,
            RuntimeValue::Reference(..) => todo!("References are not supported"),
            RuntimeValue::Object(..) => f64::NAN,
            RuntimeValue::String(value) => value.parse().unwrap_or(f64::NAN),
            RuntimeValue::Internal(..) => panic!("Can't convert a local runtime value to a number"),
        }
    }
}

impl<'a> From<RuntimeValue<'a>> for bool {
    fn from(value: RuntimeValue<'a>) -> Self {
        match value {
            RuntimeValue::Float(v) => v > 0.0,
            RuntimeValue::Boolean(bool) => bool,
            value => todo!("Unsupported types {:?}", value),
        }
    }
}
