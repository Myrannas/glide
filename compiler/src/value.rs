use crate::debugging::{DebugRepresentation, Renderer};
use crate::object::JsObject;
use crate::ops::JsContext;
use crate::result::ExecutionError;
use crate::result::JsResult;
use crate::vm::{Function, JsThread};
use crate::InternalError;
use std::fmt::{Debug, Display, Formatter};
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
        Renderer::debug(f, 5).render(self)
    }
}

impl DebugRepresentation for StaticValue {
    fn render(&self, renderer: &mut Renderer) -> std::fmt::Result {
        match self {
            StaticValue::Undefined => renderer.literal("undefined")?,
            StaticValue::Null => renderer.literal("null")?,
            StaticValue::Boolean(true) => renderer.literal("true")?,
            StaticValue::Boolean(false) => renderer.literal("false")?,
            StaticValue::Float(value) => renderer.literal(&value.to_string())?,
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
        };

        Ok(())
    }
}

impl<'a> StaticValue {
    pub(crate) fn to_runtime<'c>(&self, frame: &'c mut JsThread<'a>) -> RuntimeValue<'a> {
        match self {
            StaticValue::Undefined => RuntimeValue::Undefined,
            StaticValue::Null => RuntimeValue::Null,
            StaticValue::Boolean(v) => RuntimeValue::Boolean(*v),
            StaticValue::Float(f) => RuntimeValue::Float(*f),
            StaticValue::String(s) => RuntimeValue::String(Rc::new(s.to_owned())),
            StaticValue::Local(l) => RuntimeValue::Internal(InternalValue::Local(*l)),
            StaticValue::Function(f) => {
                let function = frame.current_function();
                let child_function = function.child_function(*f).clone();

                frame
                    .global_this
                    .wrappers
                    .wrap_function(FunctionReference::Custom(CustomFunctionReference {
                        function: child_function,
                        context: frame.current_context().clone(),
                    }))
                    .into()
            }
            StaticValue::Capture {
                frame: offset,
                local,
            } => frame
                .current_context()
                .capture(*offset, *local)
                .expect("Expected capture to work"),
            StaticValue::Branch(left, right) => {
                RuntimeValue::Internal(InternalValue::Branch(*left, *right))
            }
            StaticValue::Jump(left) => RuntimeValue::Internal(InternalValue::Jump(*left)),
            StaticValue::Object => JsObject::new().into(),
            StaticValue::GlobalThis => RuntimeValue::Object(frame.global_this.global_this.clone()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionReference<'a> {
    Custom(CustomFunctionReference<'a>),
    BuiltIn(BuiltIn<'a>),
}

#[derive(Clone, Debug)]
pub struct CustomFunctionReference<'a> {
    pub context: JsContext<'a>,
    pub function: Function,
}

impl<'a> PartialEq for CustomFunctionReference<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function && self.context == other.context
    }
}

#[derive(Clone)]
pub struct BuiltIn<'a> {
    pub(crate) op: BuiltinFn<'a>,

    // Option<Box<..>> to prevent infinite sized RuntimeValues
    pub(crate) context: Option<Box<RuntimeValue<'a>>>,
}

impl<'a> PartialEq for BuiltIn<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.op as usize == other.op as usize && self.context == other.context
    }
}

impl<'a> From<BuiltIn<'a>> for FunctionReference<'a> {
    fn from(builtin: BuiltIn<'a>) -> Self {
        FunctionReference::BuiltIn(builtin)
    }
}

impl<'a> From<Option<RuntimeValue<'a>>> for RuntimeValue<'a> {
    fn from(value: Option<RuntimeValue<'a>>) -> Self {
        match value {
            Some(value) => value,
            None => RuntimeValue::Undefined,
        }
    }
}

impl<'a> BuiltIn<'a> {
    pub(crate) fn apply(
        &self,
        arguments: usize,
        thread: &mut JsThread<'a>,
        target: Option<JsObject<'a>>,
    ) {
        let context = match &self.context {
            Some(value) => Some(value.as_ref()),
            None => None,
        };

        // let required_arg_count = self.desired_args.min(8);
        // let mut args: [Option<RuntimeValue<'a>>; 8] = Default::default();
        //
        // for i in (0..required_arg_count).rev() {
        //     if i < args_count {
        //         args[i] = frame.stack.pop();
        //     } else {
        //         args[i] = None;
        //     }
        // }

        let target = target.unwrap_or(thread.global_this.global_this.clone());
        let result = (self.op)(arguments, thread, &target, context);
        thread.stack.truncate(thread.stack.len() - arguments);

        match result {
            Ok(Some(result)) => thread.stack.push(result),
            Err(err) => {
                thread.throw(err);
            }
            _ => (),
        };
    }

    pub(crate) fn apply_return(
        &self,
        arguments: usize,
        thread: &mut JsThread<'a>,
        target: Option<JsObject<'a>>,
    ) -> JsResult<'a, Option<RuntimeValue<'a>>> {
        let context = match &self.context {
            Some(value) => Some(value.as_ref()),
            None => None,
        };

        let target = target.unwrap_or(thread.global_this.global_this.clone());
        let result = (self.op)(arguments, thread, &target, context);
        thread.stack.truncate(thread.stack.len() - arguments);
        result
    }
}

pub(crate) type BuiltinFn<'a> = fn(
    arguments: usize,
    frame: &mut JsThread<'a>,
    target: &JsObject<'a>,
    context: Option<&RuntimeValue<'a>>,
) -> JsResult<'a, Option<RuntimeValue<'a>>>;

impl<'a> Debug for BuiltIn<'a> {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

#[derive(Clone)]
pub enum RuntimeValue<'a> {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(Rc<String>),
    Object(JsObject<'a>),
    Reference(Reference<'a>),
    Internal(InternalValue),
}

impl<'a> Default for RuntimeValue<'a> {
    fn default() -> Self {
        RuntimeValue::Undefined
    }
}

impl<'a> Default for &RuntimeValue<'a> {
    fn default() -> Self {
        &RuntimeValue::Undefined
    }
}

impl<'a> RuntimeValue<'a> {
    pub(crate) fn strict_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeValue::Undefined, RuntimeValue::Undefined) => true,
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::String(b1), RuntimeValue::String(b2)) => b1.as_ref() == b2.as_ref(),
            (RuntimeValue::Float(b1), RuntimeValue::Float(b2)) => b1 == b2,
            (RuntimeValue::Object(b1), RuntimeValue::Object(b2)) => b1 == b2,
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            _ => false,
        }
    }

    pub(crate) fn non_strict_eq(&self, other: &Self, frame: &mut JsThread<'a>) -> bool {
        match (self, other) {
            (RuntimeValue::Undefined, RuntimeValue::Undefined) => true,
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::String(b1), RuntimeValue::String(b2)) => b1.eq(b2),
            (RuntimeValue::String(b1), RuntimeValue::Float(f))
                if b1.as_ref() == "" && *f as u32 == 0 =>
            {
                true
            }
            (RuntimeValue::String(b1), RuntimeValue::Boolean(false)) if b1.as_ref() == "" => true,
            (RuntimeValue::Float(b1), RuntimeValue::Float(b2)) => b1 == b2,
            (RuntimeValue::Object(b1), RuntimeValue::Object(b2)) => b1 == b2,
            (RuntimeValue::String(s1), RuntimeValue::Object(b1)) => {
                let string_value1 = b1.get_wrapped_value().unwrap().to_string(frame).unwrap();

                s1.eq(&string_value1)
            }
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            _ => false,
        }
    }

    pub(crate) fn as_object(&self) -> JsResult<'a, &JsObject<'a>> {
        match self {
            RuntimeValue::Object(obj) => Ok(obj),
            _ => InternalError::new_stackless("Unable to use as object").into(),
        }
    }
}

impl<'a> PartialEq for RuntimeValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.strict_eq(other)
    }
}

impl<'a> RuntimeValue<'a> {
    pub(crate) fn resolve<'c>(
        self,
        thread: &'c mut JsThread<'a>,
    ) -> Result<Self, ExecutionError<'a>> {
        let result = match self.clone() {
            RuntimeValue::Internal(InternalValue::Local(index)) => {
                Ok(thread.current_context().read(index))
            }
            RuntimeValue::Reference(reference) => match *reference.base {
                RuntimeValue::Object(obj) => obj.get(reference.name, thread),
                RuntimeValue::String(value) => thread
                    .global_this
                    .wrappers
                    .wrap_string(value)
                    .get(reference.name, thread),
                RuntimeValue::Undefined => {
                    let error: RuntimeValue<'a> = thread.global_this.errors.new_type_error(
                        format!("Cannot read property {} of undefined", reference.name),
                    );

                    Err(error.into())
                }
                value => todo!("Unsupported type {:?}", value),
            },
            other => Ok(other),
        }?;

        Ok(result)
    }

    pub(crate) fn update_reference(
        self,
        frame: &mut JsThread<'a>,
        value: RuntimeValue<'a>,
    ) -> JsResult<'a, ()> {
        match self {
            RuntimeValue::Internal(InternalValue::Local(index)) => {
                frame.current_context().write(index, value);
                Ok(())
            }
            RuntimeValue::Reference(reference) => match *reference.base {
                RuntimeValue::Object(obj) => {
                    obj.set(reference.name, value);
                    Ok(())
                }
                _ => Ok(()),
            },
            value => InternalError::new_stackless(format!("Unable to update - {:?}", value)).into(),
        }
    }

    pub(crate) fn to_string(&self, thread: &mut JsThread<'a>) -> JsResult<'a, Rc<String>> {
        let result = match self {
            RuntimeValue::Float(v) => v.to_string(),
            RuntimeValue::Boolean(bool) => bool.to_string(),
            RuntimeValue::String(str) => str.as_ref().to_owned(),
            RuntimeValue::Undefined => "undefined".to_owned(),
            RuntimeValue::Null => "null".to_owned(),
            RuntimeValue::Object(obj) => {
                let to_string = obj.get(Rc::new("toString".to_owned()), thread)?;

                println!("{:?}\n{:?}", obj, to_string);

                return match to_string {
                    RuntimeValue::Object(function) if function.is_callable() => thread
                        .call_from_native(obj.clone(), function.get_callable().unwrap(), 0, false)?
                        .to_string(thread),
                    RuntimeValue::Undefined => Ok(Rc::new("[Object object]".to_owned())),
                    _ => {
                        let error = thread
                            .global_this
                            .errors
                            .new_type_error("Cannot convert object to primitive value");

                        return Err(error.into());
                    }
                };
            }
            value => todo!("Unsupported types {:?}", value),
        };

        Ok(Rc::new(result))
    }

    pub(crate) fn to_object(&self, thread: &mut JsThread<'a>) -> JsObject<'a> {
        match self {
            RuntimeValue::String(str) => thread.global_this.wrappers.wrap_string(str.clone()),
            RuntimeValue::Object(obj) => obj.clone(),
            // RuntimeValue::Und(_, obj) => obj.clone(),
            _ => panic!(":( {:?}", self),
        }
    }
}

impl<'a> Debug for RuntimeValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Renderer::debug(f, 5).render(self)
    }
}

impl<'a> Display for RuntimeValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Renderer::compact(f).render(self)
    }
}

pub(crate) fn make_arguments(arguments: Vec<RuntimeValue>) -> RuntimeValue {
    JsObject::new()
        .with_name("[Arguments]")
        .with_indexed_properties(arguments)
        .into()
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

impl<'a> From<f64> for RuntimeValue<'a> {
    fn from(value: f64) -> Self {
        RuntimeValue::Float(value)
    }
}

impl<'a> From<bool> for RuntimeValue<'a> {
    fn from(value: bool) -> Self {
        RuntimeValue::Boolean(value)
    }
}

impl<'a> From<RuntimeValue<'a>> for bool {
    fn from(value: RuntimeValue<'a>) -> Self {
        (&value).into()
    }
}

impl<'a> From<Rc<String>> for RuntimeValue<'a> {
    fn from(value: Rc<String>) -> Self {
        RuntimeValue::String(value)
    }
}

impl<'a> From<String> for RuntimeValue<'a> {
    fn from(value: String) -> Self {
        RuntimeValue::String(Rc::new(value))
    }
}

impl<'a> From<&RuntimeValue<'a>> for bool {
    fn from(value: &RuntimeValue<'a>) -> Self {
        match value {
            RuntimeValue::Float(v) => *v > 0.0,
            RuntimeValue::Boolean(bool) => *bool,
            RuntimeValue::String(str) if str.as_ref().eq("undefined") => false,
            RuntimeValue::String(str) if str.as_ref().eq("") => false,
            RuntimeValue::String(..) => true,
            RuntimeValue::Undefined => false,
            RuntimeValue::Null => false,
            RuntimeValue::Object(..) => true,
            value => todo!("Unsupported types {:?}", value),
        }
    }
}

// impl<'a> From<RuntimeValue<'a>> for String {
//     fn from(value: RuntimeValue<'a>) -> Self {
//         (&value).into()
//     }
// }
//
// impl<'a> From<&RuntimeValue<'a>> for String {
//     fn from(value: &RuntimeValue<'a>) -> Self {
//         match value {
//             RuntimeValue::Float(v) => v.to_string(),
//             RuntimeValue::Boolean(bool) => bool.to_string(),
//             RuntimeValue::String(str, ..) => str.as_ref().to_owned(),
//             RuntimeValue::Undefined => "undefined".to_owned(),
//             RuntimeValue::Null => "null".to_owned(),
//             RuntimeValue::Object(_) => "[object Object]".to_owned(),
//             value => todo!("Unsupported types {:?}", value),
//         }
//     }
// }
