use crate::context::JsContext;
use crate::debugging::{DebugRepresentation, Renderer};
use crate::primordials::Primitives;
use crate::result::JsResult;
use crate::values::object::JsObject;
use crate::values::string::JsPrimitiveString;
use crate::values::value::RuntimeValue;
use crate::vm::JsThread;
use crate::GlobalThis;
use core::cmp::PartialEq;
use core::convert::From;
use core::fmt::{Debug, Formatter};
use core::option::Option;
use core::option::Option::{None, Some};
use core::result::Result::{Err, Ok};
use instruction_set::{Chunk, Function, Instruction, Local, LocalInit};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionReference<'a> {
    Custom(CustomFunctionReference<'a>),
    BuiltIn(BuiltIn<'a>),
}

#[derive(Clone, Debug)]
pub struct CustomFunctionReference<'a> {
    pub parent_context: JsContext<'a>,
    pub function: JsFunction,
}

impl<'a> PartialEq for CustomFunctionReference<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function && self.parent_context == other.parent_context
    }
}

#[derive(Clone)]
pub struct BuiltIn<'a> {
    pub op: BuiltinFn<'a>,
    pub context: Option<Box<RuntimeValue<'a>>>,
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

impl<'a> BuiltIn<'a> {
    pub fn apply(
        &self,
        arguments: usize,
        thread: &mut JsThread<'a>,
        target: Option<JsObject<'a>>,
        new: bool,
    ) {
        let context = match &self.context {
            Some(value) => Some(value.as_ref()),
            None => None,
        };

        let start_len = thread.stack.len() - arguments;
        let target = target.unwrap_or_else(|| thread.global_this.global_this.clone());
        let result = (self.op)(arguments, thread, &target, context, new);
        thread.stack.truncate(start_len);

        match result {
            Ok(Some(result)) => {
                if !new {
                    thread.stack.push(result)
                }
            }
            Err(err) => {
                thread.throw(err);
            }
            _ => (),
        };
    }

    pub fn apply_return(
        &self,
        arguments: usize,
        thread: &mut JsThread<'a>,
        target: Option<JsObject<'a>>,
    ) -> JsResult<'a, Option<RuntimeValue<'a>>> {
        let context = match &self.context {
            Some(value) => Some(value.as_ref()),
            None => None,
        };

        let start_len = thread.stack.len() - arguments;
        let target = target.unwrap_or_else(|| thread.global_this.global_this.clone());
        let result = (self.op)(arguments, thread, &target, context, false);
        thread.stack.truncate(start_len);
        result
    }
}

pub type BuiltinFn<'a> = fn(
    arguments: usize,
    frame: &mut JsThread<'a>,
    target: &JsObject<'a>,
    context: Option<&RuntimeValue<'a>>,
    new: bool,
) -> JsResult<'a, Option<RuntimeValue<'a>>>;

impl<'a> Debug for BuiltIn<'a> {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl From<JsFunctionInner> for JsFunction {
    fn from(inner: JsFunctionInner) -> Self {
        JsFunction {
            inner: Rc::new(inner),
        }
    }
}

#[derive(Clone)]
pub struct JsFunction {
    inner: Rc<JsFunctionInner>,
}

pub(crate) struct JsFunctionInner {
    pub instructions: Vec<Instruction>,
    pub atoms: Vec<JsPrimitiveString>,
    pub functions: Vec<JsFunction>,

    pub args_size: usize,
    pub local_size: usize,
    pub locals: Vec<Local>,
    pub locals_init: Vec<LocalInit>,

    #[allow(dead_code)]
    pub(crate) stack_size: usize,
    pub(crate) name: String,
}

impl<'a> Debug for JsFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.render(&mut Renderer::debug(f, 3))
    }
}

impl<'a> DebugRepresentation for JsFunction {
    fn render(&self, f: &mut Renderer) -> std::fmt::Result {
        f.function(&self.inner.name)?;

        f.start_internal("FUNCTION")?;

        f.internal_key("instructions")?;

        f.new_line()?;

        for instruction in &self.inner.instructions {
            f.formatter.write_fmt(format_args!("{:?}", instruction))?;
            f.new_line()?;
        }

        f.end_internal()?;

        Ok(())
    }
}

impl<'a> PartialEq for JsFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl<'a> JsFunction {
    pub fn load(function: Function) -> JsFunction {
        let Function {
            instructions,
            atoms,
            functions,
            stack_size,
            name,
            local_size,
            locals,
            locals_init,
            args_size,
        } = function;

        let atoms = atoms.into_iter().map(JsPrimitiveString::from).collect();
        let functions = functions.into_iter().map(|f| JsFunction::load(f)).collect();

        JsFunction {
            inner: Rc::new(JsFunctionInner {
                instructions,
                atoms,
                functions,
                local_size,
                args_size,
                locals,
                stack_size,
                locals_init,
                name: name.unwrap_or_default(),
            }),
        }
    }

    pub fn name(&self) -> &str {
        &self.inner.name
    }

    pub(crate) fn child_function(&self, index: usize) -> &JsFunction {
        &self.inner.functions[index]
    }

    pub fn local_size(&self) -> usize {
        self.inner.local_size
    }

    pub fn args_size(&self) -> usize {
        self.inner.args_size
    }

    pub(crate) fn locals(&self) -> &Vec<Local> {
        &self.inner.locals
    }

    pub fn get_atom(&self, index: usize) -> JsPrimitiveString {
        self.inner.atoms[index].clone()
    }

    pub(crate) fn init(
        &self,
        primitives: &Primitives<'a>,
        parent_context: &JsContext<'a>,
    ) -> Vec<RuntimeValue<'a>> {
        self.inner
            .locals_init
            .iter()
            .map(|local_init| match local_init {
                LocalInit::Constant(constant) => RuntimeValue::from(constant),
                LocalInit::Function(function_id) => {
                    let function = self.inner.functions[*function_id].clone();
                    let function_reference = CustomFunctionReference {
                        function,
                        parent_context: parent_context.clone(),
                    };

                    primitives
                        .wrap_function(FunctionReference::Custom(function_reference))
                        .into()
                }
            })
            .collect()
    }

    pub(crate) fn instructions(&self) -> &Vec<Instruction> {
        &self.inner.instructions
    }
}
