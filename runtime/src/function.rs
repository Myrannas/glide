use crate::context::JsContext;
use crate::debugging::{DebugRepresentation, Renderer};
use crate::result::JsResult;
use crate::values::object::JsObject;
use crate::values::string::JsPrimitiveString;
use crate::values::value::RuntimeValue;
use crate::vm::JsThread;
use core::cmp::PartialEq;
use core::convert::From;
use core::fmt::{Debug, Formatter};
use core::option::Option;
use core::option::Option::{None, Some};
use core::result::Result::{Err, Ok};
use instruction_set::{Chunk, Function, Local};
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
    pub fn apply(&self, arguments: usize, thread: &mut JsThread<'a>, target: Option<JsObject<'a>>) {
        let context = match &self.context {
            Some(value) => Some(value.as_ref()),
            None => None,
        };

        let target = target.unwrap_or_else(|| thread.global_this.global_this.clone());
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

        let target = target.unwrap_or_else(|| thread.global_this.global_this.clone());
        let result = (self.op)(arguments, thread, &target, context);
        thread.stack.truncate(thread.stack.len() - arguments);
        result
    }
}

pub type BuiltinFn<'a> = fn(
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
    pub chunks: Vec<Chunk>,
    pub atoms: Vec<JsPrimitiveString>,
    pub functions: Vec<JsFunction>,

    pub local_size: usize,
    pub locals: Vec<Local>,

    #[allow(dead_code)]
    pub(crate) stack_size: usize,
    pub(crate) name: String,
}

impl Debug for JsFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.render(&mut Renderer::debug(f, 3))
    }
}

impl DebugRepresentation for JsFunction {
    fn render(&self, f: &mut Renderer) -> std::fmt::Result {
        f.function(&self.inner.name)?;

        f.start_internal("FUNCTION")?;

        f.internal_key("instructions")?;

        f.new_line()?;

        for (i, chunk) in self.inner.chunks.iter().enumerate() {
            f.internal_index(i)?;
            f.new_line()?;

            for instruction in &chunk.instructions {
                f.formatter.write_fmt(format_args!("{:?}", instruction))?;
                f.new_line()?;
            }
        }

        f.end_internal()?;

        Ok(())
    }
}

impl PartialEq for JsFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl JsFunction {
    pub fn load(function: Function) -> JsFunction {
        let Function {
            chunks,
            atoms,
            functions,
            stack_size,
            name,
            local_size,
            locals,
        } = function;

        let atoms = atoms.into_iter().map(JsPrimitiveString::from).collect();

        let functions = functions.into_iter().map(JsFunction::load).collect();

        JsFunction {
            inner: Rc::new(JsFunctionInner {
                chunks,
                atoms,
                functions,
                local_size,
                locals,
                stack_size,
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

    pub(crate) fn locals(&self) -> &Vec<Local> {
        &self.inner.locals
    }

    pub fn get_chunk(&self, index: usize) -> Option<&Chunk> {
        self.inner.chunks.get(index)
    }

    pub fn get_atom(&self, index: usize) -> JsPrimitiveString {
        self.inner.atoms[index].clone()
    }
}
