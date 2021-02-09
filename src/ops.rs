use crate::value::{FunctionReference, RuntimeValue, StaticValue};
use crate::vm::Function;
use log::trace;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Debug)]
pub enum ControlFlow<'a> {
    Step,
    Return(RuntimeValue<'a>),
    Call {
        function: FunctionReference<'a>,
        with_args: usize,
    },
    Jump {
        chunk_index: usize,
    },
}

#[derive(Debug)]
pub(crate) struct RuntimeFrame<'a, 'b> {
    pub(crate) context: Rc<RefCell<Context<'a>>>,
    pub(crate) stack: &'b mut Vec<RuntimeValue<'a>>,
    pub(crate) function: &'a Function,
}

pub struct Context<'a> {
    locals: Vec<RuntimeValue<'a>>,
    parent: Option<Rc<RefCell<Context<'a>>>>,
}

impl<'a> Debug for Context<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Context[{:p}, size={:?}, locals={:?}]",
            self,
            self.locals.len(),
            self.locals
        ))
    }
}

impl<'a> Context<'a> {
    pub(crate) fn with_parent(
        parent: Option<Rc<RefCell<Context<'a>>>>,
        locals_size: usize,
    ) -> Rc<RefCell<Context<'a>>> {
        Rc::new(RefCell::new(Context {
            locals: vec![RuntimeValue::Undefined; locals_size],
            parent,
        }))
    }
}

pub trait ContextAccess<'a> {
    fn read(&self, index: usize) -> RuntimeValue<'a>;
    fn write(&self, index: usize, value: RuntimeValue<'a>);
    fn capture(&self, offset: usize, index: usize) -> Option<RuntimeValue<'a>>;
}

impl<'a> ContextAccess<'a> for Rc<RefCell<Context<'a>>> {
    fn read(&self, index: usize) -> RuntimeValue<'a> {
        self.borrow().locals[index].clone()
    }

    fn write(&self, index: usize, value: RuntimeValue<'a>) {
        self.borrow_mut().locals[index] = value
    }

    fn capture(&self, offset: usize, index: usize) -> Option<RuntimeValue<'a>> {
        if offset > 0 {
            self.borrow()
                .parent
                .as_ref()
                .and_then(|parent| parent.capture(offset - 1, index))
        } else {
            self.borrow().locals.get(index).cloned()
        }
    }
}

pub(crate) struct Instruction {
    pub(crate) instr:
        for<'a, 'b, 'c> fn(&Option<StaticValue>, &'b mut RuntimeFrame<'a, 'c>) -> ControlFlow<'a>,
    pub(crate) constant: Option<StaticValue>,
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self.instr {
            i if i as usize == bind as usize => "bind",
            i if i as usize == truncate as usize => "truncate",
            i if i as usize == add as usize => "add",
            i if i as usize == sub as usize => "sub",
            i if i as usize == div as usize => "div",
            i if i as usize == modulo as usize => "modulo",
            i if i as usize == mul as usize => "mul",
            i if i as usize == gt as usize => "gt",
            i if i as usize == gte as usize => "gte",
            i if i as usize == lt as usize => "lte",
            // i if i as usize == eq as usize => "eq",
            i if i as usize == load as usize => "load",
            i if i as usize == ret as usize => "ret",
            i if i as usize == call as usize => "call",
            i if i as usize == jmp as usize => "jmp",
            i if i as usize == cjmp as usize => "cjmp",
            i if i as usize == set as usize => "set",
            i if i as usize == get as usize => "get",
            _ => panic!("Unknown instruction"),
        };

        f.write_fmt(format_args!("{} {:?}", name, self.constant))
    }
}

macro_rules! next {
    ($e: expr, step) => {
        ControlFlow::Step
    };
    ($e: expr, ret) => {
        ControlFlow::Return($e)
    };
    ($e: expr, call) => {
        ControlFlow::Call($e)
    };
    ($e: expr) => {
        $e
    };
}

macro_rules! op {
    ($i: ident ($v: ident, $frame: ident) $b: block $rr:ident) => (pub(crate) fn $i <'a, 'b, 'c>($v: &Option<StaticValue>, $frame: &'c mut RuntimeFrame<'a, 'b>) -> ControlFlow<'a> {
        let _rr = $b;

        trace!("{}({:?}) [stack: {:?} locals: {:?}]", stringify!($i), $v, $frame.stack, $frame.context.borrow());

        next!(_rr, $rr)
    });

    ($i: ident ($v: ident, $frame: ident) $b: block) => (pub(crate) fn $i <'a, 'b, 'c>($v: &Option<StaticValue>, $frame: &'c mut RuntimeFrame<'a, 'b>) -> ControlFlow<'a> {
        let _rr = $b;

        trace!("{}({:?}) [stack: {:?} locals: {:?}]", stringify!($i), $v, $frame.stack, $frame.context.borrow());

        _rr
    });

    ($i: ident ($frame: ident) $b: block $rr:ident) => (pub(crate) fn $i <'a, 'b, 'c>(_val: &Option<StaticValue>, $frame: &'c mut RuntimeFrame<'a, 'b>) -> ControlFlow<'a> {
        let _rr = $b;

        trace!("{} [stack: {:?} locals: {:?}]", stringify!($i), $frame.stack, $frame.context.borrow());

        next!(_rr, $rr)
    });

    ($i: ident $b: block $rr: ident) => (pub(crate) fn $i <'a, 'b, 'c>(val: &Option<StaticValue>, frame: &'c mut RuntimeFrame<'a, 'b>) -> ControlFlow<'a> {
        $b

        next!(_rr, $rr)
    });
}

macro_rules! numeric_op {
    ($i:ident($l:ident, $r:ident) => $e: expr) => (op!($i(frame) {
        let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
        let r_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

        // let l_value = l_ref.resolve();
        let l_prim = match l_ref {
            RuntimeValue::Local(index) => frame.context.read(index),
            v => v,
        };

        let r_prim = match r_ref {
            RuntimeValue::Local(index) => frame.context.read(index),
            v => v,
        };

        // no strings yet

        let $r: f64 = l_prim.into();
        let $l: f64 = r_prim.into();

        frame.stack.push(RuntimeValue::Float($e));
    } step););
}

macro_rules! numeric_comparison_op {
    ($i:ident($l:ident, $r:ident) => $e: expr) => (op!($i(frame) {
        let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
        let r_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

        // let l_value = l_ref.resolve();
        let l_prim = match l_ref {
            RuntimeValue::Local(index) => frame.context.read(index),
            v => v,
        };

        let r_prim = match r_ref {
            RuntimeValue::Local(index) => frame.context.read(index),
            v => v,
        };

        // no strings yet

        let $r: f64 = l_prim.into();
        let $l: f64 = r_prim.into();

        frame.stack.push(RuntimeValue::Boolean($e));
    } step););
}

numeric_op!(add(l, r) => l + r);
numeric_op!(sub(l, r) => l - r);
numeric_op!(div(l, r) => l / r);
numeric_op!(modulo(l, r) => l % r);
numeric_op!(mul(l, r) => l * r);

numeric_comparison_op!(gt(l, r) => l > r);
numeric_comparison_op!(gte(l, r) => l >= r);
numeric_comparison_op!(lt(l, r) => l < r);
numeric_comparison_op!(lte(l, r) => l <= r);

op!(load(val, frame) {
   let value = val.as_ref()
    .expect("Expected a value to be present for load")
    .to_runtime(frame);

   frame.stack.push(value)
} step);

op!(bind(val, frame) {
    let value = frame.stack.pop().expect("Expect a value to be present for assign");
    if let Some(StaticValue::Local(variable)) = val {
        frame.context.write(*variable, value);
    } else {
        panic!("Bind value was incorrect {:?}", val);
    }
} step);

op!(ret(val, frame) {
    if let Some(return_value) = val {
        return_value.to_runtime(frame)
    } else {
        frame.stack.pop().expect("Expect a value to be present for return")
    }
} ret);

op!(call(val, frame) {
    let function = frame.stack.pop()
        .and_then(|f| match f {
            RuntimeValue::Function(function) => {
                Some(function)
            },
            _ => None
        })
    .expect("Expect a value to be present for call");



    if let Some(StaticValue::Float(count)) = val {
        let v = count.trunc() as usize;

        ControlFlow::Call {
            function,
            with_args: v as usize
        }
    } else {
        panic!("Invalid call operator")
    }
});

op!(truncate(frame) {
    frame.stack.truncate(0);
} step);

op!(cjmp(val, frame) {
    let conditional = frame.stack.pop().expect("Expected a value to be present for conditional jump");

    let should_jump: bool = conditional.into();

    if let Some(StaticValue::Branch(left, right)) = val {
        let next = if should_jump {
            *left
         } else {
            *right
        };

        ControlFlow::Jump { chunk_index: next }
    } else {
        panic!("Cannot jump to block {:?}", val)
    }
});

op!(jmp(val, frame) {
    if let Some(StaticValue::Jump(left)) = val {
            ControlFlow::Jump { chunk_index: *left }
        } else {
            panic!("Cannot jump to block {:?}", val)
        }
});

op!(set(val, frame) {
    let value = frame.stack.pop().expect("Need a value");

    let attribute = val.clone().map(
        |value| value.to_runtime(frame)
    ).or_else(|| {
        frame.stack.pop()
    }).expect("Need an attribute");

    let target = frame.stack.pop().expect("Need an target");

    match (target, attribute) {
        (RuntimeValue::Object(mut obj), RuntimeValue::String(str)) => {
            obj.borrow_mut().insert(str, value);
            frame.stack.push(RuntimeValue::Object(obj));
        },
        _ => {}
    };
    
} step);

op!(get(val, frame) {
    let attribute = val.clone().map(
        |value| value.to_runtime(frame)
    ).or_else(|| {
        frame.stack.pop()
    }).expect("Need an attribute");

    let target = frame.stack.pop().expect("Need an target");

    if let (RuntimeValue::Object(obj), RuntimeValue::String(str)) = (target, attribute) {
        frame.stack.push(obj.borrow().get(&str).cloned().expect("TypeError: attribute does not exist"));
    };
    
} step);

op!(get_null_safe(val, frame) {
    let attribute = val.clone().map(
        |value| value.to_runtime(frame)
    ).or_else(|| {
        frame.stack.pop()
    }).expect("Need an attribute");

    let target = frame.stack.pop().expect("Need an target");

    match (target, attribute) {
        (RuntimeValue::Object(mut obj), RuntimeValue::String(str)) => {
            frame.stack.push(obj.borrow().get(&str).cloned().unwrap_or(RuntimeValue::Undefined));
        },
        _ => {}
    };
    
} step);
