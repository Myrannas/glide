use crate::value::{RuntimeValue, StaticValue};
use crate::vm::Function;
use nom::lib::std::fmt::Formatter;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug)]
pub enum Next<'a> {
    Step,
    Return(RuntimeValue<'a>),
    Call(RuntimeValue<'a>),
}

#[derive(Debug)]
pub(crate) struct RuntimeFrame<'a> {
    pub(crate) context: Rc<RefCell<Context<'a>>>,
    pub(crate) stack: Vec<RuntimeValue<'a>>,
    pub(crate) function: &'a Function,
}

pub struct Context<'a> {
    locals: Vec<RuntimeValue<'a>>,
    parent: Option<Rc<RefCell<Context<'a>>>>,
}

impl<'a> Debug for Context<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Context[{:p}, size={:?}]",
            self,
            self.locals.len()
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
    pub(crate) instr: for<'a, 'b> fn(&Option<StaticValue>, &'b mut RuntimeFrame<'a>) -> Next<'a>,
    pub(crate) constant: Option<StaticValue>,
}

macro_rules! next {
    ($e: expr) => {
        Next::Step
    };
    ($e: expr, step) => {
        Next::Step
    };
    ($e: expr, ret) => {
        Next::Return($e)
    };
    ($e: expr, call) => {
        Next::Call($e)
    };
}

macro_rules! op {
    ($i: ident ($v: ident, $frame: ident) $b: block  $($rr:ident)?) => (pub(crate) fn $i <'a, 'b>($v: &Option<StaticValue>, $frame: &'b mut RuntimeFrame<'a>) -> Next<'a> {
        let _rr = $b;

        println!("{}({:?}) [{:?}]", stringify!($i), $v, $frame);

        next!(_rr $(, $rr)?)
    });

    ($i: ident ($frame: ident) $b: block $($rr:ident)?) => (pub(crate) fn $i <'a, 'b>(_val: &Option<StaticValue>, $frame: &'b mut RuntimeFrame<'a>) -> Next<'a> {
        let _rr = $b;

        println!("{} [{:?}]", stringify!($i), $frame);

        next!(_rr $(, $rr)?)
    });

    ($i: ident $b: block $($rr:ident)?) => (pub(crate) fn $i <'a, 'b>(val: &Option<StaticValue>, frame: &'b mut RuntimeFrame<'a>) -> Next<'a> {
        $b

        next!(_rr $(, $rr)?)
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

        let $l: f64 = l_prim.into();
        let $r: f64 = r_prim.into();

        frame.stack.push(RuntimeValue::Float($e));
    }););
}

numeric_op!(add(l, r) => l + r);
numeric_op!(sub(l, r) => l - r);
numeric_op!(div(l, r) => l / r);
numeric_op!(mul(l, r) => l * r);

op!(load(val, frame) {
   let value = val.as_ref()
    .expect("Expected a value to be present for load")
    .to_runtime(frame);

   frame.stack.push(value)
});

op!(bind(val, frame) {
    let value = frame.stack.pop().expect("Expect a value to be present for assign");
    if let Some(StaticValue::Local(variable)) = val {
        frame.context.write(*variable, value);
    } else {
        panic!("Bind value was incorrect {:?}", val);
    }
});

op!(ret(val, frame) {
    if let Some(return_value) = val {
        return_value.to_runtime(frame)
    } else {
        frame.stack.pop().expect("Expect a value to be present for return")
    }
} ret);

op!(call(frame) {
    frame.stack.pop().expect("Expect a value to be present for call")
} call);

op!(truncate(frame) {
    frame.stack.truncate(0);
});
