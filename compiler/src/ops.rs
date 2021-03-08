use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::object::{JsObject};
use crate::value::{FunctionReference, Reference, RuntimeValue, StaticValue};
use crate::vm::{Function, JsThread};
use crate::InternalError;
use std::cell::{Ref, RefCell};
use std::fmt::{Debug, Formatter, Write};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct CallStack {
    pub(crate) function: Function,
    pub(crate) parent: Option<Rc<CallStack>>,
}

struct JsContextInner<'a> {
    locals: Vec<RuntimeValue<'a>>,
    parent: Option<JsContext<'a>>,
    this: JsObject<'a>,
}

#[derive(Clone)]
pub struct JsContext<'a> {
    inner: Rc<RefCell<JsContextInner<'a>>>,
}

impl PartialEq for JsContext<'_> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl<'a, 'b> DebugRepresentation for JsContext<'a> {
    fn render(&self, render: &mut Renderer) -> std::fmt::Result {
        match render.representation {
            Representation::Compact => Ok(()),
            Representation::Debug => {
                render.start_internal("CONTEXT")?;
                let inner = self.inner.borrow();

                if !inner.locals.is_empty() {
                    render.internal_key(" locals: ")?;
                }

                for value in inner.locals.iter() {
                    value.render(render)?;

                    render.formatter.write_str(", ")?;
                }

                if let Some(parent) = &inner.parent {
                    render.internal_key(" parent: ")?;

                    JsContext::render(&parent, render)?;
                }

                render.end_internal()?;
                Ok(())
            }
        }
    }
}

impl<'a, 'b> Debug for JsContext<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Renderer::debug(f, 3).render(self,)
    }
}

struct ContextIter {
    next: Option<Rc<CallStack>>,
}

impl<'a, 'b> JsContext<'a> {
    pub(crate) fn this(&self) -> Ref<JsObject<'a>> {
        Ref::map(self.inner.borrow(), |value| &value.this)
    }

    pub(crate) fn with_parent(
        parent: Option<JsContext<'a>>,
        locals_size: usize,
        this: JsObject<'a>,
    ) -> JsContext<'a> {
        JsContext {
            inner: Rc::new(RefCell::new(JsContextInner {
                locals: vec![RuntimeValue::Undefined; locals_size + 1],
                parent,
                this,
            })),
        }
    }

    pub(crate) fn read(&self, index: usize) -> RuntimeValue<'a> {
        self.inner.borrow().locals[index].clone()
    }

    pub(crate) fn write(&self, index: usize, value: RuntimeValue<'a>) {
        if matches!(value, RuntimeValue::Internal(_)) {
            panic!("Can't assign an internal to the context");
        }

        self.inner.borrow_mut().locals[index] = value
    }

    pub(crate) fn capture(&self, offset: usize, index: usize) -> Option<RuntimeValue<'a>> {
        let ctx = self.inner.borrow();

        if offset > 0 {
            return ctx
                .parent
                .as_ref()
                .and_then(|parent| parent.capture(offset - 1, index));
        } else {
            ctx.locals.get(index).cloned()
        }
    }
}

impl Iterator for ContextIter {
    type Item = Rc<CallStack>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = std::mem::replace(&mut self.next, None);

        if let Some(current) = next {
            self.next = (&current.parent).clone();
            Some(current)
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub(crate) struct Instruction {
    pub(crate) instr: for<'a, 'b> fn(&Option<StaticValue>, &'b mut JsThread<'a>),
    pub(crate) constant: Option<StaticValue>,
}

impl DebugRepresentation for Instruction {
    fn render(&self, f: &mut Renderer) -> std::fmt::Result {
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
            i if i as usize == strict_eq as usize => "===",
            i if i as usize == not_strict_eq as usize => "!==",
            i if i as usize == type_of as usize => "type_of",
            i if i as usize == lnot as usize => "lnot",
            i if i as usize == neg as usize => "neg",
            i if i as usize == lt as usize => "lt",
            i if i as usize == lte as usize => "lte",
            i if i as usize == gt as usize => "gt",
            i if i as usize == gte as usize => "gte",
            i if i as usize == lor as usize => "lor",
            i if i as usize == land as usize => "land",
            i if i as usize == throw_value as usize => "throw",
            i if i as usize == load_this as usize => "load_this",
            i if i as usize == call_new as usize => "call_new",
            i if i as usize == eq as usize => "==",
            i if i as usize == ne as usize => "!=",
            i if i as usize == rshift_u as usize => ">>>",
            i if i as usize == rshift as usize => ">>",
            i if i as usize == lshift as usize => "<<",
            i if i as usize == increment as usize => "++",
            i if i as usize == catch as usize => "catch",
            i if i as usize == uncatch as usize => "uncatch",
            i if i as usize == duplicate as usize => "duplicate",
            i if i as usize == resolve as usize => "resolve",
            _ => panic!("Unknown instruction"),
        };

        if let Some(constant) = &self.constant {
            f.instruction(name)?;
            f.formatter.write_char(' ')?;
            constant.render(f)?;
        } else {
            f.instruction(name)?;
        }

        Ok(())
    }
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Renderer::debug(f, 3).render(self)
    }
}

macro_rules! op {
    ($i: ident ($v: ident, $frame: ident) $b: block step) => (pub(crate) fn $i <'a, 'b, 'c>($v: &Option<StaticValue>, $frame: &'c mut JsThread<'a>) {
        $b;

        $frame.step();
    });

    ($i: ident ($frame: ident) $b: block step) => (pub(crate) fn $i <'a, 'b, 'c>(_: &Option<StaticValue>, $frame: &'c mut JsThread<'a>) {
        $b;

        $frame.step();
    });

    ($i: ident ($v: ident, $frame: ident) $b: block) => (pub(crate) fn $i <'a, 'b, 'c>($v: &Option<StaticValue>, $frame: &'c mut JsThread<'a>) {
        $b;
    });
}

macro_rules! numeric_op {
    ($i:ident($l:ident, $r:ident) => $e: expr) => (op!($i(frame) {
        let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
        let r_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

        // let l_value = l_ref.resolve();
        let l_prim = resolve!(l_ref, frame);
        let r_prim = resolve!(r_ref, frame);

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
        let l_prim = resolve!(l_ref, frame);
        let r_prim = resolve!(r_ref, frame);

        match (l_prim, r_prim) {
            (RuntimeValue::String(s1), RuntimeValue::String(s2)) => {

                let $l: &str = s1.as_ref();
                let $r: &str = s2.as_ref();

            frame.stack.push(RuntimeValue::Boolean($e))

        }, (l_prim, r_prim) => {
            // no strings yet

                let $l: f64 = l_prim.into();
                let $r: f64 = r_prim.into();

                frame.stack.push(RuntimeValue::Boolean($e))
            }
        }
    } step); );
}

macro_rules! resolve {
    ($value:expr, $frame:ident) => (match $value.resolve($frame) {
        Ok(value) => value,
        Err(err) => {
            $frame.throw(err);
            return;
        }
    })
}

macro_rules! catch {
    ($frame: ident, $value:expr) => (match $value {
        Ok(value) => value,
        Err(err) => {
            $frame.throw(err);
            return;
        }
    })
}

op!(add(frame) {
    let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let r_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

    let l_prim = resolve!(l_ref, frame);
    let r_prim = resolve!(r_ref, frame);
    
    match l_prim {
        RuntimeValue::String(l_str, ..) => {
            let r_str = catch!(frame, r_prim.to_string(frame));
            let string = l_str.as_ref().to_owned() + &r_str; 
            
            let str = RuntimeValue::String(Rc::new(string));
            frame.stack.push(str);
        },
        other => {
            let r_val: f64 = other.into();
            let l_val: f64 = r_prim.into();

            frame.stack.push(RuntimeValue::Float(r_val + l_val));
        }
    }
} step);

numeric_op!(sub(l, r) => l - r);
numeric_op!(div(l, r) => l / r);
numeric_op!(modulo(l, r) => l % r);
numeric_op!(mul(l, r) => l * r);

fn to_uint_32(input: f64) -> u32 {
    let f_val = match input {
        f64::NAN => 0.0,
        f64::INFINITY => 0.0,
        input => input,
    };

    f_val.abs() as u32
}

fn to_int_32(input: f64) -> i32 {
    let f_val = match input {
        f64::NAN => 0.0,
        f64::INFINITY => 0.0,
        input => input,
    };

    f_val as i32
}

numeric_op!(lshift(l, r) => ((l as i32) << r as u32) as f64);
numeric_op!(rshift(l, r) => ((l as i32) >> r as u32) as f64);
numeric_op!(rshift_u(l, r) => {
    let value = to_int_32(l);
    let shift = to_uint_32(r) & 0x1F;

    (value >> shift) as f64
});

numeric_comparison_op!(gt(l, r) => l > r);
numeric_comparison_op!(gte(l, r) => l >= r);
numeric_comparison_op!(lt(l, r) => l < r);
numeric_comparison_op!(lte(l, r) => l <= r);

op!(increment(val, frame) {
    let target = frame.stack.pop().expect("Target");
    let by: f64 = val.clone().map(|v| v.to_runtime(frame)).unwrap_or_else(|| {
        frame.stack.pop().expect("By")
    }).into();
    
    let value = resolve!(target.clone(), frame);
    
    frame.stack.push(value.clone());
    
    let value: f64 = value.into();

    catch!(frame, target.update_reference(frame, RuntimeValue::Float(value + by)));
} step);

op!(strict_eq(val, frame) {
    let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let l_prim = resolve!(l_ref, frame);
    
    let r_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let r_prim = resolve!(r_ref, frame);
    
    let result = l_prim.strict_eq(&r_prim);
    // println!("{:?} === {:?} -> {}", l_prim, r_prim, result);
    
    frame.stack.push(RuntimeValue::Boolean(result));
} step);

op!(instance_of(val, frame) {
    let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let l_prim = resolve!(l_ref, frame);
    
    let r_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let r_prim = resolve!(r_ref, frame);
    
    if let RuntimeValue::Object(left) = l_prim {
        frame.stack.push(RuntimeValue::Boolean(false));
        frame.step();
    } else {
        frame.throw(
            frame.global_this.errors.new_type_error("Type error")
        )
    }
});

op!(eq(val, frame) {
 let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let l_prim = resolve!(l_ref, frame);
    
    let r_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let r_prim = resolve!(r_ref, frame);
    
    let result = l_prim.non_strict_eq(&r_prim, frame);
    // println!("{:?} == {:?} -> {}", l_prim, r_prim, result);
    
    frame.stack.push(RuntimeValue::Boolean(result));
} step);

op!(not_strict_eq(val, frame) {
    let l_ref = frame.stack.pop().expect("Stack should have at two values to use !== operator");
    let l_prim = resolve!(l_ref, frame);
    
    let r_ref = frame.stack.pop().expect("Stack should have at two values to use !== operator");
    let r_prim = resolve!(r_ref, frame);
    
    let result = !l_prim.strict_eq(&r_prim);
    // println!("{:?} === {:?} -> {}", l_prim, r_prim, result);
    
    frame.stack.push(RuntimeValue::Boolean(result));
} step);

op!(ne(val, frame) {
 let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let l_prim = resolve!(l_ref, frame);
    
    let r_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");
    let r_prim = resolve!(r_ref, frame);
    
    let result = !l_prim.non_strict_eq(&r_prim, frame);
    // println!("{:?} != {:?} -> {}", l_prim, r_prim, result);
    
    frame.stack.push(RuntimeValue::Boolean(result));
} step);

op!(lor(val, frame) {
    if let Some(StaticValue::Branch(left, right)) = val {
         let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

    let l_prim = l_ref.clone();
    let l_prim = resolve!(l_prim, frame);

    println!("{:?}", l_prim);

    let r: bool = l_prim.into();

    if r {
        frame.stack.push(l_ref);

        frame.jump(*left);
    } else {
        frame.jump(*right);
    }
    } else {
        panic!("Cannot jump to block {:?}", val)
    }
});

op!(land(val, frame) {
    if let Some(StaticValue::Branch(left, right)) = val {
         let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

    let l_prim = l_ref.clone();
    let l_prim = resolve!(l_prim, frame);

    let r: bool = l_prim.clone().into();

    if !r {
        frame.stack.push(l_prim);

        frame.jump(*left);
    } else {
        frame.jump(*right);
    }
    } else {
        panic!("Cannot jump to block {:?}", val)
    }
});

op!(lnot(val, frame) {
   let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

    let l_prim = resolve!(l_ref, frame);
    
    let r: bool = l_prim.into();
    
    frame.stack.push(RuntimeValue::Boolean(!r));
} step);

op!(neg(val, frame) {
   let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

    let l_prim = resolve!(l_ref, frame);
    
    let r: f64 = l_prim.into();
    
    frame.stack.push(RuntimeValue::Float(-r));
} step);

op!(type_of(val, frame) {
   let l_ref = frame.stack.pop().expect("Stack should have at two values to use $i operator");

    let l_prim = resolve!(l_ref, frame);
    
    let str = RuntimeValue::String(Rc::new(match l_prim {
        RuntimeValue::Boolean(_) => "boolean".to_owned(),
        RuntimeValue::Null => "null".to_owned(),
        RuntimeValue::Object(obj) if obj.is_callable()=> "function".to_owned(),
        RuntimeValue::Object(_) => "object".to_owned(),
        RuntimeValue::String(..) => "string".to_owned(),
        RuntimeValue::Float(_) => "number".to_owned(),
        RuntimeValue::Undefined => "undefined".to_owned(),
        _ => "???".to_owned()
    }));

    frame.stack.push(str);
} step);

op!(load(val, frame) {
   let value = val.as_ref()
    .expect("Expected a value to be present for load")
    .to_runtime(frame);

   frame.stack.push(value);
} step);

op!(load_this(val, frame) {
    let this = frame.current_context().this().clone();
   frame.stack.push(RuntimeValue::Object(this));
} step);

op!(bind(val, frame) {
    let value = resolve!(frame.stack.pop().expect("Expect a value to be present for assign"), frame);
    if let Some(StaticValue::Local(variable)) = val {
        frame.current_context().write(*variable, value);
    } else {
        panic!("Bind value was incorrect {:?}", val);
    }
} step);

op!(ret(val, frame) {
    let return_value = if frame.is_new() {
        frame.current_context().this().clone().into()
    } else if let Some(return_value) = val {
        return_value.to_runtime(frame)
    } else {
        frame.stack.pop().expect("Expect a value to be present for return")
    };

    frame.return_value(return_value);
});

op!(resolve(val, frame) {
    if let Some(value) = frame.stack.pop() {
        let value = resolve!(value, frame);
        frame.stack.push(value);
    };
} step);

op!(call(val, frame) {
    if let Some(StaticValue::Float(count)) = val {
        let v = count.trunc() as usize;

        let fn_value = frame.stack.pop().expect("Expect a target");

        let target = resolve!(match fn_value.clone() {
            RuntimeValue::Reference(Reference {
                base, ..
            }) => *base,
            other => other
        }, frame).to_object(frame);

        match resolve!(fn_value.clone(), frame) {
            RuntimeValue::Object(obj) if obj.is_callable() => {
                match obj.get_callable().unwrap() {
                    FunctionReference::Custom(function) => {
                        frame.call(target, function, v, false, false);
                    },
                    FunctionReference::BuiltIn(function) => {
                        function.apply(v, frame, Some(target));
                        frame.step();
                    },
                }
            },
            _ => frame.throw(frame.global_this.errors.new_type_error(format!("{} is not a function", fn_value)))
        };
    } else {
        panic!("Invalid call operator")
    }
});

op!(call_new(val, frame) {
    if let Some(StaticValue::Float(count)) = val {
        let v = count.trunc() as usize;

        let fn_value = frame.stack.pop().expect("Expect a target");

        match resolve!(fn_value, frame) {
            RuntimeValue::Object(obj) if obj.is_callable() => {
                match obj.get_callable().unwrap() {
                    FunctionReference::Custom(function) => {
                        let mut target = JsObject::new()
                            .with_name(function.function.name());

                        if let Some(prototype) = obj.prototype() {
                            target = target.with_prototype(prototype);
                        }

                        frame.call(target, function, v, true, false);
                    },
                    FunctionReference::BuiltIn(function) => {
                        let mut target = JsObject::new();

                        if let Some(prototype) = obj.prototype() {
                            target = target.with_prototype(prototype);
                        }

                        function.apply(v, frame, Some(target));
                        frame.step();
                    },
                }
            },
            v => {
                return frame.throw(InternalError::new_stackless(format!("Uncaught type error: {:?} is not a function", v)));
            }
        };
    } else {
        panic!("Invalid call operator")
    }
});

op!(duplicate(val, frame) {
    frame.stack.push(frame.stack[frame.stack.len() - 1].clone())
} step);

op!(truncate(frame) {
    // let top = frame.stack.pop();
    // frame.stack.truncate(0);
    // 
    // if let Some(top) = top {
    //     frame.stack.push(top);
    // }
} step);

op!(cjmp(val, frame) {
    let conditional = frame.stack.pop().expect("Expected a value to be present for conditional jump");

    let conditional_resolved = resolve!(conditional, frame);
    let should_jump: bool = conditional_resolved.into();

    if let Some(StaticValue::Branch(left, right)) = val {
        let next = if should_jump {
            *left
         } else {
            *right
        };

        frame.jump(next);
    } else {
        panic!("Cannot jump to block {:?}", val)
    }
});

op!(jmp(val, frame) {
    if let Some(StaticValue::Jump(left)) = val {
        frame.jump(*left);
    } else {
        panic!("Cannot jump to block {:?}", val)
    }
});

op!(catch(val, frame) {
    if let Some(StaticValue::Jump(left)) = val {
        frame.catch(*left);
    } else {
        panic!("Cannot jump to block {:?}", val)
    }
});

op!(uncatch(val, frame) {
    if let Some(StaticValue::Jump(chunk)) = val {
        frame.uncatch(*chunk);
    } else {
        panic!("Invalid uncatch")
    }
});

op!(set(val, frame) {
    let value = frame.stack.pop().expect("Need a value");
    let resolved_value = resolve!(value, frame).clone();

    let attribute = val.clone().map(
        |value| value.to_runtime(frame)
    ).or_else(|| {
        frame.stack.pop()
    }).expect("Need an attribute");

    let target = resolve!(frame.stack.pop().expect("Need an target"), frame);
    let obj: JsObject = resolve!(target, frame).to_object(frame);

    if let RuntimeValue::String(str, ..) = attribute {        
        obj.set(str, resolved_value);
        // println!("{:?} {:?}", target, obj);
    } else {
        frame.throw(InternalError::new_stackless(format!("Cannot set attribute: {:?}", attribute)))
    };
    
} step);

op!(get(val, frame) {
    let attribute = val.clone().map(
        |value| value.to_runtime(frame)
    ).or_else(|| {
        frame.stack.pop()
    }).expect("Need an attribute");

    let target = resolve!(frame.stack.pop()
        .expect("Need an target"), frame);

    if let (obj, RuntimeValue::String(str, ..)) = (target, attribute) {
        frame.stack.push(RuntimeValue::Reference(
            Reference {
                base: Box::new(obj),                
                name: str,
                strict: true
            }
        ));
    };
    
} step);

op!(get_null_safe(val, frame) {
    let attribute = val.clone().map(
        |value| value.to_runtime(frame)
    ).or_else(|| {
        frame.stack.pop()
    }).expect("Need an attribute");

    let target = frame.stack.pop().expect("Need an target");
    let target = resolve!(target, frame);

    if let (obj, RuntimeValue::String(str, ..)) = (target, attribute) {
        frame.stack.push(RuntimeValue::Reference(
            Reference {
                base: Box::new(obj.clone()),
                name: str,
                strict: false
            }
        ));
    };
    
} step);

op!(throw_value(val, frame) {
    let value = val.clone().map(
        |value| value.to_runtime(frame)
    ).or_else(|| {
        frame.stack.pop()
    }).expect("Need an attribute");

    frame.throw(value)
});


