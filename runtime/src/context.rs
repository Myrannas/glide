use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::object_pool::ObjectPool;
use crate::primordials::Primitives;
use crate::{JsFunction, JsThread, Realm, RuntimeValue};
use std::cell::{Ref, RefCell};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct CallStack {
    pub(crate) function: JsFunction,
    pub(crate) parent: Option<Rc<CallStack>>,
}

impl<'a, 'b> Debug for JsContext<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("JsContext").finish()
    }
}

struct JsContextInner<'a> {
    locals: Vec<RuntimeValue<'a>>,
    parent: Option<JsContext<'a>>,
    this: RuntimeValue<'a>,
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

impl<'a, 'b> DebugRepresentation<'a> for JsContext<'a> {
    fn render(&self, render: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        match render.representation {
            Representation::Compact => Ok(()),
            Representation::Debug => {
                render.start_internal("CONTEXT")?;
                let inner = self.inner.borrow();

                if !inner.locals.is_empty() {
                    render.internal_key(" locals: ")?;
                }

                for value in &inner.locals {
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

struct ContextIter {
    next: Option<Rc<CallStack>>,
}

impl<'a, 'b> JsContext<'a> {
    pub(crate) fn this(&self) -> Ref<RuntimeValue<'a>> {
        Ref::map(self.inner.borrow(), |value| &value.this)
    }

    pub(crate) fn root(global_this: &Realm<'a>) -> JsContext<'a> {
        JsContext {
            inner: Rc::new(RefCell::new(JsContextInner {
                locals: vec![],
                parent: None,
                this: global_this.global_this.clone().into(),
            })),
        }
    }

    pub(crate) fn with_parent<'c>(
        realm: &mut Realm<'a>,
        parent: JsContext<'a>,
        this: RuntimeValue<'a>,
        function: &'c JsFunction,
    ) -> JsContext<'a> {
        JsContext {
            inner: Rc::new(RefCell::new(JsContextInner {
                locals: function.init(realm, &parent),
                parent: Some(parent),
                this,
            })),
        }
    }

    pub(crate) fn read(&self, index: usize) -> RuntimeValue<'a> {
        self.inner.borrow().locals[index].clone()
    }

    pub(crate) fn write(&self, index: usize, value: RuntimeValue<'a>) {
        if matches!(value, RuntimeValue::Local(_)) {
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
        }

        ctx.locals.get(index).cloned()
    }

    pub(crate) fn write_capture(&self, offset: usize, index: usize, value: RuntimeValue<'a>) {
        let mut ctx = self.inner.borrow_mut();

        if offset > 0 {
            if let Some(parent) = &ctx.parent {
                parent.write_capture(offset - 1, index, value);
            }
        } else {
            ctx.locals[index] = value
        }
    }
}

impl Iterator for ContextIter {
    type Item = Rc<CallStack>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = std::mem::replace(&mut self.next, None);

        next.map(|current| {
            self.next = (&current.parent).clone();
            current
        })
    }
}
