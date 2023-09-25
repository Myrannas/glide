use crate::compiler::locals::Resolution::Resolved;
use crate::parser::ast::Expression;
use instruction_set::{Local, LocalInit};

#[derive(Debug)]
pub(crate) enum Resolution {
    Resolved { local: usize },
    Capture { frame: usize, local: usize },
    Unresolved,
}

#[derive(Debug)]
pub(crate) struct LocalAllocator<'a> {
    pub(crate) locals: Vec<Local>,
    pub(crate) init: Vec<Option<LocalInit>>,
    pub(crate) current_id: usize,
    pub(crate) parent: Option<Box<&'a Self>>,
}

impl<'a> LocalAllocator<'a> {
    pub(crate) fn resolve_identifier(&self, id: &str) -> Resolution {
        for local in &self.locals {
            if local.name == id {
                return Resolved { local: local.local };
            }
        }

        if let Some(parent) = &self.parent {
            match parent.resolve_identifier(id) {
                Resolution::Resolved { local } => Resolution::Capture { local, frame: 1 },
                Resolution::Capture { local, frame } => Resolution::Capture {
                    local,
                    frame: frame + 1,
                },
                Resolution::Unresolved => Resolution::Unresolved,
            }
        } else {
            Resolution::Unresolved
        }
    }

    pub(crate) fn allocate_identifier(&mut self, id: impl Into<String>, argument: bool) -> usize {
        let local = self.current_id;
        self.current_id += 1;

        self.locals.push(Local {
            name: id.into(),
            local,
            frame: 0,
            argument,
        });

        self.init.push(None);

        local
    }

    pub(crate) fn new_root() -> Self {
        LocalAllocator {
            current_id: 0,
            locals: vec![],
            init: vec![],
            parent: None,
        }
    }

    pub(crate) fn child(&'a self) -> Self {
        LocalAllocator {
            locals: vec![],
            current_id: 0,
            init: vec![],
            parent: Some(Box::new(self)),
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.locals
            .iter()
            .filter(|Local { frame, .. }| *frame == 0)
            .count()
    }

    pub(crate) fn args_len(&self) -> usize {
        self.locals
            .iter()
            .filter(
                |Local {
                     frame, argument, ..
                 }| *frame == 0 && *argument,
            )
            .count()
    }

    pub(crate) fn set_init_value(
        &mut self,
        local: usize,
        expression: &Expression,
        atoms: &mut Vec<String>,
    ) -> bool {
        let constant = if let Some(constant) = expression.to_constant(atoms) {
            constant
        } else {
            return false;
        };

        if self.init.get(local).unwrap_or(&None).is_none() {
            self.init[local] = Some(LocalInit::Constant(constant));

            true
        } else {
            false
        }
    }

    pub(crate) fn set_init_function(&mut self, local: usize, function_id: usize) -> bool {
        if self.init.get(local).is_none() {
            self.init[local] = Some(LocalInit::Function(function_id));
            true
        } else {
            false
        }
    }

    pub(crate) fn get_init(&self) -> Vec<LocalInit> {
        self.init
            .iter()
            .map(|v| v.clone().unwrap_or_default())
            .collect()
    }
}
