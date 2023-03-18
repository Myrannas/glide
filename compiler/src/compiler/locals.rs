use crate::parser::ast::Expression;
use instruction_set::{Local, LocalInit};

pub(crate) enum Resolution {
    Resolved { local: usize },
    Capture { frame: usize, local: usize },
    Unresolved,
}

#[derive(Debug)]
pub(crate) struct LocalAllocator {
    pub(crate) locals: Vec<Local>,
    pub(crate) init: Vec<Option<LocalInit>>,
    pub(crate) current_id: usize,
}

impl LocalAllocator {
    pub(crate) fn resolve_identifier(&self, id: &str) -> Resolution {
        let result = self
            .locals
            .iter()
            .rev()
            .find(|Local { name, .. }| name == id);

        match result {
            Some(Local {
                local, frame: 0, ..
            }) => Resolution::Resolved { local: *local },
            Some(Local { local, frame, .. }) => Resolution::Capture {
                local: *local,
                frame: *frame,
            },
            None => Resolution::Unresolved,
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
        }
    }

    pub(crate) fn child(&self) -> Self {
        let locals: Vec<Local> = self
            .locals
            .iter()
            .map(
                |Local {
                     name,
                     local,
                     frame,
                     argument,
                 }| Local {
                    name: name.to_owned(),
                    local: *local,
                    frame: frame + 1,
                    argument: *argument,
                },
            )
            .collect();

        LocalAllocator {
            locals,
            current_id: 0,
            init: vec![],
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
