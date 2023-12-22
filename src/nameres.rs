use std::collections::HashMap;

use crate::bytecode::LabelIndex;

pub struct Scope<'f> {
    end: LabelIndex,
    locals: HashMap<&'f str, u32>,
}

impl<'f> Scope<'f> {
    pub fn new(end: LabelIndex) -> Self {
        Self {
            end,
            locals: HashMap::default(),
        }
    }

    pub fn end(&self) -> LabelIndex {
        self.end
    }

    pub fn locals(&self) -> &HashMap<&str, u32> {
        &self.locals
    }
}

pub struct ScopeStack<'f> {
    scopes: Vec<Scope<'f>>,
}

impl<'f> Default for ScopeStack<'f> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'f> ScopeStack<'f> {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn local(&self, name: &str) -> Option<u32> {
        for scope in self.scopes.iter().rev() {
            if let Some(&local) = scope.locals.get(name) {
                return Some(local);
            }
        }
        None
    }

    pub fn add_local(&mut self, name: &'f str, local: u32) {
        self.scopes.last_mut().unwrap().locals.insert(name, local);
    }

    pub fn top(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn push(&mut self, scope: Scope<'f>) {
        self.scopes.push(scope);
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }
}
