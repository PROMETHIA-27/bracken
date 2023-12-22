use std::collections::HashMap;

use crate::bytecode::{LabelIndex, Type};

#[derive(Clone, Copy, Debug)]
pub enum ScopeItem {
    Local(u32),
    Type(Type),
}

impl ScopeItem {
    pub fn local(self) -> u32 {
        match self {
            ScopeItem::Local(local) => local,
            _ => panic!(),
        }
    }

    pub fn ty(self) -> Type {
        match self {
            ScopeItem::Type(ty) => ty,
            _ => panic!(),
        }
    }
}

pub struct Scope<'f> {
    end: LabelIndex,
    items: HashMap<&'f str, ScopeItem>,
}

impl<'f> Scope<'f> {
    pub fn new(end: LabelIndex) -> Self {
        Self {
            end,
            items: HashMap::default(),
        }
    }

    pub fn top_level_scope() -> Self {
        let mut scope = Self::new(LabelIndex::new(0));
        scope.items.insert("S4", ScopeItem::Type(Type::S4));
        scope.items.insert("F4", ScopeItem::Type(Type::F4));
        scope
    }

    pub fn end(&self) -> LabelIndex {
        self.end
    }

    pub fn items(&self) -> &HashMap<&str, ScopeItem> {
        &self.items
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
            if let Some(local) = scope.items.get(name) {
                return Some(local.local());
            }
        }
        None
    }

    pub fn add_local(&mut self, name: &'f str, local: u32) {
        self.scopes
            .last_mut()
            .unwrap()
            .items
            .insert(name, ScopeItem::Local(local));
    }

    pub fn add_type(&mut self, name: &'f str, ty: Type) {
        self.scopes
            .last_mut()
            .unwrap()
            .items
            .insert(name, ScopeItem::Type(ty));
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
