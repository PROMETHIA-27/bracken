use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::fmt::Debug;
use std::marker::PhantomData;

use crate::ast::{Expr, FunctionDef, Spanned};

pub struct Db {
    typemap: HashMap<TypeId, Box<dyn Any>>,
}

impl Db {
    pub fn register<T: ?Sized + Storable>(mut self) -> Self {
        self.typemap
            .insert(TypeId::of::<T>(), Box::new(T::Store::default()));
        self
    }

    const FORGOT_REGISTER: &str = "forgot to register store for type";

    pub fn insert<T: ?Sized + Storable>(&mut self, value: T::Input<'_>) -> Id<T> {
        let store = self
            .typemap
            .get_mut(&TypeId::of::<T>())
            .expect(Self::FORGOT_REGISTER)
            .downcast_mut::<T::Store>()
            .unwrap();
        T::insert(value, store)
    }

    pub fn get<T: ?Sized + Storable>(&self, id: Id<T>) -> &T {
        let store = self
            .typemap
            .get(&TypeId::of::<T>())
            .expect(Self::FORGOT_REGISTER)
            .downcast_ref()
            .unwrap();
        T::get(id, store)
    }

    pub fn get_mut<T: Storable>(&mut self, id: Id<T>) -> &mut T {
        let store = self
            .typemap
            .get_mut(&TypeId::of::<T>())
            .expect(Self::FORGOT_REGISTER)
            .downcast_mut()
            .unwrap();
        T::get_mut(id, store)
    }

    pub fn empty_vec<T>(&self) -> Id<Vec<T>>
    where
        Vec<T>: Storable,
    {
        Id::new(0)
    }
}

impl Default for Db {
    fn default() -> Self {
        let mut db = Db {
            typemap: Default::default(),
        }
        .register::<str>()
        .register::<Expr>()
        .register::<Vec<Id<Expr>>>()
        .register::<Spanned<FunctionDef>>();

        db.insert::<Vec<Id<Expr>>>(vec![]);

        db
    }
}

pub trait DebugWithContext<Ctx> {
    fn fmt(&self, ctx: &Ctx, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;

    fn with<'ctx, 't>(&'t self, ctx: &'ctx Ctx) -> ContextDebugger<'ctx, 't, Ctx, Self> {
        ContextDebugger { ctx, value: self }
    }
}

pub struct ContextDebugger<'ctx, 't, C, T: ?Sized + DebugWithContext<C>> {
    pub ctx: &'ctx C,
    pub value: &'t T,
}

impl<C> DebugWithContext<C> for str {
    fn fmt(&self, _: &C, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self)
    }
}

impl<C, T: DebugWithContext<C>> std::fmt::Debug for ContextDebugger<'_, '_, C, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(self.ctx, f)
    }
}

impl<T: ?Sized + DebugWithContext<Db> + Storable> DebugWithContext<Db> for Id<T> {
    fn fmt(&self, ctx: &Db, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ctx.get(*self).fmt(ctx, f)
    }
}

impl<C, T: DebugWithContext<C>> DebugWithContext<C> for Vec<T> {
    fn fmt(&self, ctx: &C, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for elem in self.iter() {
            list.entry(&ContextDebugger { ctx, value: elem });
        }
        list.finish()
    }
}

impl<C, T: DebugWithContext<C>> DebugWithContext<C> for Option<T> {
    fn fmt(&self, ctx: &C, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Some(val) => f.debug_tuple("Some").field(&val.with(ctx)).finish(),
            None => f.debug_struct("None").finish(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Id<T: ?Sized>(u32, PhantomData<fn() -> T>);

impl<T: ?Sized> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for Id<T> {}

impl<T: ?Sized> Id<T> {
    pub fn new(index: usize) -> Self {
        Self(index.try_into().unwrap(), PhantomData)
    }

    pub fn index(self) -> usize {
        usize::try_from(self.0).unwrap()
    }
}

pub trait Storable: 'static {
    type Store: Default;

    type Input<'a>;

    fn get(id: Id<Self>, store: &Self::Store) -> &Self;

    fn get_mut(id: Id<Self>, store: &mut Self::Store) -> &mut Self;

    fn insert(value: Self::Input<'_>, store: &mut Self::Store) -> Id<Self>;
}

pub trait ArenaStored {}

impl<T: 'static + ArenaStored> Storable for T {
    type Store = Vec<T>;
    type Input<'a> = Self;

    fn get(id: Id<Self>, store: &Self::Store) -> &Self {
        store.get(usize::try_from(id.0).unwrap()).unwrap()
    }

    fn get_mut(id: Id<Self>, store: &mut Self::Store) -> &mut Self {
        store.get_mut(usize::try_from(id.0).unwrap()).unwrap()
    }

    fn insert(value: T, store: &mut Self::Store) -> Id<Self> {
        let index = store.len();
        store.push(value);
        Id::new(index)
    }
}
