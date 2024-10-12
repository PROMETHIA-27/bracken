use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Default)]
pub struct Db {
    typemap: HashMap<TypeId, Box<dyn Any>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Id<T>(u32, PhantomData<fn() -> T>);

impl<T> Id<T> {
    pub fn new(index: u32) -> Self {
        Self(index, PhantomData)
    }
}

impl Db {
    pub fn insert<T: 'static>(&mut self, value: T) -> Id<T> {
        let vec = self
            .typemap
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(Vec::<T>::new()))
            .downcast_mut::<Vec<T>>()
            .unwrap();

        let index = vec.len();
        vec.push(value);

        Id::new(index.try_into().unwrap())
    }

    pub fn get<T: 'static>(&self, id: Id<T>) -> Option<&T> {
        let vec = self
            .typemap
            .get(&TypeId::of::<T>())?
            .downcast_ref::<Vec<T>>()
            .unwrap();

        vec.get::<usize>(id.0.try_into().unwrap())
    }
}
