use std::marker::PhantomData;

pub trait IntoArena<'arena>: Sized {
    type Indexed;

    fn to_ref(indexed: Self::Indexed, children: &'arena [Self]) -> Self;
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub struct Id<T>(u32, PhantomData<fn() -> T>);

impl<T> Id<T> {
    fn new(index: usize) -> Self {
        Self(index.try_into().unwrap(), PhantomData)
    }

    pub fn index(&self) -> usize {
        self.0.try_into().unwrap()
    }

    pub fn to_ref<'ast, U: IntoArena<'ast, Indexed = T>>(self) -> Id<U> {
        Id::new(self.index())
    }
}

#[derive(Clone)]
pub struct IndexedArena<T> {
    vec: Vec<T>,
}

impl<T> IndexedArena<T> {
    pub fn new() -> Self {
        Self { vec: vec![] }
    }

    pub fn push(&mut self, val: T) -> Id<T> {
        self.vec.push(val);
        Id::new(self.vec.len() - 1)
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T> Default for IndexedArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct Arena<T> {
    vec: Vec<T>,
}

impl<'ast, T: IntoArena<'ast> + 'ast> Arena<T> {
    pub fn from_indexed(indexed: IndexedArena<T::Indexed>) -> Self {
        let mut vec = Vec::with_capacity(indexed.len());
        for indexed in indexed.vec {
            // SAFETY:
            // - Lifetime transmute, so the types are fully compatible
            // - The lifetime is bound to 'ast, so it cannot outlive the arena
            // - Elements cannot be removed and the vec cannot be resized, so references will not
            //   be invalidated
            let children = unsafe { std::mem::transmute::<&[T], &'ast [T]>(&vec[..]) };
            let value = T::to_ref(indexed, children);
            vec.push(value);
        }
        Self { vec }
    }

    pub fn get(&self, id: Id<T>) -> &T {
        &self.vec[id.index()]
    }

    /// # SAFETY:
    /// - This slice and any references derived from it must be dropped before this arena is dropped.
    pub unsafe fn slice(&self) -> &'ast [T] {
        unsafe { std::mem::transmute::<&[T], &'ast [T]>(&self.vec) }
    }
}

impl<T> Drop for Arena<T> {
    fn drop(&mut self) {
        for elem in std::mem::take(&mut self.vec).into_iter().rev() {
            drop(elem);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Id;

    enum E<'a> {
        U,
        T(&'a E<'a>),
    }

    enum I {
        U,
        T(Id<I>),
    }
}
