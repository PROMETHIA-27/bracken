use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;
use std::num::NonZeroU32;

pub struct Id<T>(NonZeroU32, PhantomData<fn() -> T>);

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Id<T> {}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Id").field(&self.0).finish()
    }
}

impl<T> std::hash::Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Id<T> {
    fn new(index: usize) -> Self {
        let index: u32 = index.try_into().unwrap();
        let index = match index.checked_add(1) {
            Some(i) => i,
            None => panic!("too many Ids allocated"),
        };
        Self(NonZeroU32::new(index).unwrap(), PhantomData)
    }

    pub fn index(&self) -> usize {
        <u32 as TryInto<usize>>::try_into(self.0.get()).unwrap() - 1
    }
}

#[derive(Clone)]
pub struct CellArena<T> {
    vec: RefCell<Arena<T>>,
}

impl<T> CellArena<T> {
    pub fn new() -> Self {
        Self {
            vec: RefCell::new(Arena::new()),
        }
    }

    pub fn push(&self, val: T) -> Id<T> {
        self.vec.borrow_mut().push(val)
    }

    pub fn len(&self) -> usize {
        self.vec.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn into_inner(self) -> Arena<T> {
        self.vec.into_inner()
    }

    pub fn take(&self) -> Arena<T> {
        self.vec.take()
    }
}

impl<T> Default for CellArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct Arena<T> {
    // blocks: Vec<(usize, Box<[MaybeUninit<T>]>)>,
    vec: Vec<T>,
}

impl<T> Arena<T> {
    // TODO: Mess around with block size once a good benchmark(s) is available
    // const BLOCK_SIZE: usize = 128;

    pub fn new() -> Self {
        Self {
            // blocks: vec![Self::new_block()],
            vec: vec![],
        }
    }

    // fn new_block() -> (usize, Box<[MaybeUninit<T>]>) {
    //     (
    //         0,
    //         std::iter::repeat(MaybeUninit::uninit())
    //             .take(Self::BLOCK_SIZE)
    //             .collect(),
    //     )
    // }

    // pub fn len(&self) -> usize {
    //     let filled_count = (self.blocks.len() - 1) * Self::BLOCK_SIZE;
    //     let last_count = self.blocks.last().unwrap().0;
    //     filled_count + last_count
    // }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// # Safety:
    /// - If `value` contains references into this arena, they must be to previous elements in the arena
    ///   (they must have been inserted before this one)
    pub fn push(&mut self, value: T) -> Id<T> {
        // let (index, block) = self.get_open_block_mut();
        // block[*index] = MaybeUninit::new(value);
        // let id = Id::new(*index);
        // *index += 1;
        // id
        self.vec.push(value);
        self.last()
    }

    // fn get_open_block_mut(&mut self) -> &mut (usize, Box<[MaybeUninit<T>]>) {
    //     let block = self.blocks.last_mut().unwrap();

    //     if block.0 == Self::BLOCK_SIZE {
    //         self.blocks.push(Self::new_block());
    //     }

    //     self.blocks.last_mut().unwrap()
    // }

    // // # Safety:
    // // - This element cannot be given any references to elements inserted after it
    // pub fn get(&self, id: Id<T>) -> Option<&T> {
    //     let index = id.index();
    //     let block_index = index / Self::BLOCK_SIZE;
    //     let index = index % Self::BLOCK_SIZE;

    //     let block = self.blocks.get(block_index)?;
    //     if index >= block.0 {
    //         return None;
    //     }

    //     let value = &block.1[index];
    //     // SAFETY:
    //     // - All values with index < length of the block are init and always will be
    //     let value = unsafe { value.assume_init_ref() };
    //     Some(value)
    // }

    pub fn get(&self, id: Id<T>) -> &T {
        self.vec.get(id.index()).expect("arena/id mismatch")
    }

    pub fn last(&self) -> Id<T> {
        Id::new(self.vec.len() - 1)
    }

    pub fn ids(&self) -> impl Iterator<Item = Id<T>> {
        (0..self.len()).map(Id::new)
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ExtendArena<T, E> {
    items: Vec<E>,
    _marker: PhantomData<fn() -> T>,
}

impl<T, E: Clone> Clone for ExtendArena<T, E> {
    fn clone(&self) -> Self {
        Self {
            items: self.items.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T, E> ExtendArena<T, E> {
    pub fn new() -> Self {
        Self {
            items: vec![],
            _marker: PhantomData,
        }
    }

    pub fn get_no_resize(&self, id: Id<T>) -> Option<&E> {
        self.items.get(id.index())
    }

    pub fn map<R>(self, mapping: impl Fn(E) -> R) -> ExtendArena<T, R> {
        ExtendArena {
            items: self.items.into_iter().map(mapping).collect(),
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T, E: Default> ExtendArena<T, E> {
    fn reserve(&mut self, id: Id<T>) {
        if id.index() >= self.len() {
            self.items.resize_with(id.index() + 1, E::default);
        }
    }

    pub fn get(&mut self, id: Id<T>) -> &E {
        self.reserve(id);
        &self.items[id.index()]
    }

    pub fn get_mut(&mut self, id: Id<T>) -> &mut E {
        self.reserve(id);
        &mut self.items[id.index()]
    }

    pub fn set(&mut self, id: Id<T>, value: E) {
        self.reserve(id);
        self.items[id.index()] = value;
    }
}

impl<T, E> Default for ExtendArena<T, E> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct CellExtendArena<T, E> {
    arena: RefCell<ExtendArena<T, E>>,
}

impl<T, E> CellExtendArena<T, E> {
    pub fn new() -> Self {
        Self {
            arena: RefCell::new(ExtendArena::new()),
        }
    }

    pub fn take(&self) -> ExtendArena<T, E> {
        self.arena.take()
    }
}

impl<T, E: Default> CellExtendArena<T, E> {
    pub fn set(&self, id: Id<T>, val: E) {
        self.arena.borrow_mut().set(id, val)
    }
}

impl<T, E> Default for CellExtendArena<T, E> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct Interner<T: Clone + Hash + Eq> {
    items: Vec<T>,
    lookup: HashMap<T, usize>,
}

impl<T: Clone + Hash + Eq> Interner<T> {
    pub fn new() -> Self {
        Interner {
            items: vec![],
            lookup: HashMap::new(),
        }
    }

    pub fn add(&mut self, value: T) -> Id<T> {
        match self.lookup.get(&value) {
            Some(index) => Id::new(*index),
            None => {
                let index = self.items.len();
                self.items.push(value.clone());
                self.lookup.insert(value, index);
                Id::new(index)
            }
        }
    }

    pub fn get(&self, id: Id<T>) -> &T {
        self.items.get(id.index()).expect("interner/id mismatch")
    }

    pub fn get_id<Q>(&self, value: &Q) -> Option<Id<T>>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.lookup.get(value).copied().map(Id::new)
    }
}

impl<T: Clone + Hash + Eq> Default for Interner<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CellInterner<T: Clone + Hash + Eq> {
    interner: RefCell<Interner<T>>,
}

impl<T: Clone + Hash + Eq> CellInterner<T> {
    pub fn new() -> Self {
        Self {
            interner: RefCell::new(Interner::new()),
        }
    }

    pub fn add(&self, value: T) -> Id<T> {
        self.interner.borrow_mut().add(value)
    }

    pub fn take(&self) -> Interner<T> {
        self.interner.take()
    }
}

impl<T: Clone + Hash + Eq> Default for CellInterner<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::Arena;

    #[allow(clippy::assertions_on_constants)]
    #[test]
    fn arena_len() {
        let mut arena = Arena::new();
        for i in 0..257 {
            arena.push(i);
        }

        // make sure length exceeds block size to test multiple blocks
        // assert!(257 > Arena::<i32>::BLOCK_SIZE);
        assert_eq!(arena.len(), 257);
    }
}
