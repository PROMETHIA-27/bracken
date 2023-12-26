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
}

impl<T> Default for Arena<T> {
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
