use std::error::Error;
use std::fmt::Display;

use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub struct Errors<T: Error> {
    errs: Vec<T>,
}

impl<T: Error> Errors<T> {
    pub fn new() -> Self {
        Self { errs: vec![] }
    }

    pub fn push(&mut self, err: T) {
        self.errs.push(err);
    }

    pub fn len(&self) -> usize {
        self.errs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn map<U: Error>(self, mapping: impl Fn(T) -> U) -> Errors<U> {
        Errors {
            errs: self.errs.into_iter().map(mapping).collect(),
        }
    }

    pub fn into<U: Error>(self) -> Errors<U>
    where
        T: Into<U>,
    {
        self.map(T::into)
    }
}

impl<T: Error> Default for Errors<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Error> IntoIterator for Errors<T> {
    type Item = T;

    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.errs.into_iter()
    }
}

impl<T: Error> Display for Errors<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.len() > 1 {
            f.write_str("\n")?;
        }
        for err in &self.errs {
            f.write_fmt(format_args!("{err}\n"))?;
        }
        Ok(())
    }
}

impl<T: Error> From<T> for Errors<T> {
    fn from(value: T) -> Self {
        Self { errs: vec![value] }
    }
}

impl<T: Error> From<Vec<T>> for Errors<T> {
    fn from(value: Vec<T>) -> Self {
        Self { errs: value }
    }
}

#[derive(Clone, Debug)]
pub struct OneOf<T>(Vec<T>);

impl<T> OneOf<T> {
    pub fn new(vec: Vec<T>) -> Option<Self> {
        if vec.is_empty() {
            None
        } else {
            Some(OneOf(vec))
        }
    }
}

impl<T: Display> Display for OneOf<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            unreachable!();
        }

        if self.0.len() == 1 {
            f.write_fmt(format_args!("{}", self.0.first().unwrap()))?;
        } else {
            f.write_str("one of ")?;
            for t in self.0.iter().take(self.0.len() - 1) {
                f.write_fmt(format_args!("{t}, "))?;
            }
            f.write_fmt(format_args!("or {}", self.0.last().unwrap()))?;
        }
        Ok(())
    }
}
