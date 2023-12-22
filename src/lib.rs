use std::error::Error;
use std::fmt::Display;

#[cfg(feature = "fern")]
use lalrpop_util::lalrpop_mod;
use thiserror::Error;

pub mod ast;
pub mod bytecode;
pub mod nameres;

#[cfg(feature = "fern")]
lalrpop_mod!(pub parser);

#[derive(Clone, Debug, Error)]
pub struct Errors<T: Error> {
    errs: Vec<T>,
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
        for err in &self.errs {
            f.write_fmt(format_args!("{err}"))?;
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
