use std::ops::{Deref, DerefMut};

use allocator_api2::vec;
use bumpalo::Bump;

use crate::Arena;

#[derive(Debug)]
pub struct Vec<'a, T>(vec::Vec<T, &'a Bump>);

impl<'a, T> Vec<'a, T> {
    pub fn new_in(arena: &'a Arena) -> Vec<'a, T> {
        Vec(vec::Vec::new_in(arena))
    }
}

impl<'a, T> Deref for Vec<'a, T> {
    type Target = vec::Vec<T, &'a Bump>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> DerefMut for Vec<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
