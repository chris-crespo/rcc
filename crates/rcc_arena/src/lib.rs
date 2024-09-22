use std::ops::Deref;

use bumpalo::Bump;

pub struct Arena {
    bump: Bump,
}

impl Arena {
    pub fn new() -> Arena {
        Arena { bump: Bump::new() }
    }
}

impl Deref for Arena {
    type Target = Bump;

    fn deref(&self) -> &Self::Target {
        &self.bump
    }
}
