use std::ops::Deref;

use rcc_arena::Arena;
use rcc_interner::Interner;

pub struct GlobalContext<'src> {
    pub arenas: GlobalArenas<'src>,
    pub interner: Interner<'src>,
}

impl<'src> GlobalContext<'src> {
    pub fn new(ast: &'src AstArena, ty: &'src TyArena) -> GlobalContext<'src> {
        GlobalContext {
            arenas: GlobalArenas::new(ast, ty),
            interner: Interner::new(),
        }
    }
}

pub struct GlobalArenas<'src> {
    pub ast: &'src AstArena,
    pub ty: &'src TyArena,
}

impl<'ast> GlobalArenas<'ast> {
    pub fn new(ast: &'ast AstArena, ty: &'ast TyArena) -> GlobalArenas<'ast> {
        GlobalArenas { ast, ty }
    }
}

pub struct AstArena(Arena);

impl AstArena {
    pub fn new() -> AstArena {
        AstArena(Arena::new())
    }
}

impl Deref for AstArena {
    type Target = Arena;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct TyArena(Arena);

impl TyArena {
    pub fn new() -> TyArena {
        TyArena(Arena::new())
    }
}

impl Deref for TyArena {
    type Target = Arena;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
