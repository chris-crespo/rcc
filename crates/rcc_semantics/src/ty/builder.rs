use rcc_arena as arena;
use rcc_context::TyArena;

use super::{CanonicalFunctionType, CanonicalType, Type};

pub struct TyBuilder<'a> {
    arena: &'a TyArena,
}

impl<'a> TyBuilder<'a> {
    pub fn new(arena: &'a TyArena) -> TyBuilder<'a> {
        TyBuilder { arena }
    }

    #[inline]
    fn alloc<T>(&self, value: T) -> &'a T {
        self.arena.alloc(value)
    }

    #[inline]
    pub fn vec<T>(&self) -> rcc_arena::Vec<'a, T> {
        rcc_arena::Vec::new_in(self.arena)
    }

    pub fn func(
        &self,
        ret: CanonicalType<'a>,
        params: arena::Vec<'a, CanonicalType<'a>>,
    ) -> Type<'a> {
        let canonical_ty = self.canonical_type_func(ret, params);
        Type {
            alias: None,
            canonical: canonical_ty,
        }
    }

    pub fn canonical_type_func(
        &self,
        ret: CanonicalType<'a>,
        params: arena::Vec<'a, CanonicalType<'a>>,
    ) -> CanonicalType<'a> {
        let cannonical_func = self.cannonical_func(ret, params);
        CanonicalType::Function(self.alloc(cannonical_func))
    }

    #[inline]
    pub fn cannonical_func(
        &self,
        ret: CanonicalType<'a>,
        params: arena::Vec<'a, CanonicalType<'a>>,
    ) -> CanonicalFunctionType<'a> {
        CanonicalFunctionType { ret, params }
    }
}
