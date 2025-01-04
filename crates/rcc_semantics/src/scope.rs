use std::{collections::HashMap, thread::scope};

use index_vec::{Idx, IndexVec};
use nonmax::NonMaxU32;

use crate::symbol::SymbolId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(NonMaxU32);

impl ScopeId {
    pub const fn new(idx: u32) -> ScopeId {
        assert!(idx != u32::MAX);
        // SAFETY: We just checked `idx` is valid for `NonMaxU32`
        ScopeId(unsafe { NonMaxU32::new_unchecked(idx as u32) })
    }
}

impl Idx for ScopeId {
    fn from_usize(idx: usize) -> ScopeId {
        assert!(idx < u32::MAX as usize);
        // SAFETY: We just checked `idx` is valid for `NonMaxU32`
        ScopeId(unsafe { NonMaxU32::new_unchecked(idx as u32) })
    }

    fn index(self) -> usize {
        self.0.get() as usize
    }
}

pub(crate) type Bindings = HashMap<rcc_interner::Symbol, SymbolId>;

#[derive(Debug, Default)]
pub struct ScopeTree {
    parents: IndexVec<ScopeId, Option<ScopeId>>,
    bindings: IndexVec<ScopeId, Bindings>,
}

impl ScopeTree {
    pub const ROOT_SCOPE_ID: ScopeId = ScopeId::new(0);

    #[inline]
    pub fn root_scope_id(&self) -> ScopeId {
        ScopeTree::ROOT_SCOPE_ID
    }

    #[inline]
    pub fn create_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        self.parents.push(parent);
        self.bindings.push(Bindings::new())
    }

    pub fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = ScopeId> + '_ {
        std::iter::successors(Some(scope_id), |scope_id| self.parents[*scope_id])
    }

    #[inline]
    pub fn get_binding(&self, scope_id: ScopeId, symbol: rcc_interner::Symbol) -> Option<SymbolId> {
        self.bindings[scope_id].get(&symbol).copied()
    }

    pub fn find_binding(
        &self,
        scope_id: ScopeId,
        symbol: rcc_interner::Symbol,
    ) -> Option<SymbolId> {
        for scope_id in self.ancestors(scope_id) {
            if let Some(symbol_id) = self.bindings[scope_id].get(&symbol) {
                return Some(*symbol_id);
            }
        }

        None
    }

    pub fn add_binding(&mut self, scope_id: ScopeId, symbol: rcc_interner::Symbol, symbol_id: SymbolId) {
        self.bindings[scope_id].insert(symbol, symbol_id);
    }
}
