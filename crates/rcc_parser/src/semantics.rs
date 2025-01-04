use rcc_semantics::{SymbolFlags, SymbolId};

use crate::{Parser, Result};

impl<'a, 'src> Parser<'a, 'src> {
    pub(crate) fn define_declared_symbol(&mut self, symbol_id: SymbolId) {
        self.semantics
            .symbols
            .union_flags(symbol_id, SymbolFlags::Definition)
    }
}
