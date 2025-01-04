use bitflags::bitflags;
use index_vec::IndexVec;
use rcc_span::Span;

use crate::{scope::ScopeId, Type};

index_vec::define_index_type! {
    pub struct SymbolId = u32;
}

index_vec::define_index_type! {
    pub struct RedeclarationId = u32;
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct SymbolFlags: u8 {
        const None                = 0;

        const Definition          = 1 << 0;
        const Function            = 1 << 1;
        const Variable            = 1 << 2;
        const Typedef             = 1 << 3;
        const Parameter           = 1 << 4;

        const FunctionDeclaration = SymbolFlags::Function.bits();
        const FunctionDefinition  = SymbolFlags::Function.bits() | SymbolFlags::Definition.bits();

        const VariableDeclaration = SymbolFlags::Variable.bits();
        const VariableDefinition  = SymbolFlags::Variable.bits() | SymbolFlags::Definition.bits();

        const ParameterDeclaration = SymbolFlags::Parameter.bits();
        const ParameterDefinition  = SymbolFlags::Parameter.bits() | SymbolFlags::Definition.bits();
    }
}

impl SymbolFlags {
    #[inline]
    pub fn is_none(&self) -> bool {
        *self == SymbolFlags::None
    }

    #[inline]
    pub fn is_declaration(&self) -> bool {
        !self.is_definition()
    }

    #[inline]
    pub fn is_definition(&self) -> bool {
        self.contains(SymbolFlags::Definition)
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        self.contains(SymbolFlags::Function)
    }

    #[inline]
    pub fn is_function_definition(&self) -> bool {
        self.contains(SymbolFlags::FunctionDefinition)
    }

    #[inline]
    pub fn is_typedef(&self) -> bool {
        self.contains(SymbolFlags::Typedef)
    }

    #[inline]
    pub fn is_variable(&self) -> bool {
        self.contains(SymbolFlags::Variable)
    }

    #[inline]
    pub fn is_variable_declaration(&self) -> bool {
        self.is_variable() && !self.is_definition()
    }

    #[inline]
    pub fn is_variable_definition(&self) -> bool {
        self.contains(SymbolFlags::VariableDefinition)
    }

    #[inline]
    pub fn is_parameter(&self) -> bool {
        self.contains(SymbolFlags::Parameter)
    }

    #[inline]
    pub fn is_parameter_declaration(&self) -> bool {
        self.is_parameter() && !self.is_definition()
    } 

    #[inline]
    pub fn is_parameter_definition(&self) -> bool {
        self.contains(SymbolFlags::ParameterDefinition)
    }
}

#[derive(Debug, Default)]
pub struct SymbolTable<'ty> {
    spans: IndexVec<SymbolId, Span>,
    symbols: IndexVec<SymbolId, rcc_interner::Symbol>,
    scope_ids: IndexVec<SymbolId, ScopeId>,
    flags: IndexVec<SymbolId, SymbolFlags>,
    types: IndexVec<SymbolId, Type<'ty>>,

    redeclarations: IndexVec<SymbolId, Option<RedeclarationId>>,
    redeclaration_spans: IndexVec<RedeclarationId, Vec<Span>>,
    redeclaration_types: IndexVec<RedeclarationId, Vec<Type<'ty>>>,
}

impl<'ty> SymbolTable<'ty> {
    pub fn create_symbol(
        &mut self,
        span: Span,
        symbol: rcc_interner::Symbol,
        scope_id: ScopeId,
        flags: SymbolFlags,
        ty: Type<'ty>,
    ) -> SymbolId {
        self.spans.push(span);
        self.symbols.push(symbol);
        self.scope_ids.push(scope_id);
        self.flags.push(flags);
        self.types.push(ty)
    }

    #[inline]
    pub fn span(&self, symbol_id: SymbolId) -> Span {
        self.spans[symbol_id]
    }

    #[inline]
    pub fn set_span(&mut self, symbol_id: SymbolId, span: Span) {
        self.spans[symbol_id] = span;
    }

    #[inline]
    pub fn flags(&self, symbol_id: SymbolId) -> SymbolFlags {
        self.flags[symbol_id]
    }

    #[inline]
    pub fn flags_mut(&mut self, symbol_id: SymbolId) -> &mut SymbolFlags {
        &mut self.flags[symbol_id]
    }

    #[inline]
    pub fn set_flags(&mut self, symbol_id: SymbolId, flags: SymbolFlags) {
        self.flags[symbol_id] = flags;
    }

    #[inline]
    pub fn union_flags(&mut self, symbol_id: SymbolId, flags: SymbolFlags) {
        self.flags[symbol_id] |= flags;
    }

    #[inline]
    pub fn ty(&self, symbol_id: SymbolId) -> Type<'ty> {
        self.types[symbol_id]
    }
}
