use std::borrow::Cow;

use rcc_arena as arena;

mod builder;

pub use builder::TyBuilder;

use crate::SymbolId;

#[derive(Debug, Clone, Copy)]
pub struct Type<'a> {
    pub alias: Option<SymbolId>,
    pub canonical: CanonicalType<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalType<'a> {
    Function(&'a CanonicalFunctionType<'a>),
    Int,
    Void,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CanonicalFunctionType<'a> {
    pub ret: CanonicalType<'a>,
    pub params: arena::Vec<'a, CanonicalType<'a>>,
}

impl<'a> Type<'a> {
    #[inline]
    pub fn void() -> Type<'a> {
        Type {
            alias: None,
            canonical: CanonicalType::Void,
        }
    }

    #[inline]
    pub fn int() -> Type<'a> {
        Type {
            alias: None,
            canonical: CanonicalType::Int,
        }
    }
}

pub fn display_canonical_type(ty: &CanonicalType) -> Cow<'static, str> {
    match ty {
        CanonicalType::Function(ty) => display_canonical_function_type(ty),
        CanonicalType::Int => Cow::Borrowed("int"),
        CanonicalType::Void => Cow::Borrowed("void"),
    }
}

fn display_canonical_function_type(ty: &CanonicalFunctionType<'_>) -> Cow<'static, str> {
    let ret = display_canonical_type(&ty.ret);
    let params = ty
        .params
        .iter()
        .map(|param| display_canonical_type(param))
        .collect::<Vec<_>>()
        .join(", ");

    Cow::Owned(format!("{ret}({params})"))
}
