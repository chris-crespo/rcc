use rcc_ast::Program;
use rcc_interner::Interner;

mod diagnostics;
mod labels;

pub(crate) struct ResolutionContext<'a, 'src> {
    interner: &'a Interner<'src>,
    errors: Vec<miette::Report>,
}

impl<'a, 'src> ResolutionContext<'a, 'src> {
    #[inline(always)]
    fn error(&mut self, error: miette::Report) {
        self.errors.push(error)
    }
}

impl<'a, 'src> ResolutionContext<'a, 'src> {
    fn new(interner: &'a Interner<'src>) -> ResolutionContext<'a, 'src> {
        ResolutionContext {
            interner,
            errors: Vec::new(),
        }
    }
}

pub struct ResolutionResult {
    pub errors: Vec<miette::Report>,
}

pub fn resolve<'a>(interner: &'a Interner, program: &'a Program) -> ResolutionResult {
    let mut cx = ResolutionContext::new(interner);
    labels::resolve(&mut cx, program);

    ResolutionResult { errors: cx.errors }
}
