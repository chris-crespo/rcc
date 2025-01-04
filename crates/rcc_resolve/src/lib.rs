use rcc_ast::Program;
use rcc_context::GlobalContext;

mod diagnostics;
mod labels;
mod loops;

pub(crate) struct ResolutionContext<'a, 'src> {
    gcx: &'a GlobalContext<'src>,
    errors: Vec<miette::Report>,
}

impl<'a, 'src> ResolutionContext<'a, 'src> {
    fn new(gcx: &'a GlobalContext<'src>) -> ResolutionContext<'a, 'src> {
        ResolutionContext {
            gcx,
            errors: Vec::new(),
        }
    }

    #[inline(always)]
    fn error(&mut self, error: miette::Report) {
        self.errors.push(error)
    }
}

pub struct ResolutionResult {
    pub errors: Vec<miette::Report>,
}

pub fn resolve<'a>(gcx: &'a GlobalContext<'_>, program: &'a Program) -> ResolutionResult {
    let mut rcx = ResolutionContext::new(gcx);
    // symbols::resolve(&mut rcx, program);
    labels::resolve(&mut rcx, program);
    loops::resolve(&mut rcx, program);

    ResolutionResult { errors: rcx.errors }
}
