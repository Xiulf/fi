pub mod constraint;
pub mod layout;
pub mod subst;
pub mod tcx;
pub mod ty;

pub fn with_tcx<T>(
    reporter: &diagnostics::Reporter,
    package: &hir::Package,
    f: impl FnOnce(tcx::Tcx) -> T,
) -> T {
    let arena = bumpalo::Bump::new();
    let target = target_lexicon::Triple::host();
    let tcx = tcx::Tcx::new(reporter, &arena, &target, package);

    for (id, _) in &package.items {
        tcx.type_of(id);
    }

    tcx.unify();

    if !reporter.has_errors() {
        tcx.verify();
    }

    reporter.report(true);

    f(tcx)
}
