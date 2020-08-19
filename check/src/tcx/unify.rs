use crate::constraint::*;
use crate::subst::*;
use crate::tcx::Tcx;
use crate::ty::*;
use diagnostics::{Diagnostic, Severity};

impl<'tcx> Tcx<'tcx> {
    pub fn unify(&self) {
        let subst = self.unify_all(&*self.constraints.borrow());

        for (_, ty) in self.types.borrow_mut().iter_mut() {
            subst.apply_ty(ty);
        }
    }

    fn unify_all(&self, cs: &Constraints<'tcx>) -> Subst<'tcx> {
        if cs.is_empty() {
            Subst::empty()
        } else {
            let mut it = cs.clone().into_iter();
            let subst = self.unify_one(it.next().unwrap());
            let subst_tail = subst.apply_cs(&it.collect());
            let subst_tail = self.unify_all(&subst_tail);

            subst.compose(subst_tail)
        }
    }

    fn unify_one(&self, cs: Constraint<'tcx>) -> Subst<'tcx> {
        match cs {
            Constraint::Equal(a, a_span, b, b_span) => match (a, b) {
                (Type::Var(tvar), _) => self.unify_var(*tvar, b),
                (_, Type::Var(tvar)) => self.unify_var(*tvar, a),
                (Type::Error, _)
                | (_, Type::Error)
                | (Type::Never, _)
                | (_, Type::Never)
                | (Type::Bool, Type::Bool)
                | (Type::Str, Type::Str)
                | (Type::TypeId, Type::TypeId) => Subst::empty(),
                (Type::Int(a), Type::Int(b)) if a == b => Subst::empty(),
                (Type::UInt(a), Type::UInt(b)) if a == b => Subst::empty(),
                (Type::Float(a), Type::Float(b)) if a == b => Subst::empty(),
                (Type::VInt(tvar), Type::Int(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::VInt(tvar), Type::VInt(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::Int(_), Type::VInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::VUInt(tvar), Type::UInt(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::VUInt(tvar), Type::VUInt(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::UInt(_), Type::VUInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::VFloat(tvar), Type::Float(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::VFloat(tvar), Type::VFloat(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::Float(_), Type::VFloat(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::Ptr(a_gc, a), Type::Ptr(b_gc, b)) if a_gc == b_gc => {
                    self.unify_one(Constraint::Equal(a, a_span, b, b_span))
                }
                (Type::Tuple(a_tys), Type::Tuple(b_tys)) => {
                    let mut cs = Constraints::new();

                    for (a, b) in a_tys.iter().zip(b_tys.iter()) {
                        cs.push(Constraint::Equal(a, a_span, b, b_span));
                    }

                    self.unify_all(&cs)
                }
                (Type::Func(a_params, a_ret), Type::Func(b_params, b_ret))
                    if a_params.len() != b_params.len() =>
                {
                    let mut cs = Constraints::new();

                    for (a, b) in a_params.iter().zip(b_params.iter()) {
                        cs.push(Constraint::Equal(a.ty, a_span, b.ty, b_span));
                    }

                    cs.push(Constraint::Equal(a_ret, a_span, b_ret, b_span));

                    self.unify_all(&cs)
                }
                (_, _) => {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0007,
                            format!("mismatched types: `{}` != `{}`", a, b),
                        )
                        .label(Severity::Error, a_span, format!("found type `{}`", a))
                        .label(
                            Severity::Info,
                            b_span,
                            format!("type `{}` specified here", b),
                        ),
                    );

                    Subst::empty()
                }
            },
            Constraint::PtrArith(a, a_span, b, b_span) => match (a, b) {
                (Type::Ptr(false, a), Type::Ptr(false, b)) => {
                    self.unify_one(Constraint::Equal(a, a_span, b, b_span))
                }
                (Type::Ptr(false, _), Type::Int(0)) | (Type::Ptr(false, _), Type::UInt(0)) => {
                    Subst::empty()
                }
                (Type::Ptr(false, _), Type::VInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::Ptr(false, _), Type::VUInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (_, _) => {
                    let mut cs = Constraints::new();

                    cs.push(Constraint::Equal(b, b_span, a, a_span));
                    cs.push(Constraint::IsNum(a, a_span));
                    cs.push(Constraint::IsNum(b, b_span));

                    self.unify_all(&cs)
                }
            },
            Constraint::IsNum(ty, span) => match ty {
                Type::VInt(_)
                | Type::VUInt(_)
                | Type::VFloat(_)
                | Type::Int(_)
                | Type::UInt(_)
                | Type::Float(_) => Subst::empty(),
                _ => {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0008,
                            format!("type `{}` is not numeric", ty),
                        )
                        .label(Severity::Error, span, None::<String>),
                    );

                    Subst::empty()
                }
            },
            Constraint::IsInt(ty, span) => match ty {
                Type::VInt(_) | Type::VUInt(_) | Type::Int(_) | Type::UInt(_) => Subst::empty(),
                _ => {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0009,
                            format!("type `{}` is not an integer", ty),
                        )
                        .label(Severity::Error, span, None::<String>),
                    );

                    Subst::empty()
                }
            },
            Constraint::Call(fn_ty, fn_span, b_params, b_ret, ret_span) => {
                if let Type::Func(a_params, a_ret) = fn_ty {
                    let mut cs = Constraints::new();

                    if a_params.len() != b_params.len() {
                        self.reporter.add(
                            Diagnostic::new(Severity::Error, 0011, format!("this function takes {} parameters, but {} arguments were supplied", a_params.len(), b_params.len()))
                                .label(Severity::Error, fn_span, None::<String>)
                        );

                        return Subst::empty();
                    }

                    let mut skip = Vec::with_capacity(a_params.len());

                    for arg in &b_params {
                        if !arg.name.symbol.is_empty() {
                            if let Some(i) = a_params
                                .iter()
                                .position(|p| p.name.symbol == arg.name.symbol)
                            {
                                let param = &a_params[i];

                                skip.push(i);
                                cs.push(Constraint::Equal(
                                    arg.ty,
                                    arg.name.span,
                                    param.ty,
                                    param.name.span,
                                ));
                            } else {
                                self.reporter.add(
                                    Diagnostic::new(
                                        Severity::Error,
                                        0012,
                                        format!("unknown named parameter '{}'", arg.name),
                                    )
                                    .label(
                                        Severity::Error,
                                        arg.name.span,
                                        None::<String>,
                                    ),
                                );
                            }
                        }
                    }

                    for arg in &b_params {
                        if arg.name.symbol.is_empty() {
                            let mut i = 0;

                            while skip.contains(&i) {
                                i += 1;
                            }

                            let param = &a_params[i];

                            skip.push(i);
                            cs.push(Constraint::Equal(
                                arg.ty,
                                arg.name.span,
                                param.ty,
                                param.name.span,
                            ));
                        }
                    }

                    cs.push(Constraint::Equal(b_ret, ret_span, a_ret, fn_span));

                    self.unify_all(&cs)
                } else if let Type::Error = fn_ty {
                    Subst::empty()
                } else {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0010,
                            format!("type `{}` is not a fuction", fn_ty),
                        )
                        .label(Severity::Error, fn_span, None::<String>),
                    );

                    Subst::empty()
                }
            }
            Constraint::Field(obj_ty, obj_span, field, ret_ty, ret_span) => {
                let fields = match obj_ty {
                    Type::TypeId => vec![
                        (
                            Ident {
                                symbol: hir::Symbol::new("size"),
                                span: obj_span,
                            },
                            self.builtin.usize,
                        ),
                        (
                            Ident {
                                symbol: hir::Symbol::new("align"),
                                span: obj_span,
                            },
                            self.builtin.usize,
                        ),
                    ],
                    _ => unimplemented!(),
                };

                if let Some(field) = fields.iter().find(|f| f.0.symbol == field.symbol) {
                    self.unify_one(Constraint::Equal(field.1, field.0.span, ret_ty, ret_span))
                } else {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0013,
                            format!("type `{}` does not have field '{}'", obj_ty, field),
                        )
                        .label(Severity::Error, field.span, None::<String>),
                    );

                    Subst::empty()
                }
            }
            _ => unimplemented!(),
        }
    }

    fn unify_var(&self, tvar: TypeVar, ty: Ty<'tcx>) -> Subst<'tcx> {
        if let Type::Var(tvar2) = ty {
            if tvar == *tvar2 {
                Subst::empty()
            } else {
                vec![(tvar, ty)].into_iter().collect()
            }
        } else {
            vec![(tvar, ty)].into_iter().collect()
        }
    }
}
