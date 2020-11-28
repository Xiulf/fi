use crate::constraint::Constraint;
use crate::ctx::Ctx;
use crate::subst::Subst;
use crate::ty::*;

impl<'db> Ctx<'db> {
    pub(crate) fn unify(&mut self) {
        let mut cs = std::mem::replace(&mut self.cs, Vec::new());

        while !cs.is_empty() {
            let subst = self.unify_all(cs);

            for (_, ty) in self.tys.iter_mut() {
                subst.apply_ty(ty);
            }

            cs = std::mem::replace(&mut self.cs, Vec::new());
            subst.apply_cs(&mut cs);
        }
    }

    fn unify_all(&mut self, mut cs: Vec<Constraint>) -> Subst {
        let mut subst = Subst::empty();

        cs.reverse();

        while let Some(c) = cs.pop() {
            let s = self.unify_one(c);

            s.apply_cs(&mut cs);
            subst.compose(s);
        }

        subst
    }

    fn unify_one(&mut self, cs: Constraint) -> Subst {
        // println!("solving: {}", cs.display(self.db));

        match cs {
            Constraint::Equal(a, a_span, b, b_span) => match (&*a, &*b) {
                (Type::Infer(ivar), _) => self.unify_var(ivar, b, b_span),
                (_, Type::Infer(ivar)) => self.unify_var(ivar, a, a_span),
                (Type::Error, Type::Error) => Subst::empty(),
                (Type::Int(a), Type::Int(b)) if a == b => Subst::empty(),
                (Type::Var(a_var), Type::Var(b_var)) if a_var == b_var => Subst::empty(),
                (_, Type::ForAll(_b_vars, b_ty)) => {
                    self.unify_one(Constraint::Equal(a, a_span, b_ty.clone(), b_span))
                }
                (Type::App(a, _), _) => {
                    self.unify_one(Constraint::Equal(a.clone(), a_span, b, b_span))
                }
                (_, Type::App(b, _)) => {
                    self.unify_one(Constraint::Equal(a, a_span, b.clone(), b_span))
                }
                (Type::Func(a_params, a_ret), Type::Func(b_params, b_ret))
                    if a_params.len() == b_params.len() =>
                {
                    let mut cs = Vec::new();

                    for (a_param, b_param) in a_params.into_iter().zip(b_params) {
                        cs.push(Constraint::Equal(a_param, a_span, b_param, b_span));
                    }

                    cs.push(Constraint::Equal(
                        a_ret.clone(),
                        a_span,
                        b_ret.clone(),
                        b_span,
                    ));

                    self.unify_all(cs)
                }
                (Type::Tuple(a_tys), Type::Tuple(b_tys)) if a_tys.len() == b_tys.len() => {
                    let mut cs = Vec::with_capacity(a_tys.len());

                    for (a_ty, b_ty) in a_tys.into_iter().zip(b_tys) {
                        cs.push(Constraint::Equal(a_ty, a_span, b_ty, b_span));
                    }

                    self.unify_all(cs)
                }
                (Type::Record(a_fields, None), Type::Record(b_fields, None))
                    if a_fields.len() == b_fields.len() =>
                {
                    let mut cs = Vec::with_capacity(a_fields.len());

                    for (a_field, b_field) in a_fields.into_iter().zip(b_fields) {
                        if a_field.name != b_field.name {
                            self.db
                                .to_diag_db()
                                .error("field names are not equal")
                                .with_label(diagnostics::Label::primary(self.file, a_field.span))
                                .with_label(diagnostics::Label::primary(self.file, b_field.span))
                                .finish();
                        } else {
                            cs.push(Constraint::Equal(
                                a_field.ty.clone(),
                                a_field.span,
                                b_field.ty.clone(),
                                b_field.span,
                            ));
                        }
                    }

                    self.unify_all(cs)
                }
                (Type::Record(a_fields, Some(a_tail)), Type::Record(b_fields, None)) => {
                    let mut cs = Vec::with_capacity(a_fields.len() + 1);

                    for (a_field, b_field) in a_fields.into_iter().zip(b_fields) {
                        if a_field.name != b_field.name {
                        } else {
                            cs.push(Constraint::Equal(
                                a_field.ty.clone(),
                                a_field.span,
                                b_field.ty.clone(),
                                b_field.span,
                            ));
                        }
                    }

                    cs.push(Constraint::Equal(
                        a_tail.clone(),
                        a_span,
                        Ty::record(b_fields[a_fields.len()..].into(), None),
                        b_span,
                    ));

                    self.unify_all(cs)
                }
                (Type::Data(a), Type::Data(b)) if a == b => Subst::empty(),
                (_, _) => {
                    self.db
                        .to_diag_db()
                        .error(format!(
                            "mismatched types: `{}` != `{}`",
                            a.display(self.db),
                            b.display(self.db)
                        ))
                        .with_label(diagnostics::Label::primary(self.file, a_span))
                        .with_label(
                            diagnostics::Label::secondary(self.file, b_span).with_message(format!(
                                "type `{}` specified here",
                                b.display(self.db)
                            )),
                        )
                        .finish();

                    Subst::empty()
                }
            },
        }
    }

    fn unify_var(&self, ivar: &InferVar, ty: Ty, _span: hir::ir::Span) -> Subst {
        if let Type::Infer(ivar2) = &*ty {
            if ivar == ivar2 {
                Subst::empty()
            } else {
                vec![(*ivar, ty)].into_iter().collect()
            }
        } else {
            vec![(*ivar, ty)].into_iter().collect()
        }
    }
}
