use crate::constraint::{Constrain, Constraint};
use crate::subst::Subst;
use crate::ty::*;

pub type UnifyResult = Result<Subst, UnifyError>;

pub enum UnifyError {
    Mismatch,
}

impl<'ctx, 'db> Constrain<'ctx, 'db> {
    fn unify_all(&mut self, mut cs: Vec<Constraint>) -> UnifyResult {
        let mut subst = Subst::empty();

        cs.reverse();

        while let Some(c) = cs.pop() {
            let s = self.unify_one(c)?;

            s.apply_cs(&mut cs);
            subst.compose(s);
        }

        Ok(subst)
    }

    pub(crate) fn unify_one(&mut self, cs: Constraint) -> UnifyResult {
        // println!("solving: {}", cs.display(self.ctx.db));

        match cs {
            Constraint::Equal(a, b) => match (&*a, &*b) {
                (Type::Infer(ivar), _) => self.unify_var(ivar, b),
                (_, Type::Infer(ivar)) => self.unify_var(ivar, a),
                (Type::Error, Type::Error) => Ok(Subst::empty()),
                (Type::Int(a), Type::Int(b)) if a == b => Ok(Subst::empty()),
                (Type::Var(a_var), Type::Var(b_var)) if a_var == b_var => Ok(Subst::empty()),
                (_, Type::ForAll(_b_vars, b_ty)) => {
                    self.unify_one(Constraint::Equal(a, b_ty.clone()))
                }
                (Type::App(a, _, a_args), Type::App(b, _, b_args))
                    if a_args.len() == b_args.len() =>
                {
                    let mut cs = Vec::with_capacity(a_args.len() + 1);

                    cs.push(Constraint::Equal(a.clone(), b.clone()));

                    for (a, b) in a_args.into_iter().zip(b_args) {
                        cs.push(Constraint::Equal(a, b));
                    }

                    self.unify_all(cs)
                }
                (Type::App(a, _, _), _) => self.unify_one(Constraint::Equal(a.clone(), b)),
                (_, Type::App(b, _, _)) => self.unify_one(Constraint::Equal(a, b.clone())),
                (Type::Func(a_params, a_ret), Type::Func(b_params, b_ret))
                    if a_params.len() == b_params.len() =>
                {
                    let mut cs = Vec::new();

                    for (a_param, b_param) in a_params.into_iter().zip(b_params) {
                        cs.push(Constraint::Equal(a_param, b_param));
                    }

                    cs.push(Constraint::Equal(a_ret.clone(), b_ret.clone()));

                    self.unify_all(cs)
                }
                (Type::Tuple(a_tys), Type::Tuple(b_tys)) if a_tys.len() == b_tys.len() => {
                    let mut cs = Vec::with_capacity(a_tys.len());

                    for (a_ty, b_ty) in a_tys.into_iter().zip(b_tys) {
                        cs.push(Constraint::Equal(a_ty, b_ty));
                    }

                    self.unify_all(cs)
                }
                (Type::Record(a_fields, None), Type::Record(b_fields, None))
                    if a_fields.len() == b_fields.len() =>
                {
                    let mut cs = Vec::with_capacity(a_fields.len());

                    for (a_field, b_field) in a_fields.into_iter().zip(b_fields) {
                        if a_field.name != b_field.name {
                            // self.db
                            //     .to_diag_db()
                            //     .error("field names are not equal")
                            //     .with_label(diagnostics::Label::primary(self.file, a_field.span))
                            //     .with_label(diagnostics::Label::primary(self.file, b_field.span))
                            //     .finish();
                        } else {
                            cs.push(Constraint::Equal(a_field.ty.clone(), b_field.ty.clone()));
                        }
                    }

                    self.unify_all(cs)
                }
                (Type::Record(a_fields, Some(a_tail)), Type::Record(b_fields, None)) => {
                    let mut cs = Vec::with_capacity(a_fields.len() + 1);

                    for (a_field, b_field) in a_fields.into_iter().zip(b_fields) {
                        if a_field.name != b_field.name {
                        } else {
                            cs.push(Constraint::Equal(a_field.ty.clone(), b_field.ty.clone()));
                        }
                    }

                    cs.push(Constraint::Equal(
                        a_tail.clone(),
                        Ty::record(b_fields[a_fields.len()..].into(), None),
                    ));

                    self.unify_all(cs)
                }
                (Type::Data(a), Type::Data(b)) if a == b => Ok(Subst::empty()),
                (_, _) => Err(UnifyError::Mismatch),
            },
        }
    }

    fn unify_var(&self, ivar: &InferVar, ty: Ty) -> UnifyResult {
        if let Type::Infer(ivar2) = &*ty {
            if ivar == ivar2 {
                Ok(Subst::empty())
            } else {
                Ok(vec![(*ivar, ty)].into_iter().collect())
            }
        } else {
            Ok(vec![(*ivar, ty)].into_iter().collect())
        }
    }
}
