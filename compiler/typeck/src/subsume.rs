use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir::Expr;

impl<'db> Ctx<'db> {
    crate fn subsumes(&mut self, t1: Ty, t2: Ty) -> Result<Box<dyn Fn(&Expr) -> ()>> {
        self.subsumes_elaborate(t1, t2)
    }

    fn subsumes_elaborate(&mut self, t1: Ty, t2: Ty) -> Result<Box<dyn Fn(&Expr) -> ()>> {
        match (&*t1, &*t2) {
            (Type::ForAll(vars, r1, _), _) => {
                let subst = vars
                    .into_iter()
                    .map(|(v, k)| {
                        if let Some(k) = k {
                            (v, self.fresh_type_with_kind(t1.span(), t1.file(), k))
                        } else {
                            (v, self.fresh_type(t1.span(), t1.file()))
                        }
                    })
                    .collect();

                let replaced = r1.clone().replace_vars(subst);

                self.subsumes_elaborate(replaced, t2)
            }
            (_, Type::ForAll(vars, r1, sc)) => match sc {
                Some(sc) => {
                    let skolems = (0..vars.len())
                        .map(|_| self.new_skolem_constant())
                        .collect();

                    let sk = self.skolemize(t2.span(), t2.file(), vars, skolems, r1.clone(), *sc);

                    self.subsumes_elaborate(t1, sk)
                }
                None => Err(TypeError::Internal(
                    "subsumes: unspecified skolem scope".into(),
                )),
            },
            (Type::App(f1, a1), Type::App(f2, a2)) if self.is_func(f1) && self.is_func(f2) => {
                for (a1, a2) in a1.into_iter().zip(a2) {
                    self.subsumes_no_elaborate(a1, a2)?;
                }

                Ok(Box::new(|_| ()))
            }
            (Type::Ctnt(ctnt, r1), _) => {
                let elaborate = self.subsumes_elaborate(r1.clone(), t2)?;

                Ok(Box::new(move |e| {
                    elaborate(e);
                }))
            }
            (Type::App(f1, a1), Type::App(f2, a2)) if self.is_record(f1) && self.is_record(f2) => {
                unimplemented!();
            }
            (_, Type::App(ref base, _)) if self.is_record(base) => self.subsumes_elaborate(t2, t1),
            (_, _) => {
                self.unify_types(t1, t2)?;

                Ok(Box::new(|_| ()))
            }
        }
    }

    fn subsumes_no_elaborate(&mut self, t1: Ty, t2: Ty) -> Result<()> {
        match (&*t1, &*t2) {
            (Type::ForAll(vars, r1, _), _) => {
                let subst = vars
                    .into_iter()
                    .map(|(v, k)| {
                        if let Some(k) = k {
                            (v, self.fresh_type_with_kind(t1.span(), t1.file(), k))
                        } else {
                            (v, self.fresh_type(t1.span(), t1.file()))
                        }
                    })
                    .collect();

                let replaced = r1.clone().replace_vars(subst);

                self.subsumes_no_elaborate(replaced, t2)
            }
            (_, Type::ForAll(vars, r1, sc)) => match sc {
                Some(sc) => {
                    let skolems = (0..vars.len())
                        .map(|_| self.new_skolem_constant())
                        .collect();

                    let sk = self.skolemize(t2.span(), t2.file(), vars, skolems, r1.clone(), *sc);

                    self.subsumes_no_elaborate(t1, sk)
                }
                None => Err(TypeError::Internal(
                    "subsumes: unspecified skolem scope".into(),
                )),
            },
            (Type::App(f1, a1), Type::App(f2, a2)) if self.is_func(f1) && self.is_func(f2) => {
                for (a1, a2) in a1.into_iter().zip(a2) {
                    self.subsumes_no_elaborate(a1, a2)?;
                }

                Ok(())
            }
            (Type::App(f1, a1), Type::App(f2, a2)) if self.is_record(f1) && self.is_record(f2) => {
                unimplemented!();
            }
            (_, Type::App(ref base, _)) if self.is_record(base) => {
                self.subsumes_no_elaborate(t2, t1)
            }
            (_, _) => {
                self.unify_types(t1, t2)?;

                Ok(())
            }
        }
    }
}
