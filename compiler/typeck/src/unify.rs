use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir::Span;

impl<'db> Ctx<'db> {
    /// Generate a fresh unknown.
    crate fn fresh_unknown(&mut self) -> Unknown {
        let u = Unknown(self.next_ty);
        let _ = self.next_ty += 1;

        u
    }

    /// Generate a fresh type variable with an unknown kind.
    /// Avoid this if at all possible.
    crate fn fresh_type(&mut self, span: Span, file: source::FileId) -> Ty {
        let t1 = Unknown(self.next_ty);
        let t2 = Unknown(self.next_ty + 1);
        let kind_type = self.ty_kind(span, file);

        self.next_ty += 2;
        self.subst
            .unsolved
            .insert(t1, (UnkLevel::from(t1), kind_type));
        self.subst
            .unsolved
            .insert(t2, (UnkLevel::from(t2), Ty::unknown(span, file, t1)));

        Ty::unknown(span, file, t2)
    }

    /// Generate a fresh type variable with a known kind.
    crate fn fresh_type_with_kind(&mut self, span: Span, file: source::FileId, kind: Ty) -> Ty {
        let t = Unknown(self.next_ty);

        self.next_ty += 1;
        self.subst.unsolved.insert(t, (UnkLevel::from(t), kind));

        Ty::unknown(span, file, t)
    }

    /// Update the substitution to solve a type constraint.
    crate fn solve_type(&mut self, u: Unknown, t: Ty) -> Result<()> {
        let _ = self.occurs_check(u, &t)?;
        let k1 = self.elaborate_kind(&t)?;
        let k2 = if let Some((_, k2)) = self.subst.unsolved.get(&u) {
            self.subst_type(k2.clone())
        } else {
            panic!("Unification variable has no kind");
        };

        let t2 = self.instantiate_kind(t, k1, k2)?;

        self.subst.tys.insert(u, t2);

        Ok(())
    }

    /// Apply a substitution to a type.
    crate fn subst_type(&self, ty: Ty) -> Ty {
        ty.everywhere(|t| match *t {
            Type::Unknown(u) => match self.subst.tys.get(&u) {
                None => t,
                Some(t2) => match **t2 {
                    Type::Unknown(u2) if u2 == u => Ty::unknown(t2.span(), t2.file(), u2),
                    _ => self.subst_type(t2.clone()),
                },
            },
            _ => t,
        })
    }

    /// Make sure that an unknown does not occur in a type.
    crate fn occurs_check(&mut self, u: Unknown, t: &Ty) -> Result<()> {
        if let Type::Unknown(_) = **t {
            Ok(())
        } else {
            t.clone()
                .everywhere_result(|t2| match *t2 {
                    Type::Unknown(u2) if u == u2 => Err(TypeError::CyclicType(t.clone())),
                    _ => Ok(t2),
                })
                .map(|_| ())
        }
    }

    /// Unify two types, updating the current substitution.
    crate fn unify_types(&mut self, t1: Ty, t2: Ty) -> Result<()> {
        match (&*t1, &*t2) {
            (Type::Unknown(u1), Type::Unknown(u2)) if u1 == u2 => Ok(()),
            (Type::Unknown(u), _) => self.solve_type(*u, t2),
            (_, Type::Unknown(u)) => self.solve_type(*u, t1),
            (Type::ForAll(v1, r1, s1), Type::ForAll(v2, r2, s2)) if v1.len() == v2.len() => {
                match (s1, s2) {
                    (Some(s1), Some(s2)) => {
                        let skolems = (0..v1.len())
                            .map(|_| self.new_skolem_constant())
                            .collect::<Vec<_>>();

                        let sk1 = self.skolemize(
                            t1.span(),
                            t1.file(),
                            v1,
                            skolems.clone(),
                            r1.clone(),
                            *s1,
                        );
                        let sk2 =
                            self.skolemize(t2.span(), t2.file(), v2, skolems, r2.clone(), *s2);

                        self.unify_types(sk1, sk2)
                    }
                    (_, _) => Err(TypeError::Internal(
                        "unify_types: unspecified skolem scope".into(),
                    )),
                }
            }
            (Type::ForAll(vars, ty, sc), _) => match sc {
                Some(sc) => {
                    let skolems = (0..vars.len())
                        .map(|_| self.new_skolem_constant())
                        .collect::<Vec<_>>();

                    let sk = self.skolemize(t1.span(), t1.file(), vars, skolems, ty.clone(), *sc);

                    self.unify_types(sk, t2)
                }
                _ => Err(TypeError::Internal(
                    "unify_types: unspecified skolem scope".into(),
                )),
            },
            (_, Type::ForAll(..)) => self.unify_types(t2, t1),
            (Type::Var(v1), Type::Var(v2)) if v1 == v2 => Ok(()),
            (Type::Ctor(c1), Type::Ctor(c2)) if c1 == c2 => Ok(()),
            (Type::Int(i1), Type::Int(i2)) if i1 == i2 => Ok(()),
            (Type::String(s1), Type::String(s2)) if s1 == s2 => Ok(()),
            (Type::App(a1, b1), Type::App(a2, b2)) => {
                self.unify_types(a1.clone(), a2.clone())?;

                for (b1, b2) in b1.into_iter().zip(b2) {
                    self.unify_types(b1, b2)?;
                }

                Ok(())
            }
            (Type::Skolem(_, _, s1, _), Type::Skolem(_, _, s2, _)) if s1 == s2 => Ok(()),
            (Type::Tuple(ts1), Type::Tuple(ts2)) if ts1.len() == ts2.len() => ts1
                .into_iter()
                .zip(ts2)
                .map(|(t1, t2)| self.unify_types(t1, t2))
                .collect::<Result<_>>(),
            (Type::Row(r1, t1), Type::Row(r2, t2)) => {
                self.unify_rows(r1.clone(), t1.clone(), r2.clone(), t2.clone())
            }
            (Type::Ctnt(c1, r1), Type::Ctnt(c2, r2)) if c1.trait_ == c2.trait_ => {
                for (t1, t2) in (&c1.tys).into_iter().zip(&c2.tys) {
                    self.unify_types(t1, t2)?;
                }

                self.unify_types(r1.clone(), r2.clone())
            }
            (_, _) => Err(TypeError::Mismatch(t1, t2)),
        }
    }

    /// Unify two rows, updating the current substitution.
    /// Common labels are identified and unified. Remaining labels and types are unified with a
    /// trailing row unification variable, if appropriate.
    crate fn unify_rows(
        &mut self,
        r1: List<Field>,
        t1: Option<Ty>,
        r2: List<Field>,
        t2: Option<Ty>,
    ) -> Result<()> {
        Ok(())
    }
}
