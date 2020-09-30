use crate::constraint::*;
use crate::subst::*;
use crate::tcx::Tcx;
use crate::ty::*;
use diagnostics::{Diagnostic, Severity};

impl<'tcx> Tcx<'tcx> {
    pub fn unify(&self) {
        let mut cs = self.constraints.replace(Constraints::new());

        while !cs.is_empty() {
            let subst = self.unify_all(cs);

            for (_, ty) in self.types.borrow_mut().iter_mut() {
                subst.apply_ty(ty);
            }

            cs = self.constraints.replace(Constraints::new());
            subst.apply_cs(&mut cs);
        }
    }

    fn unify_all(&self, mut cs: Constraints<'tcx>) -> Subst<'tcx> {
        let mut subst = Subst::empty();
        // let mut i = 0;

        cs.reverse();

        while let Some(c) = cs.pop() {
            // println!("{}: {}", i, c);
            // i += 1;

            let s = self.unify_one(c);

            s.apply_cs(&mut cs);
            subst.compose(s);
        }

        subst
    }

    fn unify_one(&self, cs: Constraint<'tcx>) -> Subst<'tcx> {
        match cs {
            Constraint::Equal(a, a_span, b, b_span) => match (a, b) {
                (Type::Var(tvar), _) => self.unify_var(*tvar, b, b_span),
                (_, Type::Var(tvar)) => self.unify_var(*tvar, a, a_span),
                (Type::TypeOf(a, a_args), Type::TypeOf(b, b_args)) if a == b => {
                    let mut cs = Constraints::new();

                    for (a, b) in a_args.iter().zip(b_args.iter()) {
                        cs.push(Constraint::Equal(a, a_span, b, b_span));
                    }

                    self.unify_all(cs)
                }
                (Type::TypeOf(id, args), _) => self.unify_one(Constraint::Equal(
                    self.type_of(id).mono(self, args.to_vec()),
                    a_span,
                    b,
                    b_span,
                )),
                (_, Type::TypeOf(id, args)) => self.unify_one(Constraint::Equal(
                    a,
                    a_span,
                    self.type_of(id).mono(self, args.to_vec()),
                    b_span,
                )),
                (Type::Error, _)
                | (_, Type::Error)
                | (Type::Never, _)
                | (_, Type::Never)
                | (Type::Bool, Type::Bool)
                | (Type::Str, Type::Str)
                | (Type::TypeId, Type::TypeId) => Subst::empty(),
                (Type::Param(a), Type::Param(b)) if a == b => Subst::empty(),
                (Type::Int(a), Type::Int(b)) if a == b => Subst::empty(),
                (Type::UInt(a), Type::UInt(b)) if a == b => Subst::empty(),
                (Type::Float(a), Type::Float(b)) if a == b => Subst::empty(),
                (Type::VInt(tvar), Type::Int(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::VInt(tvar), Type::UInt(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::VInt(tvar), Type::VInt(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::Int(_), Type::VInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::VUInt(tvar), Type::UInt(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::VUInt(tvar), Type::VUInt(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::UInt(_), Type::VInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::UInt(_), Type::VUInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::VFloat(tvar), Type::Float(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::VFloat(tvar), Type::VFloat(_)) => vec![(*tvar, b)].into_iter().collect(),
                (Type::Float(_), Type::VFloat(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::Ref(a_mut, a), Type::Ref(b_mut, b))
                    if a_mut == b_mut || (!*b_mut && *a_mut) =>
                {
                    self.unify_one(Constraint::Equal(a, a_span, b, b_span))
                }
                (Type::Array(a, a_len), Type::Array(b, b_len)) if a_len == b_len => {
                    self.unify_one(Constraint::Equal(a, a_span, b, b_span))
                }
                (Type::Slice(a), Type::Slice(b)) => {
                    self.unify_one(Constraint::Equal(a, a_span, b, b_span))
                }
                (Type::Tuple(a_tys), Type::Tuple(b_tys)) => {
                    let mut cs = Constraints::new();

                    for (a, b) in a_tys.iter().zip(b_tys.iter()) {
                        cs.push(Constraint::Equal(a, a_span, b, b_span));
                    }

                    self.unify_all(cs)
                }
                (Type::Func(_, a_params, a_ret), Type::Func(_, b_params, b_ret))
                    if a_params.len() != b_params.len() =>
                {
                    let mut cs = Constraints::new();

                    for (a, b) in a_params.iter().zip(b_params.iter()) {
                        cs.push(Constraint::Equal(a.ty, a.span, b.ty, b.span));
                    }

                    cs.push(Constraint::Equal(a_ret, a_span, b_ret, b_span));

                    self.unify_all(cs)
                }
                (Type::Struct(a, a_fields), Type::Struct(b, b_fields)) if a == b => {
                    let mut cs = Constraints::new();

                    for (a, b) in a_fields.iter().zip(b_fields.iter()) {
                        cs.push(Constraint::Equal(a.ty, a_span, b.ty, b_span));
                    }

                    self.unify_all(cs)
                }
                (Type::Enum(a, _), Type::Enum(b, _)) if a == b => Subst::empty(),
                (a, Type::Forall(_, b)) => self.unify_one(Constraint::Equal(a, a_span, b, b_span)),
                (_, _) => {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0007,
                            format!(
                                "mismatched types: `{}` != `{}`",
                                a.display(self),
                                b.display(self)
                            ),
                        )
                        .label(
                            Severity::Error,
                            a_span,
                            format!("found type `{}`", a.display(self)),
                        )
                        .label(
                            Severity::Info,
                            b_span,
                            format!("type `{}` specified here", b.display(self)),
                        ),
                    );

                    Subst::empty()
                }
            },
            Constraint::PtrArith(a, a_span, b, b_span) => match (a, b) {
                (Type::Ref(_, a), Type::Ref(_, b)) => {
                    self.unify_one(Constraint::Equal(a, a_span, b, b_span))
                }
                (Type::Ref(_, _), Type::Int(0)) | (Type::Ref(_, _), Type::UInt(0)) => {
                    Subst::empty()
                }
                (Type::Ref(_, _), Type::VInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (Type::Ref(_, _), Type::VUInt(tvar)) => vec![(*tvar, a)].into_iter().collect(),
                (_, _) => {
                    let mut cs = Constraints::new();

                    cs.push(Constraint::Equal(b, b_span, a, a_span));
                    cs.push(Constraint::IsNum(a, a_span));
                    cs.push(Constraint::IsNum(b, b_span));

                    self.unify_all(cs)
                }
            },
            Constraint::IsNum(ty, span) => match ty {
                Type::VInt(_)
                | Type::VUInt(_)
                | Type::VFloat(_)
                | Type::Int(_)
                | Type::UInt(_)
                | Type::Float(_)
                | Type::Error => Subst::empty(),
                Type::Var(_) => {
                    self.constrain(Constraint::IsNum(ty, span));

                    Subst::empty()
                }
                _ => {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0008,
                            format!("type `{}` is not numeric", ty.display(self)),
                        )
                        .label(Severity::Error, span, None::<String>),
                    );

                    Subst::empty()
                }
            },
            Constraint::IsInt(ty, span) => match ty {
                Type::VInt(_) | Type::VUInt(_) | Type::Int(_) | Type::UInt(_) | Type::Error => {
                    Subst::empty()
                }
                Type::Var(_) => {
                    self.constrain(Constraint::IsInt(ty, span));

                    Subst::empty()
                }
                _ => {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0009,
                            format!("type `{}` is not an integer", ty.display(self)),
                        )
                        .label(Severity::Error, span, None::<String>),
                    );

                    Subst::empty()
                }
            },
            Constraint::Call(fn_ty, fn_span, b_params, b_ret, ret_span) => {
                if let Type::Func(_, a_params, a_ret) = fn_ty {
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
                                cs.push(Constraint::Equal(arg.ty, arg.span, param.ty, param.span));
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
                            cs.push(Constraint::Equal(arg.ty, arg.span, param.ty, param.span));
                        }
                    }

                    cs.push(Constraint::Equal(b_ret, ret_span, a_ret, fn_span));

                    self.unify_all(cs)
                } else if let Type::Error = fn_ty {
                    Subst::empty()
                } else {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0010,
                            format!("type `{}` is not a fuction", fn_ty.display(self)),
                        )
                        .label(Severity::Error, fn_span, None::<String>),
                    );

                    Subst::empty()
                }
            }
            Constraint::Index(list_ty, list_span, ret_ty, ret_span) => match list_ty {
                Type::Ref(_, to) => {
                    self.unify_one(Constraint::Index(to, list_span, ret_ty, ret_span))
                }
                Type::Str => self.unify_one(Constraint::Equal(
                    ret_ty,
                    ret_span,
                    self.builtin.u8,
                    list_span,
                )),
                Type::Array(of, _) => {
                    self.unify_one(Constraint::Equal(ret_ty, ret_span, of, list_span))
                }
                Type::Slice(of) => {
                    self.unify_one(Constraint::Equal(ret_ty, ret_span, of, list_span))
                }
                Type::Var(_) => {
                    self.constrain(Constraint::Index(list_ty, list_span, ret_ty, ret_span));

                    Subst::empty()
                }
                _ => {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0014,
                            format!("type `{}` cannot be indexed", list_ty.display(self)),
                        )
                        .label(Severity::Error, list_span, None::<String>),
                    );

                    Subst::empty()
                }
            },
            Constraint::Field(obj_ty, obj_span, field, ret_ty, ret_span) => {
                let fields = match obj_ty {
                    Type::Ref(_, to) => {
                        return self
                            .unify_one(Constraint::Field(to, obj_span, field, ret_ty, ret_span))
                    }
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
                        (
                            Ident {
                                symbol: hir::Symbol::new("stride"),
                                span: obj_span,
                            },
                            self.builtin.usize,
                        ),
                    ],
                    Type::Str => vec![
                        (
                            Ident {
                                symbol: hir::Symbol::new("ptr"),
                                span: obj_span,
                            },
                            self.builtin.ref_u8,
                        ),
                        (
                            Ident {
                                symbol: hir::Symbol::new("len"),
                                span: obj_span,
                            },
                            self.builtin.usize,
                        ),
                    ],
                    Type::Array(_, _) => vec![(
                        Ident {
                            symbol: hir::Symbol::new("len"),
                            span: obj_span,
                        },
                        self.builtin.usize,
                    )],
                    Type::Slice(of) => vec![
                        (
                            Ident {
                                symbol: hir::Symbol::new("ptr"),
                                span: obj_span,
                            },
                            self.intern_ty(Type::Ref(false, of)),
                        ),
                        (
                            Ident {
                                symbol: hir::Symbol::new("len"),
                                span: obj_span,
                            },
                            self.builtin.usize,
                        ),
                    ],
                    Type::Tuple(tys) => tys
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| {
                            (
                                Ident {
                                    symbol: hir::Symbol::new(i.to_string()),
                                    span: obj_span,
                                },
                                ty,
                            )
                        })
                        .collect(),
                    Type::Struct(_, fields) => fields.iter().map(|f| (f.name, f.ty)).collect(),
                    Type::Var(_) => {
                        self.constrain(Constraint::Field(
                            obj_ty, obj_span, field, ret_ty, ret_span,
                        ));

                        return Subst::empty();
                    }
                    Type::Forall(_, ty) => {
                        return self
                            .unify_one(Constraint::Field(ty, obj_span, field, ret_ty, ret_span));
                    }
                    _ => {
                        self.reporter.add(
                            Diagnostic::new(
                                Severity::Error,
                                0015,
                                format!("type `{}` does not have any fields", obj_ty.display(self)),
                            )
                            .label(
                                Severity::Error,
                                obj_span,
                                None::<String>,
                            ),
                        );

                        return Subst::empty();
                    }
                };

                if let Some(field) = fields.iter().find(|f| f.0.symbol == field.symbol) {
                    self.unify_one(Constraint::Equal(field.1, field.0.span, ret_ty, ret_span))
                } else {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0013,
                            format!(
                                "type `{}` does not have field '{}'",
                                obj_ty.display(self),
                                field
                            ),
                        )
                        .label(Severity::Error, field.span, None::<String>),
                    );

                    Subst::empty()
                }
            }
        }
    }

    fn unify_var(&self, tvar: TypeVar, ty: Ty<'tcx>, span: diagnostics::Span) -> Subst<'tcx> {
        if let Type::Var(tvar2) = ty {
            if tvar == *tvar2 {
                Subst::empty()
            } else {
                vec![(tvar, ty)].into_iter().collect()
            }
        } else if occurs(ty, tvar) {
            self.reporter.add(
                Diagnostic::new(
                    Severity::Error,
                    0016,
                    format!("Recursive type `{}`", ty.display(self)),
                )
                .label(Severity::Error, span, None::<String>),
            );

            let ptr = ty as *const _ as *mut _;
            unsafe {
                *ptr = Type::Error;
            }

            Subst::empty()
        } else {
            vec![(tvar, ty)].into_iter().collect()
        }
    }
}

fn occurs(ty: Ty<'_>, tvar: TypeVar) -> bool {
    match ty {
        Type::Var(tvar2) if *tvar2 == tvar => true,
        Type::Ref(_, to) => occurs(to, tvar),
        Type::Array(of, _) => occurs(of, tvar),
        Type::Slice(of) => occurs(of, tvar),
        Type::Func(_, params, ret) => {
            params.iter().any(|p| occurs(p.ty, tvar)) || occurs(ret, tvar)
        }
        Type::Tuple(tys) => tys.iter().any(|t| occurs(t, tvar)),
        Type::Struct(_, fields) => fields.iter().any(|f| occurs(f.ty, tvar)),
        Type::Enum(_, variants) => variants
            .iter()
            .any(|v| v.fields.iter().any(|f| occurs(f.ty, tvar))),
        _ => false,
    }
}
