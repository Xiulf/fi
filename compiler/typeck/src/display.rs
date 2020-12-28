use crate::ty::*;
use crate::TypeDatabase;
use hir::ir;
use std::fmt::{Display, Formatter, Result};

pub trait TypedDisplay<S = ()> {
    fn typed_fmt(&self, db: &dyn TypeDatabase, s: &S, f: &mut Formatter) -> Result;
}

pub struct Typed<'a, S, T>(pub &'a dyn TypeDatabase, pub &'a S, pub &'a T);

impl<'a, S, T: TypedDisplay<S>> Display for Typed<'a, S, T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.2.typed_fmt(self.0, self.1, f)
    }
}

impl TypedDisplay for Ty {
    fn typed_fmt(&self, db: &dyn TypeDatabase, _: &(), f: &mut Formatter) -> Result {
        match &**self {
            Type::Error => write!(f, "{{error}}"),
            Type::Unknown(u) => write!(f, "?{}", u.0),
            Type::Skolem(v, _, _, _) => v.fmt(f),
            Type::Var(v) => v.fmt(f),
            Type::Int(i) => i.fmt(f),
            Type::String(s) => write!(f, "{:?}", s),
            Type::Tuple(ts) => write!(f, "({})", Typed(db, &(), &ts)),
            Type::Row(fs, None) => write!(f, "({})", Typed(db, &(), &fs)),
            Type::Row(fs, Some(tail)) => {
                write!(f, "({} | {})", Typed(db, &(), &fs), Typed(db, &(), tail))
            }
            Type::ForAll(vars, ty, _) => {
                write!(f, "forall")?;

                for (var, _) in vars {
                    write!(f, " {}", var)?;
                }

                write!(f, ". {}", Typed(db, &(), ty))
            }
            Type::Ctnt(ctnt, ty) => write!(f, "{} => {}", Typed(db, &(), ctnt), Typed(db, &(), ty)),
            Type::Ctor(id) => {
                let file = db.module_tree(id.lib).file(id.module);
                let hir = db.module_hir(file);
                let def = hir.def(*id);

                def.name().fmt(f)
            }
            Type::App(base, args) | Type::KindApp(base, args) => {
                if base.needs_parens() {
                    write!(f, "({})", Typed(db, &(), base))?;
                } else {
                    base.typed_fmt(db, &(), f)?;
                }

                for arg in args.iter() {
                    write!(f, " ")?;

                    if arg.needs_parens() {
                        write!(f, "({})", Typed(db, &(), arg))?;
                    } else {
                        arg.typed_fmt(db, &(), f)?;
                    }
                }

                Ok(())
            }
        }
    }
}

impl<T: TypedDisplay> TypedDisplay for &List<T> {
    fn typed_fmt(&self, db: &dyn TypeDatabase, _: &(), f: &mut Formatter) -> Result {
        for (i, t) in self.into_iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            t.typed_fmt(db, &(), f)?;
        }

        Ok(())
    }
}

impl TypedDisplay for Field {
    fn typed_fmt(&self, db: &dyn TypeDatabase, _: &(), f: &mut Formatter) -> Result {
        write!(f, "{} :: {}", self.name, Typed(db, &(), &self.ty))
    }
}

impl TypedDisplay for Ctnt {
    fn typed_fmt(&self, db: &dyn TypeDatabase, _: &(), f: &mut Formatter) -> Result {
        let file = db.module_tree(self.trait_.lib).file(self.trait_.module);
        let hir = db.module_hir(file);
        let def = hir.def(self.trait_);

        def.name().fmt(f)?;

        for ty in &self.tys {
            write!(f, " {}", Typed(db, &(), &ty))?;
        }

        Ok(())
    }
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut num = if self.0.local_id.0 > i32::max_value as u32 {
            u32::max_value() - self.0.local_id.0
        } else {
            self.0.local_id.0
        };

        while num >= 26 {
            write!(f, "{}", (b'a' + (num % 26) as u8) as char)?;
            num /= 26;
        }

        write!(f, "{}", (b'a' + num as u8) as char)
    }
}

pub type Types = std::collections::HashMap<ir::HirId, Ty>;

impl TypedDisplay<Types> for ir::Body {
    fn typed_fmt(&self, db: &dyn TypeDatabase, tys: &Types, f: &mut Formatter) -> Result {
        for param in &self.params {
            write!(f, "{} ", Typed(db, tys, param))?;
        }

        write!(f, "= {}", Typed(db, tys, &self.value))
    }
}

impl TypedDisplay<Types> for ir::Param {
    fn typed_fmt(&self, db: &dyn TypeDatabase, tys: &Types, f: &mut Formatter) -> Result {
        write!(
            f,
            "($p{} :: {})",
            self.id.local_id.0,
            Typed(db, &(), &tys[&self.id])
        )
    }
}

impl TypedDisplay<Types> for ir::Guarded {
    fn typed_fmt(&self, db: &dyn TypeDatabase, tys: &Types, f: &mut Formatter) -> Result {
        match self {
            ir::Guarded::Unconditional(expr) => write!(f, "-> {}", Typed(db, tys, expr)),
            ir::Guarded::Guarded(_exprs) => unimplemented!(),
        }
    }
}

impl TypedDisplay<Types> for ir::Expr {
    fn typed_fmt(&self, db: &dyn TypeDatabase, tys: &Types, f: &mut Formatter) -> Result {
        let ty = &tys[&self.id];

        match &self.kind {
            ir::ExprKind::Error => write!(f, "{{error}} :: {}", Typed(db, &(), ty)),
            ir::ExprKind::Hole { name } => write!(f, "?{} :: {}", name, Typed(db, &(), ty)),
            ir::ExprKind::Int { val } => write!(f, "{} :: {}", val, Typed(db, &(), ty)),
            ir::ExprKind::Float { bits } => {
                write!(f, "{} :: {}", f64::from_bits(*bits), Typed(db, &(), ty))
            }
            ir::ExprKind::Ident { name, .. } => write!(f, "{} :: {}", name, Typed(db, &(), ty)),
            ir::ExprKind::Case { pred, arms } => {
                write!(f, "case ")?;

                for (i, pred) in pred.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    pred.typed_fmt(db, tys, f)?;
                }

                write!(f, " of")?;

                for arm in arms {
                    writeln!(f)?;
                    arm.typed_fmt(db, tys, f)?;
                }

                Ok(())
            }
            _ => unimplemented!(),
        }
    }
}

impl TypedDisplay<Types> for ir::CaseArm {
    fn typed_fmt(&self, db: &dyn TypeDatabase, tys: &Types, f: &mut Formatter) -> Result {
        for (i, pat) in self.pats.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            pat.typed_fmt(db, tys, f)?;
        }

        write!(f, " {}", Typed(db, tys, &self.val))
    }
}

impl TypedDisplay<Types> for ir::Pat {
    fn typed_fmt(&self, db: &dyn TypeDatabase, tys: &Types, f: &mut Formatter) -> Result {
        let ty = &tys[&self.id];

        match &self.kind {
            ir::PatKind::Error => write!(f, "{{error}} :: {}", Typed(db, &(), ty)),
            ir::PatKind::Bind { name, sub: None } => {
                write!(f, "{} :: {}", name, Typed(db, &(), ty))
            }
            _ => unimplemented!(),
        }
    }
}
