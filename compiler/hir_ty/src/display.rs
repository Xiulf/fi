use crate::class::{Class, FunDep, Instance, Instances};
use crate::db::HirDatabase;
use crate::ty::*;
use hir_def::id::Lookup;
use hir_def::lang_item::LangItem;
use std::fmt;

pub trait HirDisplay {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result;

    fn into_displayable<'a>(
        &'a self,
        db: &'a dyn HirDatabase,
        max_size: Option<usize>,
        display_target: DisplayTarget,
    ) -> HirDisplayWrapper<'a, Self>
    where
        Self: Sized,
    {
        HirDisplayWrapper {
            db,
            t: self,
            max_size,
            display_target,
        }
    }

    fn display<'a>(&'a self, db: &'a dyn HirDatabase) -> HirDisplayWrapper<'a, Self>
    where
        Self: Sized,
    {
        self.into_displayable(db, None, DisplayTarget::Diagnostics)
    }

    fn display_test<'a>(&'a self, db: &'a dyn HirDatabase) -> HirDisplayWrapper<'a, Self>
    where
        Self: Sized,
    {
        self.into_displayable(db, None, DisplayTarget::Test)
    }
}

pub struct HirFormatter<'a> {
    pub db: &'a dyn HirDatabase,
    fmt: &'a mut dyn fmt::Write,
    buf: String,
    curr_size: usize,
    max_size: Option<usize>,
    display_target: DisplayTarget,
}

#[derive(Clone, Copy)]
pub enum DisplayTarget {
    Diagnostics,
    Test,
}

pub struct HirDisplayWrapper<'a, T> {
    db: &'a dyn HirDatabase,
    t: &'a T,
    max_size: Option<usize>,
    display_target: DisplayTarget,
}

impl<'a> HirFormatter<'a> {
    pub fn write_joined<T: HirDisplay>(&mut self, iter: impl IntoIterator<Item = T>, sep: &str) -> fmt::Result {
        let mut first = true;

        for e in iter {
            if !first {
                write!(self, "{}", sep)?;
            }

            first = false;
            e.hir_fmt(self)?;
        }

        Ok(())
    }

    pub fn write_fmt(&mut self, args: fmt::Arguments) -> fmt::Result {
        self.buf.clear();
        fmt::write(&mut self.buf, args)?;
        self.curr_size += self.buf.len();
        self.fmt.write_str(&self.buf)
    }

    pub fn should_truncate(&self) -> bool {
        if let Some(max_size) = self.max_size {
            self.curr_size >= max_size
        } else {
            false
        }
    }
}

impl<'a, T: HirDisplay> HirDisplay for &'a T {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        (**self).hir_fmt(f)
    }
}

impl<'a, T> fmt::Display for HirDisplayWrapper<'a, T>
where
    T: HirDisplay,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.t.hir_fmt(&mut HirFormatter {
            db: self.db,
            fmt: f,
            buf: String::with_capacity(20),
            curr_size: 0,
            max_size: self.max_size,
            display_target: self.display_target,
        })
    }
}

impl HirDisplay for TypeVar {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        self.debruijn().hir_fmt(f)
    }
}

impl HirDisplay for Unknown {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        write!(f, "?{}", self.raw())
    }
}

impl HirDisplay for DebruijnIndex {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        let depth = self.depth();
        let depth = unsafe { std::char::from_u32_unchecked('a' as u32 + depth) };

        write!(f, "{}", depth)
    }
}

impl Ty {
    fn needs_paren(self, db: &dyn HirDatabase) -> bool {
        match self.lookup(db) {
            | TyKind::App(..) | TyKind::ForAll(..) | TyKind::Ctnt(..) => true,
            | _ => false,
        }
    }
}

struct TyParens(Ty);

impl HirDisplay for TyParens {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        let needs_paren = self.0.needs_paren(f.db);

        if needs_paren {
            write!(f, "(")?;
        }

        self.0.hir_fmt(f)?;

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl HirDisplay for Ty {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        if f.should_truncate() {
            return write!(f, "...");
        }

        let mut args = Vec::new();
        let mut ty = *self;

        while let TyKind::App(a, b) = ty.lookup(f.db) {
            args.push(b);
            ty = a;
        }

        macro_rules! match_lang {
            (match $id:ident, $args:ident, $lib:ident, $f:ident { $($name:literal($arity:literal) => $code:block),* $(,)? }) => {
                $(
                    if Some($id) == $f.db.lang_item($lib, $name.into()).and_then(LangItem::as_type_ctor) &&
                        $args.len() == $arity
                    {
                        return $code;
                    }
                )else*
            };
        }

        if let TyKind::Ctor(id) = ty.lookup(f.db) {
            let loc = id.lookup(f.db.upcast());
            let lib = loc.module.lib;

            args.reverse();

            match_lang! {
                match id, args, lib, f {
                    "fn-type"(2) => {
                        TyParens(args[0]).hir_fmt(f)?;
                        write!(f, " -> ")?;
                        args[1].hir_fmt(f)
                    },
                    "ptr-type"(1) => {
                        write!(f, "*")?;
                        TyParens(args[0]).hir_fmt(f)
                    },
                    "ptrb-type"(1) => {
                        write!(f, "[*]")?;
                        TyParens(args[0]).hir_fmt(f)
                    },
                    "ptrbs-type"(2) => {
                        write!(f, "[*:")?;
                        args[1].hir_fmt(f)?;
                        write!(f, "]")?;
                        TyParens(args[0]).hir_fmt(f)
                    },
                    "record-type"(1) => {
                        write!(f, "{{")?;

                        if let TyKind::Row(fields, tail) = args[0].lookup(f.db) {
                            if !fields.is_empty() {
                                write!(f, " ")?;
                            }

                            f.write_joined(fields.iter(), ", ")?;

                            if let Some(tail) = tail {
                                write!(f, " | ")?;
                                tail.hir_fmt(f)?;
                                write!(f, " ")?;
                            } else if !fields.is_empty() {
                                write!(f, " ")?;
                            }
                        }

                        write!(f, "}}")
                    },
                }
            }
        }

        match self.lookup(f.db) {
            | TyKind::Error => write!(f, "{{error}}"),
            | TyKind::Unknown(u) => u.hir_fmt(f),
            | TyKind::Skolem(p, kind) => {
                write!(f, "(_ :: ")?;
                kind.hir_fmt(f)?;
                write!(f, ")")
            },
            | TyKind::TypeVar(t) => t.hir_fmt(f),
            | TyKind::Figure(i) => write!(f, "{}", i),
            | TyKind::Symbol(s) => write!(f, "{}", s),
            | TyKind::Row(fields, tail) => {
                write!(f, "(")?;
                f.write_joined(fields.iter(), ", ")?;

                if let Some(tail) = tail {
                    write!(f, " | ")?;
                    tail.hir_fmt(f)?;
                }

                write!(f, ")")
            },
            | TyKind::Ctor(id) => write!(f, "{}", f.db.type_ctor_data(id).name),
            | TyKind::Tuple(tys) => {
                write!(f, "(")?;
                f.write_joined(tys.iter(), ", ")?;
                write!(f, ")")
            },
            | TyKind::App(base, arg) => {
                TyParens(base).hir_fmt(f)?;
                write!(f, " ")?;
                TyParens(arg).hir_fmt(f)
            },
            | TyKind::Ctnt(ctnt, ty) => {
                ctnt.hir_fmt(f)?;
                write!(f, " => ")?;
                ty.hir_fmt(f)
            },
            | TyKind::ForAll(kind, ty) => {
                write!(f, "for ")?;
                kind.hir_fmt(f)?;
                write!(f, ". ")?;
                ty.hir_fmt(f)
            },
            | _ => unimplemented!(),
        }
    }
}

impl HirDisplay for Field {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        write!(f, "{} :: ", self.name)?;
        self.ty.hir_fmt(f)
    }
}

impl HirDisplay for Constraint {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        let class_name = &f.db.class_data(self.class).name;

        write!(f, "{} ", class_name)?;
        f.write_joined(self.types.iter(), " ")
    }
}

impl HirDisplay for Class {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        let data = f.db.class_data(self.id);

        write!(f, "class {}", data.name)?;

        for kind in self.vars.iter() {
            write!(f, " ")?;
            kind.hir_fmt(f)?;
        }

        if !self.fundeps.is_empty() {
            write!(f, " | ")?;
            f.write_joined(self.fundeps.iter(), ", ")?;
        }

        Ok(())
    }
}

impl HirDisplay for FunDep {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        for var in self.determiners.iter() {
            var.hir_fmt(f)?;
            write!(f, " ")?;
        }

        write!(f, "->")?;

        for var in self.determined.iter() {
            write!(f, " ")?;
            var.hir_fmt(f)?;
        }

        Ok(())
    }
}

impl HirDisplay for Instance {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        write!(f, "instance {}", f.db.class_data(self.class).name)?;

        for ty in self.types.iter() {
            write!(f, " ")?;
            ty.hir_fmt(f)?;
        }

        if !self.constraints.is_empty() {
            write!(f, " : ")?;
            f.write_joined(self.constraints.iter(), ", ")?;
        }

        Ok(())
    }
}

impl HirDisplay for Instances {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        f.write_joined(self.matchers.iter().map(|m| &m.instance), "\n")
    }
}
