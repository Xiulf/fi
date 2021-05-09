use crate::class::{Class, FunDep};
use crate::db::HirDatabase;
use crate::ty::*;
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

impl HirDisplay for Ty {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        if f.should_truncate() {
            return write!(f, "...");
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
            | TyKind::Ctor(id) => write!(f, "{}", f.db.type_ctor_data(id).name),
            | TyKind::Tuple(tys) => {
                write!(f, "(")?;
                f.write_joined(tys.iter(), ", ")?;
                write!(f, ")")
            },
            | TyKind::App(base, arg) => {
                write!(f, "(")?;
                base.hir_fmt(f)?;
                write!(f, " ")?;
                arg.hir_fmt(f)?;
                write!(f, ")")
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

impl HirDisplay for Constraint {
    fn hir_fmt(&self, f: &mut HirFormatter) -> fmt::Result {
        let class_name = &f.db.class_data(self.class).name;

        write!(f, "{} ", class_name)?;
        f.write_joined(self.tys.iter(), " ")
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
