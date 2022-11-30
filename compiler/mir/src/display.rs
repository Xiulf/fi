use std::fmt::{Debug, Display, Formatter, Result, Write};

use hir::display::HirFormatter;
use hir::id::DefWithBodyId;
use hir::{DefWithBody, HirDisplay};

use crate::syntax::*;

impl HirDisplay for Module {
    fn hir_fmt(&self, f: &mut HirFormatter) -> std::fmt::Result {
        // write!(f, "module {}", self.name)?;

        // if !self.functions.is_empty() {
        //     writeln!(f)?;
        // }

        // for (_, func) in self.functions.iter() {
        //     write!(f, "\n{}", func)?;
        // }

        // if !self.bodies.is_empty() {
        //     writeln!(f)?;
        // }

        // for (_, body) in self.bodies.iter() {
        //     write!(f, "\n{}", body.display(f.db))?;
        // }

        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} fn {}", self.linkage, self.name)?;

        if let Some(body) = self.body {
            write!(f, " = {}", body)?;
        }

        Ok(())
    }
}

impl Display for Linkage {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | Self::Local => f.write_str("local "),
            | Self::Import => f.write_str("import"),
            | Self::Export => f.write_str("export"),
        }
    }
}

impl Display for Body {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "@")
    }
}

impl HirDisplay for BodyOrigin {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result {
        let def = match self.def {
            | DefWithBodyId::FuncId(id) => hir::Func::from(id).link_name(f.db).0,
            | DefWithBodyId::StaticId(id) => hir::Static::from(id).link_name(f.db).0,
            | DefWithBodyId::ConstId(id) => hir::Const::from(id).name(f.db),
        };

        write!(f, "{}", def)?;

        if let Some(expr) = self.expr {
            write!(f, "#{}", u32::from(expr.into_raw()))?;
        }

        Ok(())
    }
}

impl HirDisplay for BodyData {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result {
        writeln!(f, "body {} {{", self.origin.display(f.db))?;

        for (_, local) in self.locals.iter() {
            writeln!(f, "    {}", local.display(f.db))?;
        }

        for (_, block) in self.blocks.iter() {
            writeln!(f, "\n{}", block.display(f.db))?;
        }

        write!(f, "}}")
    }
}

impl Display for Local {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "_{}", u32::from(self.0.into_raw()))
    }
}

impl HirDisplay for LocalData {
    fn hir_fmt(&self, f: &mut HirFormatter) -> std::fmt::Result {
        write!(f, "{} {} :: {}", self.kind, self.id, self.ty.display(f.db))
    }
}

impl Display for LocalKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | Self::Arg => f.write_str("arg"),
            | Self::Var => f.write_str("var"),
            | Self::Tmp => f.write_str("tmp"),
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "%{}", u32::from(self.0.into_raw()))
    }
}

impl HirDisplay for BlockData {
    fn hir_fmt(&self, f: &mut HirFormatter) -> std::fmt::Result {
        if self.params.is_empty() {
            write!(f, "{}:", self.id)?;
        } else {
            write!(f, "{}(", self.id)?;

            for (i, p) in self.params.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                write!(f, "{p}")?;
            }

            write!(f, ")")?;
        }

        for stmt in self.stmts.iter() {
            write!(f, "\n    {}", stmt.display(f.db))?;
        }

        write!(f, "\n    {}", self.term.display(f.db))
    }
}

impl HirDisplay for Term {
    fn hir_fmt(&self, f: &mut HirFormatter) -> std::fmt::Result {
        match self {
            | Self::Unreachable => f.write_str("unreachable"),
            | Self::Abort => f.write_str("abort"),
            | Self::Return(op) => write!(f, "return {}", op.display(f.db)),
            | Self::Jump(t) => write!(f, "jump {}", t.display(f.db)),
            | Self::Switch { discr, values, targets } => {
                write!(f, "switch {} ", discr.display(f.db))?;

                for (val, target) in values.iter().zip(targets) {
                    write!(f, "case {val}: {}, ", target.display(f.db))?;
                }

                write!(f, "default: {}", targets.last().unwrap().display(f.db))
            },
        }
    }
}

impl HirDisplay for JumpTarget {
    fn hir_fmt(&self, f: &mut HirFormatter) -> std::fmt::Result {
        if self.args.is_empty() {
            write!(f, "{}", self.block)
        } else {
            write!(f, "{}(", self.block)?;

            for (i, a) in self.args.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                a.hir_fmt(f)?;
            }

            f.write_char(')')
        }
    }
}

impl HirDisplay for Stmt {
    fn hir_fmt(&self, f: &mut HirFormatter) -> std::fmt::Result {
        match self {
            | Self::Init(l) => write!(f, "init {l}"),
            | Self::Drop(l) => write!(f, "drop {l}"),
            | Self::Assign(p, v) => write!(f, "{} = {}", p.display(f.db), v.display(f.db)),
            | Self::SetDiscriminant(p, c) => write!(f, "discriminant {} = {}", p.display(f.db), c.name(f.db)),
            | Self::Call { place, func, args } => {
                write!(f, "{} = {}(", place.display(f.db), func.display(f.db))?;

                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    a.hir_fmt(f)?;
                }

                f.write_char(')')
            },
        }
    }
}

impl HirDisplay for Rvalue {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result {
        match self {
            | Self::Use(op) => op.hir_fmt(f),
            | Self::Ref(p) => write!(f, "&{}", p.display(f.db)),
            | Self::Discriminant(p) => write!(f, "discriminant {}", p.display(f.db)),
            | Self::BodyRef(b) => write!(f, "body_ref {b}"),
            | Self::DefRef(DefWithBody::Func(d)) => write!(f, "func_ref {}", d.link_name(f.db).0),
            | Self::DefRef(DefWithBody::Static(d)) => write!(f, "static_ref {}", d.link_name(f.db).0),
            | Self::DefRef(DefWithBody::Const(d)) => write!(f, "const_ref {}", d.path(f.db)),
        }
    }
}

impl HirDisplay for Operand {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result {
        match self {
            | Self::Copy(p) => write!(f, "copy {}", p.display(f.db)),
            | Self::Move(p) => write!(f, "move {}", p.display(f.db)),
            | Self::Const(c) => write!(f, "{c}"),
        }
    }
}

impl HirDisplay for Place {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result {
        for proj in self.projection.iter().rev() {
            match proj {
                | Projection::Deref => f.write_char('*')?,
                | Projection::Field(_) => {},
                | Projection::Downcast(_) => f.write_char('(')?,
            }
        }

        write!(f, "{}", self.local)?;

        for proj in self.projection.iter() {
            match proj {
                | Projection::Deref => {},
                | Projection::Field(i) => write!(f, ".{i}")?,
                | Projection::Downcast(c) => write!(f, " as {})", c.name(f.db))?,
            }
        }

        Ok(())
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | Self::Unit => f.write_str("()"),
            | Self::Int(v) => Display::fmt(v, f),
            | Self::Float(v) => Debug::fmt(&f64::from_bits(*v), f),
            | Self::Char(v) => Debug::fmt(v, f),
            | Self::String(v) => Debug::fmt(v, f),
        }
    }
}
