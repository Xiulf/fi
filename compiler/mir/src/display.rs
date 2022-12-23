use std::fmt::{Display, Formatter, Result, Write};

use hir::display::HirFormatter;
use hir::id::DefWithBodyId;
use hir::{DefWithBody, HirDisplay};

use crate::repr::{ArrayLen, Integer, Primitive, Repr, Scalar, Signature};
use crate::syntax::*;

impl HirDisplay for Module {
    fn hir_fmt(&self, _f: &mut HirFormatter) -> std::fmt::Result {
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
        write!(f, "{} {} :: {}", self.kind, self.id, self.repr.display(f.db))
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
            | Self::None => f.write_str("<no terminator>"),
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
            | Self::Cast(kind, op) => write!(f, "cast {} ({:?})", op.display(f.db), kind),
            | Self::BodyRef(b) => write!(f, "body_ref {b}"),
            | Self::DefRef(DefWithBody::Func(d)) => write!(f, "func_ref {}", d.link_name(f.db).0),
            | Self::DefRef(DefWithBody::Static(d)) => write!(f, "static_ref {}", d.link_name(f.db).0),
            | Self::DefRef(DefWithBody::Const(d)) => write!(f, "const_ref {}", d.path(f.db)),
            | Self::BinOp(op, lhs, rhs) => write!(f, "{} {} {}", lhs.display(f.db), op, rhs.display(f.db)),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            | Self::Eq => "==",
            | Self::Ne => "!=",

            | Self::Lt => "<",
            | Self::Le => "<=",
            | Self::Gt => ">",
            | Self::Ge => "<=",

            | Self::Lsh => "<<",
            | Self::Rsh => ">>",
            | Self::And => "&",
            | Self::Or => "|",
            | Self::Xor => "^",

            | Self::Add => "+",
            | Self::Sub => "-",
            | Self::Mul => "*",
            | Self::Div => "/",
            | Self::Rem => "%",

            | Self::Offset => "*+",
        };

        f.write_str(s)
    }
}

impl HirDisplay for Operand {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result {
        match self {
            | Self::Copy(p) => write!(f, "copy {}", p.display(f.db)),
            | Self::Move(p) => write!(f, "move {}", p.display(f.db)),
            | Self::Const(c, r) => write!(f, "{} [{}]", c.display(f.db), r.display(f.db)),
        }
    }
}

impl HirDisplay for Place {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result {
        for proj in self.projection.iter().rev() {
            match proj {
                | Projection::Deref => f.write_char('*')?,
                | Projection::Field(_) => {},
                | Projection::Index(_) => {},
                | Projection::Downcast(_) => f.write_char('(')?,
            }
        }

        write!(f, "{}", self.local)?;

        for proj in self.projection.iter() {
            match proj {
                | Projection::Deref => {},
                | Projection::Field(i) => write!(f, ".{i}")?,
                | Projection::Index(i) => write!(f, "[{}]", i.display(f.db))?,
                | Projection::Downcast(c) => write!(f, " as {})", c.name(f.db))?,
            }
        }

        Ok(())
    }
}

impl HirDisplay for Const {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result {
        match self {
            | Self::Undefined => f.write_str("undefined"),
            | Self::Unit => f.write_str("()"),
            | Self::Int(v) => write!(f, "{}", v),
            | Self::Float(v) => write!(f, "{}", f64::from_bits(*v)),
            | Self::Char(v) => write!(f, "{:?}", v),
            | Self::String(v) => write!(f, "{:?}", v),
            | Self::Ctor(c) => write!(f, "{}", c.name(f.db)),
        }
    }
}

impl HirDisplay for Repr {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result {
        match self {
            | Self::Opaque => f.write_str("{opaque}"),
            | Self::ReprOf(ty) => write!(f, "repr_of({})", ty.display(f.db)),
            | Self::Scalar(scalar) => write!(f, "{scalar}"),
            | Self::Struct(fields) => {
                write!(f, "struct {{ ")?;

                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    field.hir_fmt(f)?;
                }

                write!(f, " }}")
            },
            | Self::Enum(fields) => {
                write!(f, "enum {{ ")?;

                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    field.hir_fmt(f)?;
                }

                write!(f, " }}")
            },
            | Self::Array(len, el) => write!(f, "[{}]{}", len.display(f.db), el.display(f.db)),
            | Self::Ptr(to, true, _) => write!(f, "*fat {}", to.display(f.db)),
            | Self::Ptr(to, false, _) => write!(f, "*{}", to.display(f.db)),
            | Self::Box(to) => write!(f, "box({})", to.display(f.db)),
            | Self::Func(sig, false) => write!(f, "fn {}", sig.display(f.db)),
            | Self::Func(sig, true) => write!(f, "lambda {}", sig.display(f.db)),
        }
    }
}

impl HirDisplay for ArrayLen {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result {
        match self {
            | Self::Const(l) => write!(f, "{l}"),
            | Self::TypeVar(v) => v.hir_fmt(f),
        }
    }
}

impl HirDisplay for Signature {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result {
        f.write_char('(')?;

        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                f.write_str(", ")?;
            }

            param.hir_fmt(f)?;
        }

        write!(f, ") -> {}", self.ret.display(f.db))
    }
}

impl Display for Scalar {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.valid_range == (0..=u128::MAX) {
            Display::fmt(&self.value, f)
        } else if *self.valid_range.end() == u128::MAX {
            write!(f, "{} @ {}..", self.value, self.valid_range.start())
        } else {
            write!(
                f,
                "{} @ {}..={}",
                self.value,
                self.valid_range.start(),
                self.valid_range.end()
            )
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | Self::Int(Integer::Int, false) => f.write_str("usize"),
            | Self::Int(Integer::I8, false) => f.write_str("u8"),
            | Self::Int(Integer::I16, false) => f.write_str("u16"),
            | Self::Int(Integer::I32, false) => f.write_str("u32"),
            | Self::Int(Integer::I64, false) => f.write_str("u64"),
            | Self::Int(Integer::I128, false) => f.write_str("u218"),
            | Self::Int(Integer::Int, true) => f.write_str("isize"),
            | Self::Int(Integer::I8, true) => f.write_str("i8"),
            | Self::Int(Integer::I16, true) => f.write_str("i16"),
            | Self::Int(Integer::I32, true) => f.write_str("i32"),
            | Self::Int(Integer::I64, true) => f.write_str("i64"),
            | Self::Int(Integer::I128, true) => f.write_str("i218"),
            | Self::Float => f.write_str("float"),
            | Self::Double => f.write_str("double"),
            | Self::Pointer => f.write_str("ptr"),
        }
    }
}
