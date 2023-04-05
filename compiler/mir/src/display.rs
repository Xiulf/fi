use std::fmt::Write;

use hir_def::display::HirDisplay;
use salsa::AsId;

use crate::instance::{ImplInstance, ImplSource, Instance, InstanceId};
use crate::ir::{
    BinOp, Block, BlockData, Body, Const, JumpTarget, Linkage, Local, LocalData, LocalKind, MirValueId, NullOp,
    Operand, Place, Projection, RValue, Stmt, Term, ValueDef,
};
use crate::repr::{ArrayLen, Integer, Primitive, Repr, Scalar, Signature};
use crate::Db;

impl HirDisplay for Instance {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        self.id(f.db).hir_fmt(f)?;

        if let Some(subst) = self.subst(f.db) {
            f.write_char('<')?;
            f.with_upcast::<_, dyn hir_ty::Db>(|d| d, |f| f.write_joined(subst.types.iter(), ", "))?;

            if !subst.impls.is_empty() {
                write!(f, "; ")?;
                f.write_joined(subst.impls.iter(), ", ")?;
            }

            f.write_char('>')?;
        }

        Ok(())
    }
}

impl HirDisplay for ImplInstance {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        write!(f, "{}", hir::Impl::from(self.id(f.db)).vtable_link_name(f.db))?;
        Ok(())
    }
}

impl HirDisplay for ImplSource {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match *self {
            | Self::Instance(i) => i.hir_fmt(f),
            | Self::Param(i) => write!(f, "${i}"),
        }
    }
}

impl HirDisplay for InstanceId {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match *self {
            | Self::MirValueId(id) => id.hir_fmt(f),
            | Self::VtableMethod(id, vtable, method) => {
                write!(f, "{}.vtable.{vtable}.method.{method}", id.display(f.db))
            },
            | Self::Body(body) => write!(f, "{}", body),
        }
    }
}

impl HirDisplay for MirValueId {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match *self {
            | Self::Lambda(id, expr) => write!(
                f,
                "{}.#{:0>4}",
                hir::Value::from(id).link_name(f.db),
                u32::from(expr.into_raw())
            ),
            | Self::ValueId(id) => write!(f, "{}", hir::Value::from(id).link_name(f.db)),
            | Self::CtorId(id) => write!(f, "{}", hir::Ctor::from(id).link_name(f.db)),
            | Self::FieldId(_) => todo!(),
        }
    }
}

impl HirDisplay for ValueDef {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        write!(f, "{} {}", self.linkage(f.db), self.name(f.db))?;

        if let Some(body) = self.body(f.db) {
            write!(f, " = {}", body)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for Linkage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Local => f.write_str("local "),
            | Self::Export => f.write_str("export"),
            | Self::Import => f.write_str("import"),
        }
    }
}

impl std::fmt::Display for Body {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.as_id().as_u32())
    }
}

impl HirDisplay for Body {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        writeln!(f, "body @{} {{", self.as_id().as_u32())?;

        for (i, constraint) in self.constraints(f.db).iter().enumerate() {
            writeln!(f, "    ${} = where {}", i, constraint.display(f.db))?;
        }

        if !self.constraints(f.db).is_empty() {
            writeln!(f)?;
        }

        for (id, local) in self.locals(f.db).iter() {
            writeln!(f, "    {} :: {}", Local(id), local.display(f.db))?;
        }

        for (id, block) in self.blocks(f.db).iter() {
            writeln!(f, "\n{}{}", Block(id), block.display(f.db))?;
        }

        write!(f, "}}")
    }
}

impl std::fmt::Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", u32::from(self.0.into_raw()))
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", u32::from(self.0.into_raw()))
    }
}

impl HirDisplay for LocalData {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        write!(f, "{} {}", self.kind, self.repr.display(f.db))
    }
}

impl std::fmt::Display for LocalKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Arg => f.write_str("arg"),
            | Self::Var => f.write_str("var"),
            | Self::Tmp => f.write_str("tmp"),
        }
    }
}

impl HirDisplay for BlockData {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        if self.params.is_empty() {
            write!(f, ":")?;
        } else {
            write!(f, "(")?;
            for (i, p) in self.params.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                write!(f, "{p}")?;
            }
            write!(f, "):")?;
        }

        for stmt in self.stmts.iter() {
            write!(f, "\n    {}", stmt.display(f.db))?;
        }

        write!(f, "\n    {}", self.term.display(f.db))
    }
}

impl HirDisplay for Term {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match self {
            | Term::None => f.write_str("<no terminator>"),
            | Term::Unreachable => f.write_str("unreachable"),
            | Term::Abort => f.write_str("abort"),
            | Term::Return(op) => write!(f, "return {}", op.display(f.db)),
            | Term::Jump(target) => write!(f, "jump {}", target.display(f.db)),
            | Term::Switch { discr, values, targets } => {
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
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        if self.args.is_empty() {
            write!(f, "{}", self.block)
        } else {
            write!(f, "{}(", self.block)?;
            f.write_joined(self.args.iter(), ", ")?;
            write!(f, ")")
        }
    }
}

impl HirDisplay for Stmt {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match self {
            | Self::Init(l) => write!(f, "init {l}"),
            | Self::Drop(l) => write!(f, "drop {}", l.display(f.db)),
            | Self::Assign(p, v) => write!(f, "{} = {}", p.display(f.db), v.display(f.db)),
            | Self::SetDiscriminant(p, c) => write!(
                f,
                "discriminant {} = {}",
                p.display(f.db),
                hir::Ctor::from(*c).name(f.db).display(f.db)
            ),
            | Self::Intrinsic { place, name, args } => {
                write!(f, "{} = {:?}(", place.display(f.db), name)?;

                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    a.hir_fmt(f)?;
                }

                f.write_char(')')
            },
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

impl HirDisplay for RValue {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match self {
            | Self::Use(op) => op.hir_fmt(f),
            | Self::AddrOf(p) => write!(f, "&{}", p.display(f.db)),
            | Self::Cast(kind, op) => write!(f, "cast {} ({:?})", op.display(f.db), kind),
            | Self::BinOp(op, lhs, rhs) => write!(f, "{} {} {}", lhs.display(f.db), op, rhs.display(f.db)),
            | Self::NullOp(op, repr) => write!(f, "{} {}", op, repr.display(f.db)),
            | Self::Discriminant(p) => write!(f, "discriminant {}", p.display(f.db)),
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl std::fmt::Display for NullOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            | Self::SizeOf => "sizeof",
            | Self::AlignOf => "alignof",
            | Self::StrideOf => "strideof",
        };

        f.write_str(s)
    }
}

impl HirDisplay for Operand {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match self {
            | Self::Copy(p) => write!(f, "copy({})", p.display(f.db)),
            | Self::Move(p) => write!(f, "move({})", p.display(f.db)),
            // | Self::Const(c, r) => write!(f, "{} [{}]", c.display(f.db), r.display(f.db)),
            | Self::Const(c, _) => write!(f, "{}", c.display(f.db)),
        }
    }
}

impl HirDisplay for Place {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        for proj in self.projection.iter().rev() {
            match proj {
                | Projection::Deref => f.write_str("(*")?,
                | Projection::Field(_) => {},
                | Projection::Index(_) => {},
                | Projection::Slice(_, _) => {},
                | Projection::Downcast(_) => f.write_char('(')?,
            }
        }

        write!(f, "{}", self.local)?;

        for proj in self.projection.iter() {
            match proj {
                | Projection::Deref => f.write_char(')')?,
                | Projection::Field(i) => write!(f, ".{i}")?,
                | Projection::Index(i) => write!(f, "[{}]", i.display(f.db))?,
                | Projection::Slice(l, h) => write!(f, "[{}..{}]", l.display(f.db), h.display(f.db))?,
                | Projection::Downcast(c) => write!(f, " as {})", hir::Ctor::from(*c).name(f.db).display(f.db))?,
            }
        }

        Ok(())
    }
}

impl HirDisplay for Const {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match self {
            | Self::Undefined => f.write_str("undefined"),
            | Self::Zeroed => f.write_str("{{zeroed}}"),
            | Self::Unit => f.write_str("()"),
            | Self::Int(v) => write!(f, "{}", v),
            | Self::Float(v) => write!(f, "{}", f64::from_bits(*v)),
            | Self::Char(v) => write!(f, "{:?}", v),
            | Self::String(v) => write!(f, "{:?}", v),
            | Self::Ctor(c) => write!(f, "{}", hir::Ctor::from(*c).name(f.db).display(f.db)),
            | Self::Instance(i) => write!(f, "{}", i.display(f.db)),
            // | Self::TypeVar(v) => write!(f, "${}", v.display(f.db)),
        }
    }
}

impl HirDisplay for Repr {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match self {
            | Self::Uninhabited => f.write_str("{uninhabited}"),
            | Self::Opaque => f.write_str("{opaque}"),
            | Self::TypeVar(v) => write!(f, "{}", v.display(f.db)),
            | Self::ReprOf(t) => write!(f, "repr_of({})", t.display(f.db)),
            | Self::Scalar(scalar) => write!(f, "{scalar}"),
            | Self::Array(len, el) => write!(f, "{} * {}", len.display(f.db), el.display(f.db)),
            | Self::Ptr(el, true, _) => write!(f, "*fat {}", el.display(f.db)),
            | Self::Ptr(el, false, _) => write!(f, "*{}", el.display(f.db)),
            | Self::Box(el) => write!(f, "box({})", el.display(f.db)),
            | Self::Func(sig, None) => write!(f, "fn {}", sig.display(f.db)),
            | Self::Func(sig, Some(env)) => write!(f, "lambda [{}] {}", env.display(f.db), sig.display(f.db)),
            | Self::Discr(repr) => write!(f, "discriminant({})", repr.display(f.db)),
            | Self::Struct(fields) if fields.is_empty() => write!(f, "()"),
            | Self::Struct(fields) => {
                write!(f, "struct {{ ")?;
                f.write_joined(fields.iter(), ", ")?;
                write!(f, " }}")
            },
            | Self::Enum(variants) => {
                write!(f, "enum {{ ")?;
                f.write_joined(variants.iter(), ", ")?;
                write!(f, " }}")
            },
        }
    }
}

impl HirDisplay for ArrayLen {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        match self {
            | Self::Const(c) => write!(f, "{c}"),
            | Self::TypeVar(v) => write!(f, "{}", v.display(f.db)),
        }
    }
}

impl HirDisplay for Signature {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        f.write_char('(')?;
        f.write_joined(self.params.iter(), ", ")?;
        write!(f, ") -> {}", self.ret.display(f.db))
    }
}

impl std::fmt::Display for Scalar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.valid_range == (0..=u128::MAX) {
            write!(f, "{}", self.value)
        } else if *self.valid_range.end() == u128::MAX {
            write!(f, "{} @ {}..", self.value, self.valid_range.start())
        } else {
            write!(
                f,
                "{} @ {}..{}",
                self.value,
                self.valid_range.start(),
                self.valid_range.end()
            )
        }
    }
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Int(Integer::Int, false) => f.write_str("usize"),
            | Self::Int(Integer::I8, false) => f.write_str("u8"),
            | Self::Int(Integer::I16, false) => f.write_str("u16"),
            | Self::Int(Integer::I32, false) => f.write_str("u32"),
            | Self::Int(Integer::I64, false) => f.write_str("u64"),
            | Self::Int(Integer::I128, false) => f.write_str("u128"),
            | Self::Int(Integer::Int, true) => f.write_str("isize"),
            | Self::Int(Integer::I8, true) => f.write_str("i8"),
            | Self::Int(Integer::I16, true) => f.write_str("i16"),
            | Self::Int(Integer::I32, true) => f.write_str("i32"),
            | Self::Int(Integer::I64, true) => f.write_str("i64"),
            | Self::Int(Integer::I128, true) => f.write_str("i128"),
            | Self::Float => f.write_str("float"),
            | Self::Double => f.write_str("double"),
            | Self::Pointer => f.write_str("ptr"),
        }
    }
}
