use crate::*;
use std::fmt::{Display, Formatter, Result, Write};

impl Display for Package<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (i, (id, item)) in self.items.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }

            write!(f, "{}: {}", id, item)?;
        }

        Ok(())
    }
}

impl Display for Item<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} :: ", self.name)?;

        match &self.kind {
            ItemKind::Extern(ty) => write!(f, "extern {}", ty),
            ItemKind::Global(ty, val) => write!(f, "var {} = {}", ty, val),
            ItemKind::Body(body) => body.fmt(f),
        }
    }
}

impl Display for Body<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "fn ({}) {{", list(self.params().map(|p| p.id), ", "))?;

        for local in self.locals.values() {
            writeln!(indent(f), "{}", local)?;
        }

        for block in self.blocks.values() {
            writeln!(f, "{}", block)?;
        }

        write!(f, "}}")
    }
}

impl Display for LocalId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "_{}", self.0)
    }
}

impl Display for BlockId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "%{}", self.0)
    }
}

impl Display for Local<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

impl Display for Block<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}:", self.id)?;

        for stmt in &self.stmts {
            writeln!(f)?;
            write!(indent(f), "{}", stmt)?;
        }

        write!(f, "\n    {}", self.term)
    }
}

impl Display for Stmt<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Stmt::Nop => write!(f, "nop"),
            Stmt::Assign(place, rvalue) => write!(f, "{} = {}", place, rvalue),
            Stmt::VarLive(var) => write!(f, "VarLive({})", var),
            Stmt::VarDead(var) => write!(f, "VarDead({})", var),
        }
    }
}

impl Display for Term<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Term::Unset => write!(f, "unset"),
            Term::Abort => write!(f, "abort"),
            Term::Return => write!(f, "return"),
            Term::Jump(to) => write!(f, "jump {}", to),
            Term::Switch(op, vals, targets) => {
                write!(f, "switch {} [", op)?;

                for (val, target) in vals.iter().zip(targets.iter()) {
                    write!(f, "{}: {}, ", val, target)?;
                }

                write!(f, "otherwise {}]", targets.last().unwrap())
            }
            Term::Call(place, func, args, target) => write!(
                f,
                "call {} = {}({}), {}",
                place,
                func,
                list(args, ", "),
                target
            ),
        }
    }
}

impl Display for Operand<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Operand::Place(place) => place.fmt(f),
            Operand::Const(const_) => const_.fmt(f),
        }
    }
}

impl Display for Place<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.base {
            PlaceBase::Local(id) => id.fmt(f)?,
            PlaceBase::Global(id) => id.fmt(f)?,
        }

        for elem in &self.elems {
            match elem {
                PlaceElem::Deref => write!(f, ".deref")?,
                PlaceElem::Field(idx) => write!(f, ".{}", idx)?,
                PlaceElem::Index(idx) => write!(f, "[{}]", idx)?,
                PlaceElem::Slice(lo, hi) => write!(f, "[{}..{}]", lo, hi)?,
            }
        }

        Ok(())
    }
}

impl Display for Const<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Const::Undefined => write!(f, "undefined"),
            Const::Ref(to) => write!(f, "ref {}", to),
            Const::Tuple(vals) => write!(f, "({})", list(vals, ", ")),
            Const::Array(vals) => write!(f, "[{}]", list(vals, ", ")),
            Const::Scalar(val, ty) => write!(f, "({}: {})", val, ty),
            Const::FuncAddr(id) => write!(f, "{}", id),
            Const::Type(ty) => write!(f, "`{}`", ty),
            Const::Bytes(bytes) => write!(f, "{:?}", String::from_utf8_lossy(bytes)),
        }
    }
}

impl Display for RValue<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            RValue::Use(op) => op.fmt(f),
            RValue::Ref(place) => write!(f, "ref {}", place),
            RValue::Cast(ty, op) => write!(f, "{}.({})", op, ty),
            RValue::BinOp(op, lhs, rhs) => write!(f, "{:?} {} {}", op, lhs, rhs),
            RValue::UnOp(op, rhs) => write!(f, "{:?} {}", op, rhs),
            RValue::Init(ty, variant, ops) => {
                write!(f, "{}.{} {{ {} }}", ty, variant, list(ops, ", "))
            }
        }
    }
}

fn list(i: impl IntoIterator<Item = impl Display>, sep: &str) -> String {
    i.into_iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(sep)
}

fn indent<'a, W: Write>(f: &'a mut W) -> Indent<'a, W> {
    Indent(f, true, "    ")
}

struct Indent<'a, W: Write>(&'a mut W, bool, &'a str);

impl<'a, W: Write> Write for Indent<'a, W> {
    fn write_str(&mut self, s: &str) -> Result {
        for c in s.chars() {
            if c == '\n' {
                self.0.write_char(c)?;
                self.1 = true;
                continue;
            }

            if self.1 {
                self.0.write_str(self.2)?;
                self.1 = false;
            }

            self.0.write_char(c)?;
        }

        Ok(())
    }
}
