use crate::*;
use std::fmt::{Display, Formatter, Result, Write};

pub struct MirDisplay<'mir, 'tcx> {
    mir: &'mir Package<'tcx>,
    tcx: &'mir check::tcx::Tcx<'tcx>,
}

pub struct ConstDisplay<'mir, 'tcx> {
    const_: &'mir Const<'tcx>,
    tcx: &'mir check::tcx::Tcx<'tcx>,
}

impl<'tcx> Package<'tcx> {
    pub fn display<'mir>(&'mir self, tcx: &'mir check::tcx::Tcx<'tcx>) -> MirDisplay<'mir, 'tcx> {
        MirDisplay { mir: self, tcx }
    }
}

impl<'tcx> Const<'tcx> {
    pub fn display<'mir>(&'mir self, tcx: &'mir check::tcx::Tcx<'tcx>) -> ConstDisplay<'mir, 'tcx> {
        ConstDisplay { const_: self, tcx }
    }
}

impl<'mir, 'tcx> Display for MirDisplay<'mir, 'tcx> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (i, (_, item)) in self.mir.items.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }

            self.fmt_item(item, f)?;
        }

        Ok(())
    }
}

impl<'mir, 'tcx> Display for ConstDisplay<'mir, 'tcx> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.const_ {
            Const::Undefined => write!(f, "undefined"),
            Const::Ref(to) => write!(f, "&{}", to),
            Const::Tuple(vals) => write!(
                f,
                "({})",
                vals.iter()
                    .map(|v| format!("{},", v.display(self.tcx)))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Const::Array(vals) => write!(
                f,
                "[{}]",
                vals.iter()
                    .map(|v| v.display(self.tcx).to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Const::Scalar(val) => write!(f, "{}", val),
            Const::FuncAddr(id) => write!(f, "{}", self.tcx.get_full_name(id)),
            Const::Type(ty) => ty.display(self.tcx).fmt(f),
            Const::Bytes(bytes) => write!(f, "{:?}", String::from_utf8_lossy(bytes)),
        }
    }
}

impl<'mir, 'tcx> MirDisplay<'mir, 'tcx> {
    fn fmt_item(&self, item: &'mir Item<'tcx>, f: &mut Formatter) -> Result {
        match &item.kind {
            ItemKind::Extern(ty) => write!(f, "extern {}: {};", item.name, ty.display(self.tcx)),
            ItemKind::Global(ty, val) => write!(
                f,
                "var {}: {} = {};",
                item.name,
                ty.display(self.tcx),
                val.display(self.tcx)
            ),
            ItemKind::Body(body) => self.fmt_body(item.name, body, f),
        }
    }

    fn fmt_body(&self, name: Ident, body: &'mir Body<'tcx>, f: &mut Formatter) -> Result {
        writeln!(
            f,
            "fn {}({})",
            name,
            list(body.params().map(|p| p.id), ", ")
        )?;

        for local in body.locals.values() {
            self.fmt_local(local, indent(f))?;
            writeln!(f)?;
        }

        for block in body.blocks.values() {
            self.fmt_block(block, f)?;
            writeln!(f)?;
        }

        write!(f, "end")
    }

    fn fmt_local(&self, local: &'mir Local<'tcx>, mut f: Indent<Formatter>) -> Result {
        match local.kind {
            LocalKind::Ret => write!(f, "ret")?,
            LocalKind::Arg => write!(f, "arg")?,
            LocalKind::Tmp => write!(f, "tmp")?,
            LocalKind::Var => write!(f, "var")?,
        }

        write!(f, " {}: {}", local.id, local.ty.display(self.tcx))
    }

    fn fmt_block(&self, block: &'mir Block<'tcx>, f: &mut Formatter) -> Result {
        write!(f, "{}:", block.id)?;

        for stmt in &block.stmts {
            writeln!(f)?;
            self.fmt_stmt(stmt, &mut indent(f))?;
        }

        writeln!(f)?;
        self.fmt_term(&block.term, &mut indent(f))
    }

    fn fmt_stmt(&self, stmt: &'mir Stmt<'tcx>, f: &mut Indent<Formatter>) -> Result {
        match stmt {
            Stmt::Nop => write!(f, "nop"),
            Stmt::Assign(place, rvalue) => {
                self.fmt_place(place, f)?;
                write!(f, " = ")?;
                self.fmt_rvalue(rvalue, f)
            }
            Stmt::VarLive(var) => write!(f, "VarLive({})", var),
            Stmt::VarDead(var) => write!(f, "VarDead({})", var),
        }
    }

    fn fmt_term(&self, term: &'mir Term<'tcx>, f: &mut Indent<Formatter>) -> Result {
        match term {
            Term::Unset => write!(f, "unset"),
            Term::Abort => write!(f, "abort"),
            Term::Return => write!(f, "return"),
            Term::Jump(to) => write!(f, "jump {}", to),
            Term::Switch(op, vals, targets) => {
                write!(f, "switch ")?;
                self.fmt_op(op, f)?;
                write!(f, " [")?;

                for (val, target) in vals.iter().zip(targets.iter()) {
                    write!(f, "{}: {}, ", val, target)?;
                }

                write!(f, "otherwise {}]", targets.last().unwrap())
            }
            Term::Call(place, func, args, target) => {
                write!(f, "call ")?;
                self.fmt_place(place, f)?;
                write!(f, " = ")?;
                self.fmt_op(func, f)?;
                write!(f, "(")?;

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    self.fmt_op(arg, f)?;
                }

                write!(f, "), {}", target)
            }
        }
    }

    fn fmt_op(&self, op: &'mir Operand<'tcx>, f: &mut Indent<Formatter>) -> Result {
        match op {
            Operand::Copy(place) => {
                write!(f, "copy(")?;
                self.fmt_place(place, f)?;
                write!(f, ")")
            }
            Operand::Move(place) => {
                write!(f, "move(")?;
                self.fmt_place(place, f)?;
                write!(f, ")")
            }
            Operand::Const(const_, _) => write!(f, "{}", const_.display(self.tcx)),
        }
    }

    fn fmt_place(&self, place: &'mir Place<'tcx>, f: &mut Indent<Formatter>) -> Result {
        match &place.base {
            PlaceBase::Local(id) => write!(f, "{}", id)?,
            PlaceBase::Global(id) => write!(f, "{}", self.tcx.get_full_name(id))?,
        }

        for elem in &place.elems {
            match elem {
                PlaceElem::Deref => write!(f, ".*")?,
                PlaceElem::Field(idx) => write!(f, ".{}", idx)?,
                PlaceElem::Index(idx) => {
                    write!(f, "[")?;
                    self.fmt_op(idx, f)?;
                    write!(f, "]")?;
                }
                PlaceElem::Slice(lo, hi) => {
                    write!(f, "[")?;
                    self.fmt_op(lo, f)?;
                    write!(f, "..")?;
                    self.fmt_op(hi, f)?;
                    write!(f, "]")?
                }
                PlaceElem::AsVariant(idx) => write!(f, ".({})", idx)?,
            }
        }

        Ok(())
    }

    fn fmt_rvalue(&self, rvalue: &'mir RValue<'tcx>, f: &mut Indent<Formatter>) -> Result {
        match rvalue {
            RValue::Use(op) => self.fmt_op(op, f),
            RValue::Ref(place) => {
                write!(f, "&")?;
                self.fmt_place(place, f)
            }
            RValue::Cast(ty, op, _) => {
                self.fmt_op(op, f)?;
                write!(f, ".({})", ty.display(self.tcx))
            }
            RValue::BinOp(op, lhs, rhs) => {
                write!(f, "{:?} ", op)?;
                self.fmt_op(lhs, f)?;
                write!(f, " ")?;
                self.fmt_op(rhs, f)
            }
            RValue::UnOp(op, rhs) => {
                write!(f, "{:?} ", op)?;
                self.fmt_op(rhs, f)
            }
            RValue::Init(ty, variant, ops) => {
                write!(f, "{}.{} {{ ", ty.display(self.tcx), variant)?;

                for (i, op) in ops.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    self.fmt_op(op, f)?;
                }

                write!(f, " }}")
            }
            RValue::Discr(place) => {
                write!(f, "discr(")?;
                self.fmt_place(place, f)?;
                write!(f, ")")
            }
        }
    }
}

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
        match self.kind {
            LocalKind::Ret => write!(f, "ret")?,
            LocalKind::Arg => write!(f, "arg")?,
            LocalKind::Tmp => write!(f, "tmp")?,
            LocalKind::Var => write!(f, "var")?,
        }

        write!(f, " {}: {}", self.id, self.ty)
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
            Operand::Copy(place) => write!(f, "copy({})", place),
            Operand::Move(place) => write!(f, "move({})", place),
            Operand::Const(const_, _) => const_.fmt(f),
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
                PlaceElem::Deref => write!(f, ".*")?,
                PlaceElem::Field(idx) => write!(f, ".{}", idx)?,
                PlaceElem::Index(idx) => write!(f, "[{}]", idx)?,
                PlaceElem::Slice(lo, hi) => write!(f, "[{}..{}]", lo, hi)?,
                PlaceElem::AsVariant(idx) => write!(f, ".({})", idx)?,
            }
        }

        Ok(())
    }
}

impl Display for Const<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Const::Undefined => write!(f, "undefined"),
            Const::Ref(to) => write!(f, "&{}", to),
            Const::Tuple(vals) => write!(f, "({})", list(vals, ", ")),
            Const::Array(vals) => write!(f, "[{}]", list(vals, ", ")),
            Const::Scalar(val) => val.fmt(f),
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
            RValue::Ref(place) => write!(f, "&{}", place),
            RValue::Cast(ty, op, _) => write!(f, "{}.({})", op, ty),
            RValue::BinOp(op, lhs, rhs) => write!(f, "{:?} {} {}", op, lhs, rhs),
            RValue::UnOp(op, rhs) => write!(f, "{:?} {}", op, rhs),
            RValue::Init(ty, variant, ops) => {
                write!(f, "{}.{} {{ {} }}", ty, variant, list(ops, ", "))
            }
            RValue::Discr(place) => write!(f, "discr({})", place),
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
