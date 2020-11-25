use crate::ir::*;
use std::fmt::{Display, Formatter, Result, Write};

pub trait MirDisplay {
    type Disp<'a>: Display;
    fn display<'a>(&'a self, db: &'a dyn crate::MirDatabase) -> Self::Disp<'a>;
}

impl<'a, T: MirDisplay> MirDisplay for &'a T {
    type Disp<'b> = T::Disp<'b>;

    fn display<'b>(&'b self, db: &'b dyn crate::MirDatabase) -> Self::Disp<'b> {
        T::display(self, db)
    }
}

macro_rules! display {
    ($ty:ty: $name:ident($s:ident, $db:ident, $f:ident) { $($stmts:stmt);* }) => {
        pub struct $name<'a>(&'a $ty, &'a dyn crate::MirDatabase);

        impl MirDisplay for $ty {
            type Disp<'a> = $name<'a>;

            fn display<'a>(&'a self, db: &'a dyn crate::MirDatabase) -> $name<'a> {
                $name(self, db)
            }
        }

        impl Display for $name<'_> {
            fn fmt(&self, $f: &mut Formatter) -> Result {
                let $name($s, $db) = *self;

                $($stmts)*
            }
        }
    }
}

display!(Module: ModuleDisplay(s, db, f) {
    for body in &s.bodies {
        writeln!(f, "{}", body.display(db))?;
    };

    Ok(())
});

display!(Body: BodyDisplay(s, db, f) {
    let file = db.module_tree(s.def.lib).file(s.def.module);
    let module = db.module_hir(file);
    let def = module.def(s.def);

    match def {
        hir::ir::Def::Item(item) => write!(f, "{}.{}", module.name, item.name),
        hir::ir::Def::TraitItem(item) => write!(f, "{}.{}", module.name, item.name),
        hir::ir::Def::ImplItem(item) => write!(f, "{}.{}", module.name, item.name),
    }?;

    writeln!(f, " {{")?;

    for local in &s.locals {
        writeln!(f, "{}", local.display(db))?;
    };

    for block in &s.blocks {
        writeln!(f, "{}", block.display(db))?;
    };

    write!(f, "}}")
});

impl Display for Local {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "_{}", self.raw())
    }
}

display!(LocalData: LocalDisplay(s, db, f) {
    match &s.kind {
        LocalKind::Ret => write!(f, "ret "),
        LocalKind::Arg => write!(f, "arg "),
        LocalKind::Tmp => write!(f, "tmp "),
        LocalKind::Var => write!(f, "var ")
    }?;

    write!(f, "{} :: {}", s.id, s.ty.display(db.to_ty_db()))
});

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "%{}", self.raw())
    }
}

display!(BlockData: BlockDisplay(s, db, f) {
    write!(f, "{}:", s.id)?;

    for stmt in &s.stmts {
        writeln!(f)?;
        write!(indent(f), "{}", stmt.display(db))?;
    };

    Ok(())
});

display!(Stmt: StmtDisplay(s, db, f) {
    match s {
        Stmt::Nop => write!(f, "nop"),
        Stmt::VarLive(l) => write!(f, "lifetime_start {}", l),
        Stmt::VarDead(l) => write!(f, "lifetime_end {}", l),
        Stmt::Assign(p, r) => write!(f, "{} = {}", p.display(db), r.display(db)),
        Stmt::SetDiscr(p, i) => write!(f, "set_discr {}, {}", p.display(db), i),
    }
});

display!(Term: TermDisplay(s, db, f) {
    match s {
        Term::Unset => write!(f, "unset"),
        Term::Abort => write!(f, "abort"),
        Term::Return => write!(f, "return"),
        Term::Jump(to) => write!(f, "jump {}", to),
        Term::Call(place, op, args, to) => write!(
            f, "{} = call {}({}), {}",
            place.display(db),
            op.display(db),
            list(args, ", ", db),
            to,
        ),
        Term::Switch(op, vals, tos) => write!(
            f, "switch {} [{}, otherwise {}]",
            op.display(db),
            vals.iter().zip(tos)
                .map(|(v, t)| format!("{}: {}", v, t))
                .collect::<Vec<_>>().join(", "),
            tos.last().unwrap(),
        ),
    }
});

display!(RValue: RValueDisplay(s, db, f) {
    match s {
        RValue::Use(op) => op.display(db).fmt(f),
        RValue::Ref(place) => write!(f, "addrof {}", place.display(db)),
        RValue::Discr(place) => write!(f, "get_discr {}", place.display(db)),
        RValue::Init(ty, ops) => write!(f, "init {}({})", ty.display(db.to_ty_db()), list(ops, ", ", db)),
    }
});

display!(Operand: OperandDisplay(s, db, f) {
    match s {
        Operand::Copy(p) => write!(f, "copy {}", p.display(db)),
        Operand::Move(p) => write!(f, "move {}", p.display(db)),
        Operand::Const(c, ty) => write!(f, "({} :: {})", c.display(db), ty.display(db.to_ty_db())),
    }
});

display!(Place: PlaceDisplay(s, db, f) {
    for elem in &s.elems {
        match elem {
            PlaceElem::Deref => write!(f, "("),
            PlaceElem::Downcast(_) => write!(f, "("),
            _ => Ok(()),
        }?
    };

    match &s.base {
        PlaceBase::Local(l) => write!(f, "{}", l),
        PlaceBase::Static(id) => {
            let file = db.module_tree(id.lib).file(id.module);
            let module = db.module_hir(file);
            let def = module.def(*id);

            match def {
                hir::ir::Def::Item(item) => write!(f, "{}.{}", module.name, item.name),
                hir::ir::Def::TraitItem(item) => write!(f, "{}.{}", module.name, item.name),
                hir::ir::Def::ImplItem(item) => write!(f, "{}.{}", module.name, item.name),
            }
        },
    }?;

    for elem in &s.elems {
        match elem {
            PlaceElem::Deref => write!(f, ")"),
            PlaceElem::Field(i) => write!(f, ".{}", i),
            PlaceElem::Index(op) => write!(f, "[{}]", op.display(db)),
            PlaceElem::Downcast(i) => write!(f, " as {})", i),
        }?
    };

    Ok(())
});

display!(Const: ConstDisplay(s, db, f) {
    match s {
        Const::Undefined => write!(f, "undefined"),
        Const::Ref(c) => write!(f, "&{}", c.display(db)),
        Const::Tuple(cs) => write!(f, "({})", list(cs, ", ", db)),
        Const::Array(cs) => write!(f, "[{}]", list(cs, ", ", db)),
        Const::Scalar(s) => write!(f, "{:#X}", s),
        Const::FuncAddr(id) => {
            let file = db.module_tree(id.lib).file(id.module);
            let module = db.module_hir(file);
            let def = module.def(*id);

            match def {
                hir::ir::Def::Item(item) => write!(f, "{}.{}", module.name, item.name),
                hir::ir::Def::TraitItem(item) => write!(f, "{}.{}", module.name, item.name),
                hir::ir::Def::ImplItem(item) => write!(f, "{}.{}", module.name, item.name),
            }
        },
        Const::Bytes(b) => write!(f, "{:?}", String::from_utf8_lossy(b)),
    }
});

fn list(
    i: impl IntoIterator<Item = impl MirDisplay>,
    sep: &str,
    db: &dyn crate::MirDatabase,
) -> String {
    i.into_iter()
        .map(|i| i.display(db).to_string())
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
