use super::*;
use std::fmt::{Debug, Formatter, Result, Write};

impl Debug for DefIndex {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "#{:X}", self.0.wrapping_mul(3).wrapping_add(self.1))
    }
}

impl Debug for DefId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "L{}{:?}", self.lib.0, self.index)
    }
}

impl Debug for HirId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}.{:?}", self.owner, self.local_id)
    }
}

impl Debug for LocalId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for BodyId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self.0)
    }
}

impl Debug for TraitItemId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self.0)
    }
}

impl Debug for ImplItemId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self.0)
    }
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Module name = {:?}", &**self.name.symbol)?;

        for attr in &self.attrs {
            writeln!(f)?;
            write!(indent(f), "{:?}", attr)?;
        }

        for export in &self.exports {
            writeln!(f)?;
            write!(indent(f), "{:?}", export)?;
        }

        for (_, item) in &self.items {
            writeln!(f)?;
            write!(indent(f), "{:?}", item)?;
        }

        for (_, item) in &self.trait_items {
            writeln!(f)?;
            write!(indent(f), "{:?}", item)?;
        }

        for (_, item) in &self.impl_items {
            writeln!(f)?;
            write!(indent(f), "{:?}", item)?;
        }

        for (_, body) in &self.bodies {
            writeln!(f)?;
            write!(indent(f), "{:?}", body)?;
        }

        Ok(())
    }
}

impl Debug for Export {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(
            f,
            "Export name = {:?}, module = {:?}, ns = {:?}",
            &**self.name, self.module, self.ns
        )?;
        write!(indent(f), "{:?}", self.res)
    }
}

impl Debug for Item {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Item::")?;

        match &self.kind {
            ItemKind::Foreign { ty, kind } => {
                writeln!(
                    f,
                    "Foreign id = {:?}, name = {:?}, kind = {:?}",
                    self.id, &**self.name.symbol, kind
                )?;
                write!(indent(f), "{:?}", ty)
            }
            ItemKind::Func { ty, body } => {
                writeln!(
                    f,
                    "Func id = {:?}, name = {:?}, body = {:?}",
                    self.id, &**self.name.symbol, body
                )?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", ty)
            }
            ItemKind::Const { ty, body } => {
                writeln!(
                    f,
                    "Const id = {:?}, name = {:?}, body = {:?}",
                    self.id, &**self.name.symbol, body
                )?;
                write!(indent(f), "{:?}", ty)
            }
            ItemKind::Static { ty, body } => {
                writeln!(
                    f,
                    "Static id = {:?}, name = {:?}, body = {:?}",
                    self.id, &**self.name.symbol, body
                )?;
                write!(indent(f), "{:?}", ty)
            }
            ItemKind::Alias { kind, vars, value } => {
                writeln!(
                    f,
                    "Alias id = {:?}, name = {:?}",
                    self.id, &**self.name.symbol
                )?;

                for var in vars {
                    writeln!(indent(f), "{:?}", var)?;
                }

                writeln!(indent(f), "{:?}", value)?;
                write!(indent(f), "{:?}", kind)
            }
            ItemKind::Data { head, body } => {
                writeln!(
                    f,
                    "Data id = {:?}, name = {:?}",
                    self.id, &**self.name.symbol
                )?;
                write!(indent(f), "{:?}", head)?;

                for ctor in body {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", ctor)?;
                }

                Ok(())
            }
            ItemKind::Trait { head, body } => {
                writeln!(
                    f,
                    "Trait id = {:?}, name = {:?}",
                    self.id, &**self.name.symbol
                )?;
                writeln!(indent(f), "{:?}", head)?;
                write!(indent(f), "{:?}", body)
            }
            ItemKind::Impl {
                chain,
                index,
                head,
                body,
            } => {
                writeln!(
                    f,
                    "Impl id = {:?}, name = {:?}",
                    self.id, &**self.name.symbol
                )?;
                writeln!(f, "           index = {:?}, chain = {:?}", index, chain)?;
                writeln!(indent(f), "{:?}", head)?;
                write!(indent(f), "{:?}", body)
            }
        }
    }
}

impl Debug for DataHead {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "DataHead id = {:?}", self.id)?;

        for var in &self.vars {
            writeln!(indent(f), "{:?}", var)?;
        }

        write!(indent(f), "{:?}", self.kind)
    }
}

impl Debug for DataCtor {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "DataCtor id = {:?}, name = {:?}",
            self.id, &**self.name.symbol
        )?;

        for ty in &self.tys {
            writeln!(f)?;
            write!(indent(f), "{:?}", ty)?;
        }

        Ok(())
    }
}

impl Debug for TraitHead {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "TraitHead id = {:?}", self.id)?;

        for cs in &self.parent {
            writeln!(f)?;
            write!(indent(f), "{:?}", cs)?;
        }

        for var in &self.vars {
            writeln!(f)?;
            write!(indent(f), "{:?}", var)?;
        }

        Ok(())
    }
}

impl Debug for TraitBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "TraitBody id = {:?}", self.id)?;

        for item in &self.items {
            writeln!(f)?;
            write!(indent(f), "{:?}", item)?;
        }

        Ok(())
    }
}

impl Debug for TraitItemRef {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "TraitItemRef id = {:?}, name = {:?}, kind = {:?}",
            self.id, &**self.name.symbol, self.kind
        )
    }
}

impl Debug for ImplHead {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "ImplHead id = {:?}, trait = {:?}", self.id, self.trait_)?;

        for c in &self.cs {
            writeln!(f)?;
            write!(indent(f), "{:?}", c)?;
        }

        for ty in &self.tys {
            writeln!(f)?;
            write!(indent(f), "{:?}", ty)?;
        }

        Ok(())
    }
}

impl Debug for ImplBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "ImplBody id = {:?}", self.id)?;

        for item in &self.items {
            writeln!(f)?;
            write!(indent(f), "{:?}", item)?;
        }

        Ok(())
    }
}

impl Debug for ImplItemRef {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "ImplItemRef id = {:?}, name = {:?}, kind = {:?}",
            self.id, &**self.name.symbol, self.kind
        )
    }
}

impl Debug for TraitItem {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "TraitItem::")?;

        match &self.kind {
            IfaceItemKind::Func { ty } => {
                writeln!(
                    f,
                    "Func id = {:?}, name = {:?}",
                    self.id, &**self.name.symbol
                )?;
                writeln!(indent(f), "{:?}", ty)
            }
        }
    }
}

impl Debug for ImplItem {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "ImplItem::")?;

        match &self.kind {
            ImplItemKind::Func { ty, body } => {
                writeln!(
                    f,
                    "Func id = {:?}, name = {:?}, body = {:?}",
                    self.id, &**self.name.symbol, body
                )?;
                writeln!(indent(f), "{:?}", ty)
            }
        }
    }
}

impl Debug for Body {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "Body id = {:?}", self.id)?;

        for param in &self.params {
            writeln!(indent(f), "{:?}", param)?;
        }

        write!(indent(f), "{:?}", self.value)
    }
}

impl Debug for Param {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Param id = {:?}", self.id)
    }
}

impl Debug for Res {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Res::")?;

        match self {
            Res::Error => write!(f, "Error"),
            Res::Def(kind, id) => write!(f, "Def kind = {:?}, id = {:?}", kind, id),
            Res::Local(id) => write!(f, "Local id = {:?}", id),
        }
    }
}

impl Debug for Pat {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Pat::")?;

        match &self.kind {
            PatKind::Error => write!(f, "Error id = {:?}", self.id),
            PatKind::Wildcard => write!(f, "Wildcard id = {:?}", self.id),
            PatKind::Int { val } => write!(f, "Int id = {:?}, val = {:?}", self.id, val),
            PatKind::Float { bits } => write!(f, "Float id = {:?}, bits = {:?}", self.id, bits),
            PatKind::Char { val } => write!(f, "Char id = {:?}, val = {:?}", self.id, val),
            PatKind::Str { val } => write!(f, "Str id = {:?}, val = {:?}", self.id, val),
            PatKind::Bind { name, sub } => {
                write!(f, "Bind id = {:?}, name = {:?}", self.id, &**name.symbol)?;

                if let Some(sub) = sub {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", sub)?;
                }

                Ok(())
            }
            PatKind::Ctor { ctor, pats } => {
                write!(f, "Ctor id = {:?}, ctor = {:?}", self.id, ctor)?;

                for pat in pats {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", pat)?;
                }

                Ok(())
            }
            PatKind::Array { pats } => {
                write!(f, "Array id = {:?}", self.id)?;

                for pat in pats {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", pat)?;
                }

                Ok(())
            }
            PatKind::Tuple { pats } => {
                write!(f, "Tuple id = {:?}", self.id)?;

                for pat in pats {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", pat)?;
                }

                Ok(())
            }
            PatKind::Record { fields } => {
                write!(f, "Record id = {:?}", self.id)?;

                for field in fields {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", field)?;
                }

                Ok(())
            }
            PatKind::Typed { pat, ty } => {
                writeln!(f, "Typed id = {:?}", self.id)?;
                writeln!(indent(f), "{:?}", pat)?;
                write!(indent(f), "{:?}", ty)
            }
        }
    }
}

impl<T: Debug> Debug for RecordField<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(
            f,
            "RecordField id = {:?}, name = {:?}",
            self.id, &**self.name.symbol
        )?;
        write!(indent(f), "{:?}", self.val)
    }
}

impl Debug for Guarded {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Guarded::")?;

        match self {
            Guarded::Unconditional(expr) => {
                writeln!(f, "Unconditional")?;
                write!(indent(f), "{:?}", expr)
            }
            Guarded::Guarded(guards) => {
                write!(f, "Guarded")?;

                for guard in guards {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", guard)?;
                }

                Ok(())
            }
        }
    }
}

impl Debug for GuardedExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "GuardedExpr id = {:?}", self.id)?;
        writeln!(indent(f), "{:?}", self.guard)?;
        write!(indent(f), "{:?}", self.val)
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Expr::")?;

        match &self.kind {
            ExprKind::Error => write!(f, "Error id = {:?}", self.id),
            ExprKind::Ident { res } => {
                writeln!(f, "Ident id = {:?}", self.id)?;
                write!(indent(f), "{:?}", res)
            }
            ExprKind::Int { val } => write!(f, "Int id = {:?}, val = {:?}", self.id, val),
            ExprKind::App { base, args } => {
                writeln!(f, "App id = {:?}", self.id)?;
                write!(indent(f), "{:?}", base)?;

                for arg in args {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", arg)?;
                }

                Ok(())
            }
            ExprKind::Case { pred, arms } => {
                write!(f, "Case id = {:?}", self.id)?;

                for expr in pred {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", expr)?;
                }

                for arm in arms {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", arm)?;
                }

                Ok(())
            }
            ExprKind::Do { block } => {
                writeln!(f, "Do id = {:?}", self.id)?;
                write!(indent(f), "{:?}", block)
            }
            _ => unimplemented!(),
        }
    }
}

impl Debug for Block {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Block id = {:?}", self.id)?;

        for stmt in &self.stmts {
            writeln!(f)?;
            write!(indent(f), "{:?}", stmt)?;
        }

        Ok(())
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Stmt::")?;

        match &self.kind {
            StmtKind::Discard { expr } => {
                writeln!(f, "Discard id = {:?}", self.id)?;
                write!(indent(f), "{:?}", expr)
            }
            StmtKind::Bind { binding } => {
                writeln!(f, "Bind id = {:?}", self.id)?;
                write!(indent(f), "{:?}", binding)
            }
        }
    }
}

impl Debug for Binding {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "Binding id = {:?}", self.id)?;
        writeln!(indent(f), "{:?}", self.pat)?;
        writeln!(indent(f), "{:?}", self.val)?;
        write!(indent(f), "{:?}", self.ty)
    }
}

impl Debug for CaseArm {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "CaseArm id = {:?}", self.id)?;

        for pat in &self.pats {
            writeln!(indent(f), "{:?}", pat)?;
        }

        write!(indent(f), "{:?}", self.val)
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Type::")?;

        match &self.kind {
            TypeKind::Error => write!(f, "Error id = {:?}", self.id),
            TypeKind::Infer => write!(f, "Infer id = {:?}", self.id),
            TypeKind::Hole { name } => {
                write!(f, "Hole id = {:?}, name = {:?}", self.id, &**name.symbol)
            }
            TypeKind::Ident { res } => {
                writeln!(f, "Ident id = {:?}", self.id)?;
                write!(indent(f), "{:?}", res)
            }
            TypeKind::Int { val } => write!(f, "Int id = {:?}, val = {:?}", self.id, val),
            TypeKind::App { base, args } => {
                writeln!(f, "App id = {:?}", self.id)?;
                write!(indent(f), "{:?}", base)?;

                for arg in args {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", arg)?;
                }

                Ok(())
            }
            TypeKind::Tuple { tys } => {
                write!(f, "Tuple id = {:?}", self.id)?;

                for ty in tys {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", ty)?;
                }

                Ok(())
            }
            TypeKind::Record { row } => {
                writeln!(f, "Record id = {:?}", self.id)?;
                write!(indent(f), "{:?}", row)
            }
            TypeKind::Func { params, ret } => {
                writeln!(f, "Func id = {:?}", self.id)?;

                for param in params {
                    writeln!(indent(f), "{:?}", param)?;
                }

                write!(indent(f), "{:?}", ret)
            }
            TypeKind::Forall { vars, ty } => {
                writeln!(f, "Forall id = {:?}", self.id)?;

                for var in vars {
                    writeln!(indent(f), "{:?}", var)?;
                }

                write!(indent(f), "{:?}", ty)
            }
            TypeKind::Cons { cs, ty } => {
                writeln!(f, "Cons id = {:?}", self.id)?;
                writeln!(indent(f), "{:?}", cs)?;
                write!(indent(f), "{:?}", ty)
            }
            TypeKind::Kinded { ty, kind } => {
                writeln!(f, "Kinded id = {:?}", self.id)?;
                writeln!(indent(f), "{:?}", ty)?;
                write!(indent(f), "{:?}", kind)
            }
        }
    }
}

impl Debug for Row {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Row id = {:?}", self.id)?;

        for field in &self.fields {
            writeln!(f)?;
            write!(indent(f), "{:?}", field)?;
        }

        if let Some(tail) = &self.tail {
            writeln!(f)?;
            write!(indent(f), "{:?}", tail)?;
        }

        Ok(())
    }
}

impl Debug for RowField {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(
            f,
            "RowField id = {:?}, name = {:?}",
            self.id, &**self.name.symbol
        )?;
        write!(indent(f), "{:?}", self.ty)
    }
}

impl Debug for TypeVar {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(
            f,
            "TypeVar id = {:?}, name = {:?}",
            self.id, &**self.name.symbol
        )?;
        write!(indent(f), "{:?}", self.kind)
    }
}

impl Debug for Constraint {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "Constraint id = {:?}, trait = {:?}",
            self.id, self.trait_
        )?;

        for ty in &self.tys {
            writeln!(f)?;
            write!(indent(f), "{:?}", ty)?;
        }

        Ok(())
    }
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
