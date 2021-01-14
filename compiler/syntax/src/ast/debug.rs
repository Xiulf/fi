use super::*;
use std::fmt::{Debug, Formatter, Result, Write};

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "Module name = {:?}", &**self.name.symbol)?;

        for attr in &self.attrs {
            writeln!(indent(f), "{:?}", attr)?;
        }

        write!(indent(f), "{:?}", self.exports)?;

        for import in &self.imports {
            writeln!(f)?;
            write!(indent(f), "{:?}", import)?;
        }

        for decl in &self.decls {
            writeln!(f)?;
            write!(indent(f), "{:?}", decl)?;
        }

        Ok(())
    }
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Attribute name = {:?}", &**self.name.symbol)?;

        if let Some(body) = &self.body {
            writeln!(f)?;
            write!(indent(f), "{:?}", body)?;
        }

        Ok(())
    }
}

impl Debug for AttrBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "AttrBody")?;

        for arg in &self.args {
            writeln!(f)?;
            write!(indent(f), "{:?}", arg)?;
        }

        Ok(())
    }
}

impl Debug for AttrArg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "AttrArg::")?;

        match self {
            AttrArg::Literal(lit) => write!(f, "Literal val = {}", lit),
            AttrArg::Field(name, val) => {
                write!(f, "Field name = {:?}, val = {}", &**name.symbol, val)
            }
            AttrArg::Call(name, args) => {
                write!(f, "Call name = {:?}", &**name.symbol)?;

                for arg in args {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", arg)?;
                }

                Ok(())
            }
        }
    }
}

impl Debug for Exports {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Exports::")?;

        match self {
            Exports::All => write!(f, "All"),
            Exports::Some(exports) => {
                write!(f, "Some")?;

                for export in exports {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", export)?;
                }

                Ok(())
            }
        }
    }
}

impl Debug for Export {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Export name = {:?}, kind = {:?}", &**self.name.symbol, self.kind)
    }
}

impl Debug for ImportDecl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "ImportDecl module = {:?}", &**self.module.symbol)?;

        if let Some(qual) = &self.qual {
            write!(f, ", qual = {:?}", &**qual.symbol)?;
        }

        if let Some((hiding, names)) = &self.names {
            write!(f, ", hiding = {}", hiding)?;

            for name in names {
                writeln!(f)?;
                write!(indent(f), "{:?}", name)?;
            }
        }

        Ok(())
    }
}

impl Debug for Import {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Import name = {:?}, kind = {:?}", &**self.name.symbol, self.kind)
    }
}

impl Debug for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Decl::")?;

        match &self.kind {
            DeclKind::Foreign { ty, kind } => {
                writeln!(f, "Foreign name = {:?}, kind = {:?}", &**self.name.symbol, kind)?;
                write!(indent(f), "{:?}", ty)
            }
            DeclKind::FuncTy { ty } => {
                writeln!(f, "FuncTy name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", ty)
            }
            DeclKind::Func { pats, val } => {
                writeln!(f, "Func name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                for pat in pats {
                    writeln!(indent(f), "{:?}", pat)?;
                }

                write!(indent(f), "{:?}", val)
            }
            DeclKind::ConstTy { ty } => {
                writeln!(f, "ConstTy name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", ty)
            }
            DeclKind::Const { val } => {
                writeln!(f, "Const name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", val)
            }
            DeclKind::StaticTy { ty } => {
                writeln!(f, "StaticTy name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", ty)
            }
            DeclKind::Static { val } => {
                writeln!(f, "Static name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", val)
            }
            DeclKind::AliasKind { kind } => {
                writeln!(f, "AliasKind name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", kind)
            }
            DeclKind::Alias { vars, ty } => {
                writeln!(f, "Alias name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                for var in vars {
                    writeln!(indent(f), "{:?}", var)?;
                }

                write!(indent(f), "{:?}", ty)
            }
            DeclKind::DataKind { kind } => {
                writeln!(f, "DataKind name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", kind)
            }
            DeclKind::Data { head, body } => {
                writeln!(f, "Data name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", head)?;

                if let Some(body) = body {
                    for ctor in body {
                        writeln!(f)?;
                        write!(indent(f), "{:?}", ctor)?;
                    }
                }

                Ok(())
            }
            DeclKind::Trait { head, body } => {
                writeln!(f, "Trait name = {:?}", &**self.name.symbol)?;

                for attr in &self.attrs {
                    writeln!(indent(f), "{:?}", attr)?;
                }

                write!(indent(f), "{:?}", head)?;

                if let Some(body) = body {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", body)?;
                }

                Ok(())
            }
            DeclKind::ImplChain { impls } => {
                write!(f, "ImplChain")?;

                for attr in &self.attrs {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", attr)?;
                }

                for impl_ in impls {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", impl_)?;
                }

                Ok(())
            }
        }
    }
}

impl Debug for DataHead {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "DataHead")?;

        for var in &self.vars {
            writeln!(f)?;
            write!(indent(f), "{:?}", var)?;
        }

        Ok(())
    }
}

impl Debug for DataCtor {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "DataCtor name = {:?}", &**self.name.symbol)?;

        for ty in &self.tys {
            writeln!(f)?;
            write!(indent(f), "{:?}", ty)?;
        }

        Ok(())
    }
}

impl Debug for TraitHead {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "TraitHead")?;

        if let Some(cs) = &self.parent {
            for c in cs {
                writeln!(f)?;
                write!(indent(f), "{:?}", c)?;
            }
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
        write!(f, "TraitBody")?;

        for decl in &self.decls {
            writeln!(f)?;
            write!(indent(f), "{:?}", decl)?;
        }

        Ok(())
    }
}

impl Debug for TraitDecl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "TraitDecl::")?;

        match &self.kind {
            TraitDeclKind::FuncTy { ty } => {
                writeln!(f, "FuncTy")?;
                write!(indent(f), "{:?}", ty)
            }
        }
    }
}

impl Debug for Impl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "Impl name = {:?}", &**self.head.name.symbol)?;
        write!(indent(f), "{:?}", self.head)?;

        if let Some(body) = &self.body {
            writeln!(f)?;
            write!(indent(f), "{:?}", body)?;
        }

        Ok(())
    }
}

impl Debug for ImplHead {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "ImplHead trait = {:?}", &**self.iface.symbol)?;

        if let Some(cs) = &self.cs {
            for c in cs {
                writeln!(f)?;
                write!(indent(f), "{:?}", c)?;
            }
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
        write!(f, "ImplBody")?;

        for decl in &self.decls {
            writeln!(f)?;
            write!(indent(f), "{:?}", decl)?;
        }

        Ok(())
    }
}

impl Debug for ImplDecl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "ImplDecl::")?;

        match &self.kind {
            ImplDeclKind::FuncTy { ty } => {
                writeln!(f, "FuncTy")?;
                write!(indent(f), "{:?}", ty)
            }
            ImplDeclKind::Func { pats, val } => {
                writeln!(f, "Func")?;

                for pat in pats {
                    writeln!(indent(f), "{:?}", pat)?;
                }

                write!(indent(f), "{:?}", val)
            }
        }
    }
}

impl Debug for Pat {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Pat::")?;

        match &self.kind {
            PatKind::Parens { inner } => {
                writeln!(f, "Parens")?;
                write!(indent(f), "{:?}", inner)
            }
            PatKind::Wildcard => write!(f, "Wildcard"),
            PatKind::Int { val } => write!(f, "Int val = {}", val),
            PatKind::Float { bits } => write!(f, "Float bits = {}", bits),
            PatKind::Char { val } => write!(f, "Char val = {:?}", val),
            PatKind::Str { val } => write!(f, "String val = {:?}", val),
            PatKind::Ident { name } => write!(f, "Ident name = {:?}", &**name.symbol),
            PatKind::Named { name, pat } => {
                writeln!(f, "Named name = {:?}", &**name.symbol)?;
                write!(indent(f), "{:?}", pat)
            }
            PatKind::Ctor { name, pats } => {
                write!(f, "Ctor name = {:?}", &**name.symbol)?;

                for pat in pats {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", pat)?;
                }

                Ok(())
            }
            PatKind::Array { pats } => {
                write!(f, "Array")?;

                for pat in pats {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", pat)?;
                }

                Ok(())
            }
            PatKind::Tuple { pats } => {
                write!(f, "Tuple")?;

                for pat in pats {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", pat)?;
                }

                Ok(())
            }
            PatKind::Record { fields } => {
                write!(f, "Record")?;

                for field in fields {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", field)?;
                }

                Ok(())
            }
            PatKind::Typed { pat, ty } => {
                writeln!(f, "Typed")?;
                writeln!(indent(f), "{:?}", pat)?;
                write!(indent(f), "{:?}", ty)
            }
        }
    }
}

impl<T: Debug> Debug for RecordField<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "RecordField::")?;

        match self {
            RecordField::Pun { name } => write!(f, "Pun name = {:?}", &**name.symbol),
            RecordField::Field { name, val } => {
                writeln!(f, "Field name = {:?}", &**name.symbol)?;
                write!(indent(f), "{:?}", val)
            }
        }
    }
}

impl Debug for Guarded {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Guarded")?;

        match self {
            Guarded::Guarded(guards) => {
                for guard in guards {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", guard)?;
                }

                Ok(())
            }
            Guarded::Unconditional(expr) => {
                writeln!(f, "::Unconditional")?;
                write!(indent(f), "{:?}", expr)
            }
        }
    }
}

impl Debug for GuardedExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "GuardedExpr")?;
        writeln!(indent(f), "{:?}", self.guard)?;
        write!(indent(f), "{:?}", self.val)
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Expr::")?;

        match &self.kind {
            ExprKind::Hole { name } => write!(f, "Hole name = {:?}", &**name.symbol),
            ExprKind::Parens { inner } => {
                writeln!(f, "Inner")?;
                write!(indent(f), "{:?}", inner)
            }
            ExprKind::Ident { name } => write!(f, "Ident name = {:?}", &**name.symbol),
            ExprKind::Int { val } => write!(f, "Int val = {}", val),
            ExprKind::Float { bits } => write!(f, "Float bits = {}", bits),
            ExprKind::Char { val } => write!(f, "Char val = {:?}", val),
            ExprKind::Str { val } => write!(f, "Str val = {:?}", val),
            ExprKind::App { base, arg } => {
                writeln!(f, "App")?;
                writeln!(indent(f), "{:?}", base)?;
                write!(indent(f), "{:?}", arg)?;

                Ok(())
            }
            ExprKind::Array { exprs } => {
                write!(f, "Array")?;

                for expr in exprs {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", expr)?;
                }

                Ok(())
            }
            ExprKind::Tuple { exprs } => {
                write!(f, "Tuple")?;

                for expr in exprs {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", expr)?;
                }

                Ok(())
            }
            ExprKind::Record { fields } => {
                write!(f, "Record")?;

                for field in fields {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", field)?;
                }

                Ok(())
            }
            ExprKind::Field { base, field } => {
                writeln!(f, "Field field = {:?}", &**field.symbol)?;
                write!(indent(f), "{:?}", base)
            }
            ExprKind::Index { base, index } => {
                writeln!(f, "Index")?;
                writeln!(indent(f), "{:?}", base)?;
                write!(indent(f), "{:?}", index)
            }
            ExprKind::Assign { lhs, rhs } => {
                writeln!(f, "Assign")?;
                writeln!(indent(f), "{:?}", lhs)?;
                write!(indent(f), "{:?}", rhs)
            }
            ExprKind::Infix { op, lhs, rhs } => {
                writeln!(f, "Infix op = {:?}", op)?;
                writeln!(indent(f), "{:?}", lhs)?;
                write!(indent(f), "{:?}", rhs)
            }
            ExprKind::Prefix { op, rhs } => {
                writeln!(f, "Prefix op = {:?}", op)?;
                write!(indent(f), "{:?}", rhs)
            }
            ExprKind::Postfix { op, lhs } => {
                writeln!(f, "Postfix op = {:?}", op)?;
                write!(indent(f), "{:?}", lhs)
            }
            ExprKind::Let { bindings, body } => {
                writeln!(f, "Let")?;

                for binding in bindings {
                    writeln!(indent(f), "{:?}", binding)?;
                }

                write!(indent(f), "{:?}", body)
            }
            ExprKind::If { cond, then, else_ } => {
                writeln!(f, "If")?;
                writeln!(indent(f), "{:?}", cond)?;
                writeln!(indent(f), "{:?}", then)?;
                write!(indent(f), "{:?}", else_)
            }
            ExprKind::Case { pred, arms } => {
                write!(f, "Case")?;

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
            ExprKind::Loop { body } => {
                writeln!(f, "Loop")?;
                write!(indent(f), "{:?}", body)
            }
            ExprKind::While { cond, body } => {
                writeln!(f, "While")?;
                writeln!(indent(f), "{:?}", cond)?;
                write!(indent(f), "{:?}", body)
            }
            ExprKind::Break {} => write!(f, "Break"),
            ExprKind::Next {} => write!(f, "Next"),
            ExprKind::Do { block } => {
                writeln!(f, "Do")?;
                write!(indent(f), "{:?}", block)
            }
            ExprKind::Return { val } => {
                writeln!(f, "Return")?;
                write!(indent(f), "{:?}", val)
            }
            ExprKind::Typed { expr, ty } => {
                writeln!(f, "Typed")?;
                writeln!(indent(f), "{:?}", expr)?;
                write!(indent(f), "{:?}", ty)
            }
        }
    }
}

impl Debug for Block {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Block")?;

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
                writeln!(f, "Discard")?;
                write!(indent(f), "{:?}", expr)
            }
            StmtKind::Bind { pat, val } => {
                writeln!(f, "Bind")?;
                writeln!(indent(f), "{:?}", pat)?;
                write!(indent(f), "{:?}", val)
            }
        }
    }
}

impl Debug for LetBinding {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "LetBinding::")?;

        match &self.kind {
            LetBindingKind::Type { name, ty } => {
                writeln!(f, "Type name = {:?}", &**name.symbol)?;
                write!(indent(f), "{:?}", ty)
            }
            LetBindingKind::Value { pat, val } => {
                writeln!(f, "Value")?;
                writeln!(indent(f), "{:?}", pat)?;
                write!(indent(f), "{:?}", val)
            }
        }
    }
}

impl Debug for CaseArm {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "CaseArm")?;

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
            TypeKind::Parens { inner } => {
                writeln!(f, "Parens")?;
                write!(indent(f), "{:?}", inner)
            }
            TypeKind::Hole { name } => write!(f, "Hole name = {:?}", &**name.symbol),
            TypeKind::Int { val } => write!(f, "Int val = {}", val),
            TypeKind::Str { val } => write!(f, "Str val = {:?}", val),
            TypeKind::Ident { name } => write!(f, "Ident name = {:?}", &**name.symbol),
            TypeKind::Qual { module, name } => write!(f, "Qual module = {:?} name = {:?}", &**module.symbol, &**name.symbol),
            TypeKind::App { base, arg } => {
                writeln!(f, "App")?;
                writeln!(indent(f), "{:?}", base)?;
                write!(indent(f), "{:?}", arg)?;

                Ok(())
            }
            TypeKind::Tuple { tys } => {
                write!(f, "Tuple")?;

                for ty in tys {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", ty)?;
                }

                Ok(())
            }
            TypeKind::Func { param, ret } => {
                writeln!(f, "Func")?;
                writeln!(indent(f), "{:?}", param)?;
                write!(indent(f), "{:?}", ret)
            }
            TypeKind::Forall { vars, ret } => {
                writeln!(f, "Forall")?;

                for var in vars {
                    writeln!(indent(f), "{:?}", var)?;
                }

                write!(indent(f), "{:?}", ret)
            }
            TypeKind::Cons { cs, ty } => {
                writeln!(f, "Constrained")?;
                writeln!(indent(f), "{:?}", cs)?;
                write!(indent(f), "{:?}", ty)
            }
            TypeKind::Record { row } => {
                writeln!(f, "Record")?;
                write!(indent(f), "{:?}", row)
            }
            TypeKind::Kinded { ty, kind } => {
                writeln!(f, "Kinded")?;
                writeln!(indent(f), "{:?}", ty)?;
                write!(indent(f), "{:?}", kind)
            }
        }
    }
}

impl Debug for Row {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Row")?;

        for field in &self.fields {
            writeln!(f)?;
            write!(indent(f), "{:?}", field)?;
        }

        if let Some(ty) = &self.tail {
            writeln!(f)?;
            write!(indent(f), "{:?}", ty)?;
        }

        Ok(())
    }
}

impl Debug for RowField {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "RowField name = {:?}", &**self.name.symbol)?;
        write!(indent(f), "{:?}", self.ty)
    }
}

impl Debug for TypeVar {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "TypeVar::")?;

        match self {
            TypeVar::Name { name } => write!(f, "Name name = {:?}", &**name.symbol),
            TypeVar::Kind { name, kind } => {
                writeln!(f, "Kind name = {:?}", &**name.symbol)?;
                write!(indent(f), "{:?}", kind)
            }
        }
    }
}

impl Debug for Constraint {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Constraint")?;

        match self {
            Constraint::CS { iface, tys } => {
                write!(f, " trait = {:?}", &**iface.symbol)?;

                for ty in tys {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", ty)?;
                }

                Ok(())
            }
            Constraint::Parens { inner } => {
                writeln!(f, "::Parens")?;
                write!(indent(f), "{:?}", inner)
            }
        }
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
