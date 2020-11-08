use super::*;
use std::fmt::{Debug, Formatter, Result, Write};

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "Module name = {:?}", &**self.name.symbol)?;
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
        write!(
            f,
            "Export name = {:?}, kind = {:?}",
            &**self.name.symbol, self.kind
        )
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
        write!(
            f,
            "Import name = {:?}, kind = {:?}",
            &**self.name.symbol, self.kind
        )?;

        if let Some(alias) = &self.alias {
            write!(f, ", alias = {:?}", &**alias.symbol)?;
        }

        Ok(())
    }
}

impl Debug for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Decl::")?;

        match &self.kind {
            DeclKind::FuncTy { ty } => {
                writeln!(f, "FuncTy name = {:?}", &**self.name.symbol)?;
                write!(indent(f), "{:?}", ty)
            }
            DeclKind::Func { pats, val } => {
                writeln!(f, "Func name = {:?}", &**self.name.symbol)?;

                for pat in pats {
                    writeln!(indent(f), "{:?}", pat)?;
                }

                write!(indent(f), "{:?}", val)
            }
            DeclKind::AliasKind { kind } => {
                writeln!(f, "AliasKind name = {:?}", &**self.name.symbol)?;
                write!(indent(f), "{:?}", kind)
            }
            DeclKind::Alias { vars, ty } => {
                writeln!(f, "Alias name = {:?}", &**self.name.symbol)?;

                for var in vars {
                    writeln!(indent(f), "{:?}", var)?;
                }

                write!(indent(f), "{:?}", ty)
            }
            DeclKind::Iface { head, body } => {
                writeln!(f, "Iface name = {:?}", &**self.name.symbol)?;
                write!(indent(f), "{:?}", head)?;

                if let Some(body) = body {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", body)?;
                }

                Ok(())
            }
            DeclKind::ImplChain { impls } => {
                write!(f, "ImplChain name = {:?}", &**self.name.symbol)?;

                for impl_ in impls {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", impl_)?;
                }

                Ok(())
            }
        }
    }
}

impl Debug for IfaceHead {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "IfaceHead")?;

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

impl Debug for IfaceBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "IfaceBody")?;

        for decl in &self.decls {
            writeln!(f)?;
            write!(indent(f), "{:?}", decl)?;
        }

        Ok(())
    }
}

impl Debug for IfaceDecl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "IfaceDecl::")?;

        match &self.kind {
            IfaceDeclKind::FuncTy { ty } => {
                writeln!(f, "FuncTy")?;
                write!(indent(f), "{:?}", ty)
            }
        }
    }
}

impl Debug for Impl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "Impl")?;
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
        write!(f, "ImplHead iface = {:?}", &**self.iface.symbol)?;

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
            PatKind::Ident { name } => write!(f, "Ident name = {:?}", &**name.symbol),
            PatKind::App { base, args } => {
                writeln!(f, "App")?;
                write!(indent(f), "{:?}", base)?;

                for arg in args {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", arg)?;
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
            ExprKind::Parens { inner } => {
                writeln!(f, "Inner")?;
                write!(indent(f), "{:?}", inner)
            }
            ExprKind::Ident { name } => write!(f, "Ident name = {:?}", &**name.symbol),
            ExprKind::Int { val } => write!(f, "Int val = {}", val),
            ExprKind::Float { bits } => write!(f, "Float bits = {}", bits),
            ExprKind::Char { val } => write!(f, "Char val = {:?}", val),
            ExprKind::Str { val } => write!(f, "Str val = {:?}", val),
            ExprKind::App { base, args } => {
                writeln!(f, "App")?;
                write!(indent(f), "{:?}", base)?;

                for arg in args {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", arg)?;
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
            ExprKind::Typed { expr, ty } => {
                writeln!(f, "Typed")?;
                writeln!(indent(f), "{:?}", expr)?;
                write!(indent(f), "{:?}", ty)
            }
        }
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
            TypeKind::Hole => write!(f, "Hole"),
            TypeKind::Var { name } => write!(f, "Var name = {:?}", &**name.symbol),
            TypeKind::Ident { name } => write!(f, "Ident name = {:?}", &**name.symbol),
            TypeKind::App { base, args } => {
                writeln!(f, "App")?;
                write!(indent(f), "{:?}", base)?;

                for arg in args {
                    writeln!(f)?;
                    write!(indent(f), "{:?}", arg)?;
                }

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
            TypeKind::Func { params, ret } => {
                writeln!(f, "Func")?;

                for param in params {
                    writeln!(indent(f), "{:?}", param)?;
                }

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
                write!(f, " iface = {:?}", &**iface.symbol)?;

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
