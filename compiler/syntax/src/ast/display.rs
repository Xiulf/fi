#![allow(dead_code)]

use super::*;
use std::fmt::{Display, Formatter, Result, Write};

impl Display for Module {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for attr in &self.attrs {
            writeln!(f, "{}", attr)?;
        }

        write!(f, "module {}{} where", self.name, self.exports)?;

        if !self.imports.is_empty() {
            writeln!(f)?;
        }

        for import in &self.imports {
            writeln!(f)?;
            import.fmt(f)?;
        }

        if !self.decls.is_empty() {
            writeln!(f)?;
        }

        for decl in &self.decls {
            writeln!(f)?;
            decl.fmt(f)?;
        }

        Ok(())
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "[{}", self.name)?;

        if let Some(body) = &self.body {
            body.fmt(f)?;
        }

        write!(f, "]")
    }
}

impl Display for AttrBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "(")?;

        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            arg.fmt(f)?;
        }

        write!(f, ")")
    }
}

impl Display for AttrArg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            AttrArg::Literal(lit) => lit.fmt(f),
            AttrArg::Field(name, lit) => write!(f, "{} = {}", name, lit),
            AttrArg::Call(name, args) => {
                write!(f, "{}(", name)?;

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    arg.fmt(f)?;
                }

                write!(f, ")")
            }
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Literal::Int(_, val) => val.fmt(f),
            Literal::Float(_, bits) => f64::from_bits(*bits).fmt(f),
            Literal::Char(_, val) => std::fmt::Debug::fmt(val, f),
            Literal::String(_, val) => std::fmt::Debug::fmt(val, f),
        }
    }
}

impl Display for Exports {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Exports::All => Ok(()),
            Exports::Some(exports) => {
                write!(f, " (")?;

                for (i, export) in exports.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    export.fmt(f)?;
                }

                write!(f, ")")
            }
        }
    }
}

impl Display for Export {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.kind {
            ExportKind::Module => write!(f, "module {}", self.name),
            ExportKind::Group(grp) => {
                write!(f, "{}(", self.name)?;
                grp.fmt(f)?;
                write!(f, ")")
            }
            ExportKind::Any => self.name.fmt(f),
        }
    }
}

impl Display for ExportGroup {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ExportGroup::All => write!(f, ".."),
            ExportGroup::Some(names) => {
                for (i, name) in names.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    name.fmt(f)?;
                }

                Ok(())
            }
        }
    }
}

impl Display for ImportDecl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "import {}", self.module)?;

        if let Some((hiding, names)) = &self.names {
            if *hiding {
                write!(f, " hiding")?;
            }

            write!(f, " (")?;

            for (i, name) in names.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                name.fmt(f)?;
            }

            write!(f, ")")?;
        }

        if let Some(alias) = &self.qual {
            write!(f, " as {}", alias)?;
        }

        Ok(())
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.kind {
            ImportKind::Group(grp) => {
                write!(f, "{}(", self.name)?;
                grp.fmt(f)?;
                write!(f, ")")
            }
            ImportKind::Any => self.name.fmt(f),
        }
    }
}

impl Display for ImportGroup {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ImportGroup::All => write!(f, ".."),
            ImportGroup::Some(names) => {
                for (i, name) in names.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    name.fmt(f)?;
                }

                Ok(())
            }
        }
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for attr in &self.attrs {
            writeln!(f, "{}", attr)?;
        }

        match &self.kind {
            DeclKind::Foreign { .. } => {
                write!(f, "foreign {}", self.name)
            }
            DeclKind::FuncTy { .. } => {
                write!(f, "fn {}", self.name)
            }
            DeclKind::Func { .. } => {
                write!(f, "fn {}", self.name)
            }
            DeclKind::StaticTy { .. } => {
                write!(f, "static {}", self.name)
            }
            DeclKind::Static { .. } => {
                write!(f, "static {}", self.name)
            }
            DeclKind::ConstTy { .. } => {
                write!(f, "const {}", self.name)
            }
            DeclKind::Const { .. } => {
                write!(f, "const {}", self.name)
            }
            DeclKind::Fixity { .. } => {
                write!(f, "fixity {}", self.name)
            }
            DeclKind::AliasKind { .. } => {
                write!(f, "alias {}", self.name)
            }
            DeclKind::Alias { .. } => {
                write!(f, "alias {}", self.name)
            }
            DeclKind::DataKind { .. } => {
                write!(f, "data {}", self.name)
            }
            DeclKind::Data { .. } => {
                write!(f, "data {}", self.name)
            }
            DeclKind::Trait { .. } => {
                write!(f, "trait {}", self.name)
            }
            DeclKind::ImplChain { .. } => {
                write!(f, "impl {}", self.name)
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
