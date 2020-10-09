use crate::*;
use std::fmt::{Display, Formatter, Result, Write};

impl Display for ItemId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "#{:0>8X}",
            (self.0 & 0x00000000FFFFFFFF) & (self.0 >> 32)
        )
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}:{:0>8}", self.0, self.1)
    }
}

impl Display for Package {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "                   | ####### ITEMS #######")?;

        for (id, item) in &self.items {
            write!(f, "\n{} | ", id)?;
            write!(
                f,
                "{}",
                list(item.to_string().lines(), "\n                   | ")
            )?;
        }

        write!(f, "\n                   | ####### EXPRS #######")?;

        for (id, expr) in &self.exprs {
            write!(f, "\n{} | ", id)?;
            write!(
                f,
                "{}",
                list(expr.to_string().lines(), "\n                  | ")
            )?;
        }

        write!(f, "\n                   | ####### PATS  #######")?;

        for (id, pat) in &self.pats {
            write!(f, "\n{} | ", id)?;
            write!(
                f,
                "{}",
                list(pat.to_string().lines(), "\n                  | ")
            )?;
        }

        write!(f, "\n                   | ####### TYPES #######")?;

        for (id, ty) in &self.types {
            write!(f, "\n{} | ", id)?;
            write!(
                f,
                "{}",
                list(ty.to_string().lines(), "\n                   | ")
            )?;
        }

        Ok(())
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.kind {
            ItemKind::Extern { abi, ty } => write!(f, "{} :: extern {}{};", self.name, abi, ty),
            ItemKind::Func {
                generics,
                params,
                ret,
                body,
            } => write!(
                f,
                "{} :: fn{} ({}) -> {} {}",
                self.name,
                generics,
                list(params, ", "),
                ret,
                body
            ),
            ItemKind::Param { ty } => write!(f, "param {}: {};", self.name, ty),
            ItemKind::Var {
                ty,
                val: Some(val),
                global: _,
            } => write!(f, "var {}: {} = {};", self.name, ty, val),
            ItemKind::Var {
                ty,
                val: None,
                global: _,
            } => write!(f, "var {}: {};", self.name, ty),
            ItemKind::Const { ty, val } => write!(f, "const {}: {} = {};", self.name, ty, val),
            ItemKind::Struct {
                generics,
                fields,
                methods,
            } => {
                writeln!(f, "struct {}{}", self.name, generics)?;

                for field in fields {
                    writeln!(indent(f), "{}", field)?;
                }

                for method in methods {
                    writeln!(indent(f), "{}", method)?;
                }

                write!(f, "end")
            }
            ItemKind::Enum {
                generics,
                variants,
                methods,
            } => {
                writeln!(f, "enum {}{}", self.name, generics)?;

                for variant in variants {
                    writeln!(indent(f), "{}", variant)?;
                }

                for method in methods {
                    writeln!(indent(f), "{}", method)?;
                }

                write!(f, "end")
            }
            ItemKind::Method {
                owner,
                generics,
                self_param,
                params,
                ret,
                body,
            } => write!(
                f,
                "{}.fn {}{}(self({}){}) -> {} {}",
                owner,
                self.name,
                generics,
                self_param,
                params
                    .iter()
                    .map(|p| format!(", {}", p))
                    .collect::<Vec<_>>()
                    .join(""),
                ret,
                body
            ),
            ItemKind::Ctor {
                item,
                variant: _,
                params: Some(params),
            } => write!(f, "cons {}({}) -> {}", self.name, list(params, ", "), item),
            ItemKind::Ctor {
                item,
                variant: _,
                params: None,
            } => write!(f, "cons {} -> {}", self.name, item),
        }
    }
}

impl Display for Generics {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.params.is_empty() {
            Ok(())
        } else {
            write!(f, "({})", list(&self.params, ", "))
        }
    }
}

impl Display for Generic {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.name.fmt(f)
    }
}

impl Display for StructField {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl Display for EnumVariant {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} {}", self.name, self.ctor)?;

        if let Some(fields) = &self.fields {
            write!(f, "({})", list(fields, ", "))?;
        }

        Ok(())
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.stmts.is_empty() {
            write!(f, "{{}}")
        } else if self.stmts.len() == 1 {
            if let StmtKind::Expr(id) = &self.stmts[0].kind {
                write!(f, "do {}", id)
            } else {
                writeln!(f, "{{")?;
                writeln!(indent(f), "{}", self.stmts[0])?;
                write!(f, "}}")
            }
        } else {
            writeln!(f, "{{")?;

            for stmt in &self.stmts {
                writeln!(indent(f), "{}", stmt)?;
            }

            write!(f, "}}")
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.kind {
            StmtKind::Item(id) => id.fmt(f),
            StmtKind::Semi(id) => write!(f, "{};", id),
            StmtKind::Expr(id) => id.fmt(f),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.kind {
            ExprKind::Err => write!(f, "[error]"),
            ExprKind::Path { res } => res.fmt(f),
            ExprKind::Apply { expr, args } => write!(f, "{}.<{}>", expr, list(args, ", ")),
            ExprKind::Int { val } => val.fmt(f),
            ExprKind::Float { bits } => f64::from_bits(*bits).fmt(f),
            ExprKind::Char { val } => write!(f, "{:?}", val),
            ExprKind::String { val } => write!(f, "{:?}", val),
            ExprKind::Type { ty } => write!(f, "`{}`", ty),
            ExprKind::Array { exprs } => write!(f, "[{}]", list(exprs, ", ")),
            ExprKind::Tuple { exprs } => write!(f, "({})", list(exprs, ", ")),
            ExprKind::Range { lo, hi } => write!(f, "{}..{}", lo, hi),
            ExprKind::Block { block } => block.fmt(f),
            ExprKind::Call { func, args } => write!(f, "{}({})", func, list(args, ", ")),
            ExprKind::MethodCall { obj, method, args } => {
                write!(f, "{}.{}({})", obj, method, list(args, ", "))
            }
            ExprKind::Field { obj, field } => write!(f, "{}.{}", obj, field),
            ExprKind::Index { list, index } => write!(f, "{}[{}]", list, index),
            ExprKind::Slice {
                list,
                low: Some(l),
                high: Some(h),
            } => write!(f, "{}[{}..{}]", list, l, h),
            ExprKind::Slice {
                list,
                low: Some(l),
                high: None,
            } => write!(f, "{}[{}..]", list, l),
            ExprKind::Slice {
                list,
                low: None,
                high: Some(h),
            } => write!(f, "{}[..{}]", list, h),
            ExprKind::Slice {
                list,
                low: None,
                high: None,
            } => write!(f, "{}[..]", list),
            ExprKind::Ref { expr } => write!(f, "{}.ref", expr),
            ExprKind::Deref { expr } => write!(f, "{}.deref", expr),
            ExprKind::TypeOf { expr } => write!(f, "{}.type", expr),
            ExprKind::Cast { expr, ty } => write!(f, "{}.({})", expr, ty),
            ExprKind::Box { expr } => write!(f, "box {}", expr),
            ExprKind::Unbox { expr } => write!(f, "unbox {}", expr),
            ExprKind::Assign { lhs, rhs } => write!(f, "{} = {}", lhs, rhs),
            ExprKind::BinOp { op, lhs, rhs } => write!(f, "{} {} {}", lhs, op, rhs),
            ExprKind::UnOp { op, rhs } => write!(f, "{}{}", op, rhs),
            ExprKind::IfElse {
                cond,
                then,
                else_: Some(else_),
            } => write!(f, "if {} {} else {}", cond, then, else_),
            ExprKind::IfElse {
                cond,
                then,
                else_: None,
            } => write!(f, "if {} {}", cond, then),
            ExprKind::Match { pred, arms } => {
                writeln!(f, "match {}", pred)?;

                for arm in arms {
                    writeln!(indent(f), "{}", arm)?;
                }

                write!(f, "end")
            }
            ExprKind::While {
                label: Some(label),
                cond,
                body,
            } => write!(f, ":{} while {} {}", label, cond, body),
            ExprKind::While {
                label: None,
                cond,
                body,
            } => write!(f, "while {} {}", cond, body),
            ExprKind::Loop {
                label: Some(label),
                body,
            } => write!(f, ":{} loop {}", label, body),
            ExprKind::Loop { label: None, body } => write!(f, "loop {}", body),
            ExprKind::Break {
                label: Some(label),
                expr: Some(expr),
            } => write!(f, "break :{} {}", label, expr),
            ExprKind::Break {
                label: Some(label),
                expr: None,
            } => write!(f, "break :{}", label),
            ExprKind::Break {
                label: None,
                expr: Some(expr),
            } => write!(f, "break {}", expr),
            ExprKind::Break {
                label: None,
                expr: None,
            } => write!(f, "break"),
            ExprKind::Continue { label: Some(label) } => write!(f, "continue :{}", label),
            ExprKind::Continue { label: None } => write!(f, "continue"),
            ExprKind::Return { expr: Some(expr) } => write!(f, "return {}", expr),
            ExprKind::Return { expr: None } => write!(f, "return"),
            ExprKind::Defer { expr } => write!(f, "defer {}", expr),
        }
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(name) = &self.name {
            write!(f, "{} = ", name)?;
        }

        self.value.fmt(f)
    }
}

impl Display for MatchArm {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}: {}", self.pat, self.value)
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.kind {
            PatKind::Err => write!(f, "[error]"),
            PatKind::Wildcard => write!(f, "_"),
            PatKind::Bind {
                var,
                inner: Some(id),
            } => write!(f, "{} @ {}", var, id),
            PatKind::Bind { var, inner: None } => write!(f, "{}", var),
            PatKind::Ctor { id, pats } => write!(f, "{}({})", id, list(pats, ", ")),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.kind {
            TypeKind::Err => write!(f, "[error]"),
            TypeKind::Infer => write!(f, "_"),
            TypeKind::Path { res } => res.fmt(f),
            TypeKind::Ptr {
                kind: PtrKind::Single,
                to,
            } => write!(f, "*{}", to),
            TypeKind::Ptr {
                kind: PtrKind::Multiple(true),
                to,
            } => write!(f, "[*:0]{}", to),
            TypeKind::Ptr {
                kind: PtrKind::Multiple(false),
                to,
            } => write!(f, "[*]{}", to),
            TypeKind::Array { of, len } => write!(f, "[{}]{}", len, of),
            TypeKind::Slice { of } => write!(f, "[]{}", of),
            TypeKind::Tuple { tys } => write!(
                f,
                "({})",
                tys.iter()
                    .map(|t| format!("{},", t))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            TypeKind::Func { params, ret } => write!(f, "fn ({}) -> {}", list(params, ", "), ret),
            TypeKind::Subst { ty, args } => write!(f, "{}({})", ty, list(args, ", ")),
            TypeKind::Forall { gen, ty } => write!(f, "forall {}. {}", gen, ty),
        }
    }
}

impl Display for TypeParam {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl Display for Res {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Res::Module(id) => id.fmt(f),
            Res::Item(id) => id.fmt(f),
            Res::Local(id) => id.fmt(f),
            Res::Label(id) => id.fmt(f),
            Res::PrimVal(prim) => prim.fmt(f),
            Res::PrimTy(prim) => prim.fmt(f),
        }
    }
}

impl Display for PrimVal {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            PrimVal::True => write!(f, "true"),
            PrimVal::False => write!(f, "false"),
            PrimVal::Undefined => write!(f, "undefined"),
        }
    }
}

impl Display for PrimTy {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            PrimTy::Never => write!(f, "never"),
            PrimTy::Bool => write!(f, "bool"),
            PrimTy::Str => write!(f, "str"),
            PrimTy::Int(255, false) => write!(f, "uint"),
            PrimTy::Int(255, true) => write!(f, "int"),
            PrimTy::Float(255) => write!(f, "float"),
            PrimTy::Int(0, false) => write!(f, "usize"),
            PrimTy::Int(0, true) => write!(f, "isize"),
            PrimTy::Int(bits, false) => write!(f, "u{}", bits),
            PrimTy::Int(bits, true) => write!(f, "i{}", bits),
            PrimTy::Float(bits) => write!(f, "f{}", bits),
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
