use std::io::{self, BufWriter, Write};
use std::sync::Arc;

use hir::db::HirDatabase;
use hir::{Expr, Literal, Pat, Stmt};

#[no_mangle]
pub fn init(logger: &'static dyn log::Log, max_level: log::LevelFilter) {
    log::set_logger(logger).unwrap();
    log::set_max_level(max_level);
}

#[no_mangle]
pub fn codegen(db: &dyn HirDatabase, module: hir::Module, file: &mut dyn Write) {
    let mut ctx = Ctx::new(db, file);

    ctx.codegen(module).unwrap();
}

struct Ctx<'a> {
    db: &'a dyn HirDatabase,
    out: BufWriter<&'a mut dyn Write>,
    indent: usize,
    should_indent: bool,
}

struct BodyCtx<'a, 'b> {
    ctx: &'a mut Ctx<'b>,
    body: Arc<hir::Body>,
    did_return: bool,
}

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn HirDatabase, out: &'a mut dyn Write) -> Self {
        Self {
            db,
            out: BufWriter::new(out),
            indent: 0,
            should_indent: false,
        }
    }

    pub fn codegen(&mut self, module: hir::Module) -> io::Result<()> {
        writeln!(self, "(function($shade) {{")?;
        self.indent += 1;
        writeln!(
            self,
            r#"const $module = $shade["{0}"] || ($shade["{0}"] = {{}})"#,
            module.name(self.db)
        )?;

        for def in module.declarations(self.db) {
            self.codegen_def(def)?;
        }

        self.indent -= 1;
        writeln!(self, "}})($shade || ($shade = {{}}));")?;
        self.flush()
    }

    pub fn codegen_def(&mut self, def: hir::ModuleDef) -> io::Result<()> {
        match def {
            | hir::ModuleDef::Func(func) => self.codegen_func(func),
            | hir::ModuleDef::Const(const_) => self.codegen_const(const_),
            | hir::ModuleDef::Static(static_) => self.codegen_static(static_),
            | _ => Ok(()),
        }
    }

    pub fn codegen_func(&mut self, func: hir::Func) -> io::Result<()> {
        if !func.is_foreign(self.db) && func.has_body(self.db) {
            writeln!(self, "function {}() {{", func.name(self.db))?;
            self.indent += 1;

            let mut bcx = BodyCtx::new(self, self.db.body(hir::id::FuncId::from(func).into()));
            bcx.lower(true, true)?;

            self.indent -= 1;
            writeln!(self, "}}")?;
        }

        if !func.is_intrinsic(self.db) && (func.is_foreign(self.db) || func.has_body(self.db)) {
            writeln!(self, "$module.{0} = {0};", func.name(self.db))?;
        }

        Ok(())
    }

    pub fn codegen_const(&mut self, const_: hir::Const) -> io::Result<()> {
        writeln!(self, "const {} = undefined;", const_.name(self.db))?;
        writeln!(self, "$module.{0} = {0};", const_.name(self.db))
    }

    pub fn codegen_static(&mut self, static_: hir::Static) -> io::Result<()> {
        writeln!(self, "const {} = {{ $: undefined }};", static_.name(self.db))?;
        writeln!(self, "$module.{0} = {0};", static_.name(self.db))
    }
}

impl<'a, 'b> BodyCtx<'a, 'b> {
    pub fn new(ctx: &'a mut Ctx<'b>, body: Arc<hir::Body>) -> Self {
        Self {
            ctx,
            body,
            did_return: false,
        }
    }

    pub fn lower(&mut self, in_block: bool, should_return: bool) -> io::Result<()> {
        self.lower_expr(self.body.body_expr(), in_block, should_return)?;

        if should_return && !self.did_return {
            let body_expr = self.body.body_expr();
            writeln!(self, "return ${};", u32::from(body_expr.into_raw()))?;
        }

        Ok(())
    }

    pub fn lower_expr(&mut self, expr: hir::ExprId, in_block: bool, should_return: bool) -> io::Result<()> {
        let body = self.body.clone();

        match &body[expr] {
            | Expr::Hole => write!(self, "undefined"),
            | Expr::Typed { expr, .. } => self.lower_expr(*expr, in_block, should_return),
            | Expr::Do { stmts } => self.lower_block(expr, stmts, in_block, should_return),
            | Expr::Lit { lit } => match lit {
                | Literal::Int(l) => write!(self, "{}", l),
                | Literal::Float(l) => write!(self, "{}", l),
                | Literal::Char(l) => write!(self, "{:?}", l),
                | Literal::String(l) => write!(self, "{:?}", l),
            },
            | Expr::Path { path } => match path.as_ident() {
                | Some(ident) => write!(self, "{}", ident),
                | None => {
                    let (ident, module) = path.segments().split_last().unwrap();
                    let module = module.iter().map(|t| t.to_string()).collect::<Vec<_>>().join("/");

                    write!(self, "$shade[\"{}\"].{}", module, ident)
                },
            },
            | Expr::App { mut base, arg } => {
                let mut args = vec![*arg];

                while let Expr::App { base: base2, arg } = body[base] {
                    base = base2;
                    args.push(arg);
                }

                self.lower_expr(base, false, false)?;
                write!(self, "(")?;

                for (i, arg) in args.into_iter().enumerate() {
                    if i > 0 {
                        write!(self, ", ")?;
                    }

                    self.lower_expr(arg, false, false)?;
                }

                write!(self, ")")
            },
            | Expr::If {
                cond,
                then,
                else_: Some(else_),
            } => {
                self.lower_expr(*cond, false, false)?;
                write!(self, " ? ")?;
                self.lower_expr(*then, false, false)?;
                write!(self, " : ")?;
                self.lower_expr(*else_, false, false)
            },
            | Expr::If {
                cond,
                then,
                else_: None,
            } => {
                write!(self, "if (")?;
                self.lower_expr(*cond, false, false)?;
                writeln!(self, ") {{")?;
                self.indent += 1;
                self.lower_expr(*then, true, false)?;
                self.indent -= 1;
                writeln!(self, "}}")
            },
            | e => {
                log::warn!(target: "lower_expr", "not yet implemented: {:?}", e);
                Ok(())
            },
        }
    }

    pub fn lower_block(
        &mut self,
        expr: hir::ExprId,
        stmts: &[Stmt],
        in_block: bool,
        should_return: bool,
    ) -> io::Result<()> {
        if !in_block {
            writeln!(self, "(let ${} = (function() {{", u32::from(expr.into_raw()))?;
        }

        for (i, stmt) in stmts.iter().enumerate() {
            match stmt {
                | Stmt::Expr { expr } if i == stmts.len() - 1 => {
                    if !in_block || should_return {
                        write!(self, "return ")?;

                        if should_return {
                            self.did_return = true;
                        }
                    }

                    self.lower_expr(*expr, false, false)?;
                    writeln!(self, ";")?;
                },
                | Stmt::Expr { expr } => {
                    self.lower_expr(*expr, false, false)?;
                    writeln!(self, ";")?;
                },
                | Stmt::Let { pat, val } => {
                    write!(self, "let ${} = ", u32::from(val.into_raw()))?;
                    self.lower_expr(*val, false, false)?;
                    writeln!(self, ";")?;
                    self.lower_pat(*pat, format!("${}", u32::from(val.into_raw())))?;
                },
                | Stmt::Bind { .. } => {},
            }
        }

        if !in_block {
            writeln!(self, "}})())")?;
        }

        Ok(())
    }

    pub fn lower_pat(&mut self, pat: hir::PatId, expr: String) -> io::Result<()> {
        let body = self.body.clone();

        match &body[pat] {
            | Pat::Missing => Ok(()),
            | Pat::Wildcard => Ok(()),
            | Pat::Bind { name, subpat: None } => {
                writeln!(self, "let {} = {};", name, expr)
            },
            | p => {
                log::warn!(target: "lower_pat", "not yet implemented: {:?}", p);
                Ok(())
            },
        }
    }
}

impl<'a> Write for Ctx<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut start = 0;
        let mut written = 0;

        for cur in 0..buf.len() {
            if buf[cur] == b'\n' {
                self.should_indent = true;
                continue;
            }

            if self.should_indent {
                written += self.out.write(&buf[start..cur])?;
                start = cur;
                self.should_indent = false;

                for _ in 0..self.indent {
                    self.out.write(&[b' ', b' ', b' ', b' '])?;
                }
            }
        }

        if start < buf.len() {
            written += self.out.write(&buf[start..])?;
        }

        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.out.flush()
    }
}

impl<'a, 'b> std::ops::Deref for BodyCtx<'a, 'b> {
    type Target = Ctx<'b>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl<'a, 'b> std::ops::DerefMut for BodyCtx<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}
