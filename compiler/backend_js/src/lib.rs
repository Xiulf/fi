mod expr;
mod indent;

use std::io::{self, BufWriter, Write};
use std::sync::Arc;

use arena::ArenaMap;
use hir::db::HirDatabase;
use hir::id::DefWithBodyId;

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
    out: indent::IndentWriter<BufWriter<&'a mut dyn Write>>,
}

struct BodyCtx<'a, 'b> {
    ctx: &'a mut Ctx<'b>,
    body: Arc<hir::Body>,
    owner: DefWithBodyId,
    locals: ArenaMap<hir::PatId, expr::JsExpr>,
}

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn HirDatabase, out: &'a mut dyn Write) -> Self {
        Self {
            db,
            out: indent::IndentWriter::new(BufWriter::new(out)),
        }
    }

    pub fn codegen(&mut self, module: hir::Module) -> io::Result<()> {
        writeln!(self, "(function($shade) {{")?;
        self.out.indent();
        writeln!(
            self,
            r#"const $module = $shade["{0}"] || ($shade["{0}"] = {{}})"#,
            module.name(self.db)
        )?;

        for def in module.declarations(self.db) {
            self.codegen_def(def)?;
        }

        self.out.dedent();
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
            write!(self, "function {}(", func.name(self.db))?;
            let mut bcx = BodyCtx::new(self, hir::id::FuncId::from(func).into());
            let body = bcx.body.clone();

            for (i, &pat) in body.params().iter().enumerate() {
                let name = format!("${}", u32::from(pat.into_raw()));
                let param = expr::JsExpr::Ident { name: name.clone() };

                bcx.lower_pat(pat, param, &mut Vec::new());

                if i > 0 {
                    write!(bcx, ", ")?;
                }

                write!(bcx, "{}", name)?;
            }

            writeln!(bcx, ") {{")?;
            bcx.out.indent();
            bcx.lower(true)?;
            bcx.out.dedent();
            writeln!(self, ";\n}}")?;
        }

        if !func.is_intrinsic(self.db)
            && (func.is_foreign(self.db) || func.has_body(self.db))
            && func.is_exported(self.db)
        {
            writeln!(self, "$module.{0} = {0};", func.name(self.db))?;
        }

        Ok(())
    }

    pub fn codegen_const(&mut self, const_: hir::Const) -> io::Result<()> {
        write!(self, "const {} = ", const_.name(self.db))?;
        let mut bcx = BodyCtx::new(self, hir::id::ConstId::from(const_).into());
        bcx.lower(false)?;
        writeln!(self, ";")?;
        writeln!(self, "$module.{0} = {0};", const_.name(self.db))
    }

    pub fn codegen_static(&mut self, static_: hir::Static) -> io::Result<()> {
        write!(self, "const {} = {{ $: ", static_.name(self.db))?;
        let mut bcx = BodyCtx::new(self, hir::id::StaticId::from(static_).into());
        bcx.lower(false)?;
        writeln!(self, " }};")?;
        writeln!(self, "$module.{0} = {0};", static_.name(self.db))
    }
}

impl<'a, 'b> BodyCtx<'a, 'b> {
    pub fn new(ctx: &'a mut Ctx<'b>, owner: DefWithBodyId) -> Self {
        Self {
            body: ctx.db.body(owner),
            locals: ArenaMap::default(),
            owner,
            ctx,
        }
    }

    pub fn lower(&mut self, in_block: bool) -> io::Result<()> {
        let expr = self.lower_expr_inline(self.body.body_expr());
        let ret = if in_block {
            match expr {
                | expr::JsExpr::Block { mut exprs } => {
                    let last = exprs.pop().unwrap();

                    exprs.push(expr::JsExpr::Return { expr: Box::new(last) });
                    expr::JsExpr::Block { exprs }
                },
                | _ => expr::JsExpr::Return { expr: Box::new(expr) },
            }
        } else {
            expr
        };

        ret.write(&mut self.out, in_block)
    }
}

impl<'a> Write for Ctx<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.out.write(buf)
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
