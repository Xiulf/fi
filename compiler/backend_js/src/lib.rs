mod adt;
mod expr;
mod indent;

use std::io::{self, BufWriter, Write};
use std::sync::Arc;

use adt::Repr;
use arena::ArenaMap;
use hir::db::HirDatabase;
use hir::id::DefWithBodyId;
use hir::{AsName, TypeCtor};
use rustc_hash::FxHashMap;

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
    ty_ctor_reprs: FxHashMap<TypeCtor, Repr>,
}

struct BodyCtx<'a, 'b> {
    ctx: &'a mut Ctx<'b>,
    owner: DefWithBodyId,
    body: Arc<hir::Body>,
    infer: Arc<hir::InferenceResult<hir::ty::Ty, hir::ty::Constraint>>,
    locals: ArenaMap<hir::PatId, expr::JsExpr>,
    records: Vec<expr::JsExpr>,
    in_lambda: Vec<String>,
}

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn HirDatabase, out: &'a mut dyn Write) -> Self {
        Self {
            db,
            out: indent::IndentWriter::new(BufWriter::new(out)),
            ty_ctor_reprs: FxHashMap::default(),
        }
    }

    pub fn codegen(&mut self, module: hir::Module) -> io::Result<()> {
        let decls = module.declarations(self.db);
        let members = module.members(self.db);

        if !decls.is_empty() || !members.is_empty() {
            writeln!(self, "(function($shade) {{")?;
            self.out.indent();
            writeln!(
                self,
                r#"const $module = $shade["{0}"] || ($shade["{0}"] = {{}})"#,
                module.name(self.db)
            )?;

            for def in decls {
                self.codegen_def(def)?;
            }

            for member in members {
                self.codegen_member(member)?;
            }

            self.out.dedent();
            writeln!(self, "}})(this.$shade || (this.$shade = {{}}));")?;
        }

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
        let id = hir::id::FuncId::from(func);

        if !func.is_foreign(self.db) && func.has_body(self.db) {
            self.codegen_body(id.into(), Some(func.name(self.db)))?;
            writeln!(self, "")?;
        }

        if !func.is_intrinsic(self.db) && func.is_exported(self.db) {
            if func.is_foreign(self.db) {
                let link_name = if let Some(name) = self.db.attrs(id.into()).by_key("link_name").string_value().next() {
                    name.as_name()
                } else {
                    func.name(self.db)
                };

                writeln!(self, "$module.{} = {};", func.name(self.db), link_name)?;
            } else if func.has_body(self.db) {
                writeln!(self, "$module.{0} = {0};", func.name(self.db))?;
            }
        }

        if self.db.attrs(id.into()).by_key("main").exists() {
            writeln!(self, "$shade.main = {}", func.name(self.db))?;
        }

        Ok(())
    }

    pub fn codegen_const(&mut self, const_: hir::Const) -> io::Result<()> {
        write!(self, "const {} = ", const_.name(self.db))?;
        self.codegen_body_expr(hir::id::ConstId::from(const_).into())?;
        writeln!(self, ";")?;
        writeln!(self, "$module.{0} = {0};", const_.name(self.db))
    }

    pub fn codegen_static(&mut self, static_: hir::Static) -> io::Result<()> {
        write!(self, "const {} = {{ $: ", static_.name(self.db))?;
        self.codegen_body_expr(hir::id::StaticId::from(static_).into())?;
        writeln!(self, " }};")?;
        writeln!(self, "$module.{0} = {0};", static_.name(self.db))
    }

    pub fn codegen_member(&mut self, member: hir::Member) -> io::Result<()> {
        let items = member.items(self.db);

        if !items.is_empty() {
            writeln!(self, "const {} = {{", member.link_name(self.db))?;
            self.out.indent();

            for item in items {
                write!(self, "{}: ", item.name(self.db))?;

                match item {
                    | hir::AssocItem::Func(id) => self.codegen_body(hir::id::FuncId::from(id).into(), None)?,
                    | hir::AssocItem::Static(id) => self.codegen_body_expr(hir::id::StaticId::from(id).into())?,
                }

                writeln!(self, ",")?;
            }

            self.out.dedent();
            writeln!(self, "}};")?;

            if member.is_exported(self.db) {
                writeln!(self, "$module.{0} = {0};", member.link_name(self.db))?;
            }
        }

        Ok(())
    }

    pub fn codegen_body(&mut self, owner: DefWithBodyId, name: Option<hir::Name>) -> io::Result<()> {
        write!(self, "function {}(", name.as_ref().map(AsRef::as_ref).unwrap_or(""))?;
        let mut bcx = BodyCtx::new(self, owner);
        let body = bcx.body.clone();
        let mut ty = bcx.infer.self_type.ty;

        loop {
            match ty.lookup(bcx.db) {
                | hir::ty::TyKind::ForAll(_, inner, _) => ty = inner,
                | hir::ty::TyKind::Where(clause, inner) => {
                    for ctnt in clause.constraints.iter() {
                        let class = bcx.db.class_data(ctnt.class);

                        if !class.items.is_empty() {
                            let name = format!("$r{}", bcx.records.len());
                            let record = expr::JsExpr::Ident { name: name.clone() };

                            write!(bcx, "{}, ", name)?;
                            bcx.records.push(record);
                        }
                    }

                    ty = inner;
                },
                | _ => break,
            }
        }

        for (i, &pat) in body.params().iter().enumerate() {
            let name = format!("$p{}", u32::from(pat.into_raw()));
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
        write!(self, ";\n}}")
    }

    pub fn codegen_body_expr(&mut self, owner: DefWithBodyId) -> io::Result<()> {
        let mut bcx = BodyCtx::new(self, owner);
        bcx.lower(false)
    }
}

impl<'a, 'b> BodyCtx<'a, 'b> {
    pub fn new(ctx: &'a mut Ctx<'b>, owner: DefWithBodyId) -> Self {
        Self {
            body: ctx.db.body(owner),
            infer: ctx.db.infer(owner),
            locals: ArenaMap::default(),
            records: Vec::new(),
            in_lambda: Vec::new(),
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
