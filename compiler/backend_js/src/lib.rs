mod adt;
mod expr;
mod indent;
mod intrinsic;
mod pat;

use std::io::{self, BufWriter, Write};
use std::sync::Arc;

use adt::Layout;
use arena::ArenaMap;
use hir::db::HirDatabase;
use hir::id::DefWithBodyId;
use hir::TypeCtor;
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
    ty_ctor_reprs: FxHashMap<TypeCtor, Layout>,
}

struct BodyCtx<'a, 'b> {
    ctx: &'a mut Ctx<'b>,
    owner: DefWithBodyId,
    body: Arc<hir::Body>,
    infer: Arc<hir::InferenceResult<hir::ty::Ty, hir::ty::Constraint>>,
    locals: ArenaMap<hir::PatId, expr::JsExpr>,
    type_vars: Vec<expr::JsExpr>,
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

    fn mangle(&self, (name, mangle): (impl AsRef<str>, bool)) -> String {
        let name = name.as_ref();

        if !mangle {
            return name.to_string();
        }

        let mut res = String::with_capacity(name.len());

        for c in name.chars() {
            match c {
                | '\'' => res.push('$'),
                | '.' => res.push('_'),
                | _ => res.push(c),
            }
        }

        res
    }

    pub fn codegen(&mut self, module: hir::Module) -> io::Result<()> {
        let decls = module.declarations(self.db);
        let members = module.members(self.db);

        for &def in &decls {
            self.register_def(def)?;
        }

        for def in decls {
            self.codegen_def(def)?;
        }

        for member in members {
            self.codegen_member(member)?;
        }

        self.flush()
    }

    pub fn register_def(&mut self, def: hir::ModuleDef) -> io::Result<()> {
        match def {
            | hir::ModuleDef::Func(f) if !f.is_foreign(self.db) && f.has_body(self.db) => {
                writeln!(self, "var {};", self.mangle(f.link_name(self.db)))
            },
            | hir::ModuleDef::Static(s) if !s.is_foreign(self.db) => {
                writeln!(self, "var {};", self.mangle(s.link_name(self.db)))
            },
            | hir::ModuleDef::Const(c) => {
                writeln!(self, "var {};", self.mangle((c.path(self.db).to_string(), true)))
            },
            | _ => Ok(()),
        }
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
            write!(self, "{} = ", self.mangle(func.link_name(self.db)))?;
            self.codegen_body(id.into(), false)?;
            writeln!(self, ";")?;
        }

        if self.db.attrs(id.into()).by_key("main").exists() {
            writeln!(self, "const $main = {}", self.mangle(func.link_name(self.db)))?;
        }

        Ok(())
    }

    pub fn codegen_const(&mut self, const_: hir::Const) -> io::Result<()> {
        write!(self, "{} = ", self.mangle((const_.path(self.db).to_string(), true)))?;
        self.codegen_body_expr(hir::id::ConstId::from(const_).into())?;
        writeln!(self, ";")
    }

    pub fn codegen_static(&mut self, static_: hir::Static) -> io::Result<()> {
        if !static_.is_foreign(self.db) {
            write!(self, "{} = ", self.mangle(static_.link_name(self.db)))?;
            self.codegen_body_expr(hir::id::StaticId::from(static_).into())?;
            writeln!(self, ";")?;
        }

        Ok(())
    }

    pub fn codegen_member(&mut self, member: hir::Member) -> io::Result<()> {
        let items = member.items(self.db);
        let lower = self.db.lower_member(member.into());

        if !items.is_empty() {
            let has_constraints = lower
                .member
                .where_clause
                .constraints
                .iter()
                .any(|c| !self.db.class_data(c.class).items.is_empty());

            if !has_constraints {
                writeln!(self, "const {} = {{", member.link_name(self.db))?;
            } else {
                write!(self, "function {}(", member.link_name(self.db))?;
                let mut i = 0;

                for c in lower.member.where_clause.constraints.iter() {
                    if !self.db.class_data(c.class).items.is_empty() {
                        if i > 0 {
                            write!(self, ", ")?;
                        }

                        write!(self, "$r{}", i)?;
                        i += 1;
                    }
                }

                writeln!(self, ") {{")?;
                self.out.indent();
                writeln!(self, "return {{")?;
            }

            self.out.indent();

            for item in items {
                write!(self, "{}: ", self.mangle((item.name(self.db), true)))?;

                match item {
                    | hir::AssocItem::Func(id) => self.codegen_body(hir::id::FuncId::from(id).into(), true)?,
                    | hir::AssocItem::Static(id) => self.codegen_body_expr(hir::id::StaticId::from(id).into())?,
                }

                writeln!(self, ",")?;
            }

            self.out.dedent();
            writeln!(self, "}};")?;

            if has_constraints {
                self.out.dedent();
                writeln!(self, "}}")?;
            }
        }

        Ok(())
    }

    pub fn codegen_body(&mut self, owner: DefWithBodyId, is_assoc: bool) -> io::Result<()> {
        write!(self, "function(")?;
        let mut skip_where = is_assoc;
        let mut skip_forall = is_assoc;
        let mut bcx = BodyCtx::new(self, owner);
        let mut ty = bcx.infer.self_type.ty;
        let body = bcx.body.clone();

        loop {
            match ty.lookup(bcx.db) {
                | hir::ty::TyKind::ForAll(kinds, inner, _) => {
                    use hir::id::HasModule;
                    let lib = bcx.owner.module(bcx.db.upcast()).lib;
                    let symbol_ctor = bcx.db.lang_item(lib, "symbol-kind".into()).unwrap();
                    let symbol_ctor = symbol_ctor.as_type_ctor().unwrap();

                    for kind in kinds.iter() {
                        if kind.lookup(bcx.db) == hir::ty::TyKind::Ctor(symbol_ctor) {
                            let name = format!("$t{}", bcx.type_vars.len());

                            if !skip_forall {
                                write!(bcx, "{}, ", name)?;
                            }

                            bcx.type_vars.push(expr::JsExpr::Ident { name });
                        }
                    }

                    ty = inner;
                    skip_forall = false;
                },
                | hir::ty::TyKind::Where(clause, inner) => {
                    for ctnt in clause.constraints.iter() {
                        let class = bcx.db.class_data(ctnt.class);

                        if !class.items.is_empty() {
                            let name = format!("$r{}", bcx.records.len());

                            if !skip_where {
                                write!(bcx, "{}, ", name)?;
                            }

                            bcx.records.push(expr::JsExpr::Ident { name });
                        }
                    }

                    ty = inner;
                    skip_where = false;
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
            type_vars: Vec::new(),
            records: Vec::new(),
            in_lambda: Vec::new(),
            owner,
            ctx,
        }
    }

    pub fn lower(&mut self, in_block: bool) -> io::Result<()> {
        let expr = self.lower_expr_inline(self.body.body_expr());
        let ret = match expr {
            | expr::JsExpr::Block { mut exprs } => {
                let last = exprs.pop().unwrap();

                exprs.push(expr::JsExpr::Return { expr: Box::new(last) });
                expr::JsExpr::Block { exprs }
            },
            | _ => expr::JsExpr::Return { expr: Box::new(expr) },
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
