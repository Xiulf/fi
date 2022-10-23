mod expr;
mod indent;
mod intrinsic;
mod optimize;
mod pat;

use std::io::{self, BufWriter, Write};
use std::sync::Arc;

use arena::ArenaMap;
use hir::db::HirDatabase;
use hir::id::{DefWithBodyId, HasModule};
use hir::ty::{Ty, TyKind};
use rustc_hash::FxHashMap;
use tracing_subscriber::EnvFilter;

#[no_mangle]
pub fn init_logging(filter: EnvFilter) {
    tracing_subscriber::fmt().without_time().with_env_filter(filter).init();

    std::panic::set_hook(Box::new(|info| {
        let loc = info.location().unwrap();

        // if let Some(ice) = info.payload().downcast_ref::<ICE>() {
        //     eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", ice.0, loc);
        //     return;
        // }

        // if let Some(err) = info.payload().downcast_ref::<Error>() {
        //     eprintln!("\x1B[31mError:\x1B[0m '{}'", err.0);
        //     return;
        // }

        let msg = match info.payload().downcast_ref::<&'static str>() {
            | Some(s) => *s,
            | None => match info.payload().downcast_ref::<String>() {
                | Some(s) => &s[..],
                | None => "...",
            },
        };

        eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", msg, loc);
    }));
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
    owner: DefWithBodyId,
    body: Arc<hir::Body>,
    infer: Arc<hir::InferenceResult<hir::ty::Ty, hir::ty::Constraint>>,
    locals: ArenaMap<hir::PatId, expr::JsExpr>,
    type_vars: Vec<expr::JsExpr>,
    records: Vec<expr::JsExpr>,
    in_lambda: Vec<String>,
    member_refs: FxHashMap<(hir::Member, Vec<expr::JsExpr>), expr::JsExpr>,
}

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn HirDatabase, out: &'a mut dyn Write) -> Self {
        Self {
            db,
            out: indent::IndentWriter::new(BufWriter::new(out)),
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
        let modules = module.children(self.db);
        let decls = module.declarations(self.db);
        let members = module.members(self.db);

        for module in modules {
            self.codegen(module)?;
        }

        for &def in &decls {
            self.register_def(def)?;
        }

        for &member in &members {
            self.register_member(member)?;
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
            | hir::ModuleDef::Ctor(c) => {
                writeln!(self, "var {};", self.mangle((c.path(self.db).to_string(), true)))
            },
            | _ => Ok(()),
        }
    }

    pub fn register_member(&mut self, def: hir::Member) -> io::Result<()> {
        writeln!(self, "var {};", self.mangle((def.link_name(self.db), true)))
    }

    pub fn codegen_def(&mut self, def: hir::ModuleDef) -> io::Result<()> {
        match def {
            | hir::ModuleDef::Func(func) => self.codegen_func(func),
            | hir::ModuleDef::Const(const_) => self.codegen_const(const_),
            | hir::ModuleDef::Static(static_) => self.codegen_static(static_),
            | hir::ModuleDef::Ctor(ctor) => self.codegen_ctor(ctor),
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
            self.codegen_main_shim(func)?;
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

    pub fn codegen_ctor(&mut self, ctor: hir::Ctor) -> io::Result<()> {
        let types = ctor.types(self.db);
        let name = ctor.name(self.db);

        writeln!(
            self,
            "{} = (function() {{",
            self.mangle((ctor.path(self.db).to_string(), true)),
        )?;

        self.out.indent();
        writeln!(self, "class {} {{", name)?;
        self.out.indent();
        write!(self, "constructor(")?;

        for i in 0..types.len() {
            if i != 0 {
                write!(self, ", ")?;
            }

            write!(self, "field{}", i)?;
        }

        writeln!(self, ") {{")?;
        self.out.indent();

        for i in 0..types.len() {
            writeln!(self, "this.field{0} = field{0};", i)?;
        }

        self.out.dedent();
        writeln!(self, "}}")?;
        writeln!(self, "toString() {{")?;
        self.out.indent();
        write!(self, "return `{}", name)?;

        if !types.is_empty() {
            write!(self, "(")?;

            for i in 0..types.len() {
                if i != 0 {
                    write!(self, ", ")?;
                }

                write!(self, "{{this.field{}}}", i)?;
            }

            write!(self, ")")?;
        }

        self.out.dedent();
        writeln!(self, "`;\n}}")?;
        self.out.dedent();
        writeln!(self, "}}")?;
        writeln!(self, "return {};", name)?;
        self.out.dedent();
        writeln!(self, "}})();")
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

            write!(self, "{} = ", self.mangle((member.link_name(self.db), true)))?;

            if !has_constraints {
                writeln!(self, "{{")?;
            } else {
                write!(self, "function(")?;
                let mut i = 0;

                for c in lower.member.where_clause.constraints.iter() {
                    if !self.db.class_data(c.class).items.is_empty() {
                        if i > 0 {
                            write!(self, ", ")?;
                        }

                        write!(self, "record{}", i)?;
                        i += 1;
                    }
                }

                writeln!(self, ") {{")?;
                self.out.indent();
                writeln!(self, "return {{")?;
            }

            self.out.indent();

            let verify = self.db.verify_member(member.into());
            let mut ctnts = verify.constraints.iter().copied();
            let mut i = 0;

            while let Some(c) = ctnts.next() {
                write!(self, "constraint{}: () => ", i)?;

                match c {
                    | hir::MethodSource::Member(id) => {
                        let member = hir::Member::from(id);
                        write!(self, "{}", self.mangle((member.link_name(self.db), true)))?;
                    },
                    | hir::MethodSource::Record(idx, p) => {
                        write!(self, "record{}", idx)?;

                        for i in p.iter() {
                            write!(self, ".constraint{}", i)?;
                        }
                    },
                }

                writeln!(self, ",")?;
                i += 1;
            }

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
        let mut bcx = BodyCtx::new(self, owner);
        let mut ty = if is_assoc {
            bcx.member_item()
        } else {
            bcx.infer.self_type.ty
        };

        let type_vars = bcx.type_vars.len();
        let records = bcx.records.len();
        let body = bcx.body.clone();

        ty = for_each_type_var(bcx.db, owner, ty, || {
            let name = format!("type{}", bcx.type_vars.len());

            write!(bcx, "{}, ", name)?;
            bcx.type_vars.push(expr::JsExpr::Ident { name });
            Ok(())
        })?;

        for_each_record(bcx.db, ty, || {
            let name = format!("record{}", bcx.records.len());

            write!(bcx, "{}, ", name)?;
            bcx.records.push(expr::JsExpr::Ident { name });
            Ok(())
        })?;

        if bcx.type_vars.len() != type_vars || bcx.records.len() != records {
            writeln!(bcx, ") {{")?;
            bcx.out.indent();
            write!(bcx, "return function (")?;
        }

        for (i, &pat) in body.params().iter().enumerate() {
            let name = format!("param{}", u32::from(pat.into_raw()));
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
        write!(bcx, ";\n}}")?;

        if bcx.type_vars.len() != type_vars || bcx.records.len() != records {
            bcx.out.dedent();
            write!(self, ";\n}}")?;
        }

        Ok(())
    }

    pub fn codegen_body_expr(&mut self, owner: DefWithBodyId) -> io::Result<()> {
        let mut bcx = BodyCtx::new(self, owner);
        bcx.lower(false)
    }

    pub fn codegen_main_shim(&mut self, main_func: hir::Func) -> io::Result<()> {
        let mut bcx = BodyCtx::new(self, hir::id::FuncId::from(main_func).into());
        let mut block = Vec::new();
        let infer = bcx.infer.clone();
        let mut methods = infer.methods[&(bcx.body.body_expr(), 0)].iter().copied();
        let b = match methods.next().unwrap() {
            | hir::MethodSource::Member(id) => bcx.member_ref(id.into(), &mut methods, &mut block),
            | hir::MethodSource::Record(idx, _) => bcx.records[idx].clone(),
        };

        let report = expr::JsExpr::Field {
            base: Box::new(b),
            field: bcx.mangle(("report", true)),
        };

        writeln!(bcx, "const $main = () => {{")?;
        bcx.out.indent();

        for var in block {
            var.write(&mut bcx.ctx.out, true)?;
            writeln!(bcx, ";")?;
        }

        write!(bcx, "return ")?;
        report.write(&mut bcx.ctx.out, true)?;
        writeln!(self, "({}());", self.mangle(main_func.link_name(self.db)))?;
        self.out.dedent();
        writeln!(self, "}};")
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
            member_refs: FxHashMap::default(),
            owner,
            ctx,
        }
    }

    pub fn lower(&mut self, in_block: bool) -> io::Result<()> {
        let expr = self.lower_expr_inline(self.body.body_expr());
        let ret = self.wrap_return(expr);

        ret.write(&mut self.out, in_block)
    }

    pub fn member_item(&mut self) -> Ty {
        let mut ty = self.infer.self_type.ty;

        ty = for_each_type_var(self.db, self.owner, ty, || {
            let name = format!("type{}", self.type_vars.len());

            self.type_vars.push(expr::JsExpr::Ident { name });
            Ok(())
        })
        .unwrap();

        ty = for_each_record(self.db, ty, || {
            let name = format!("record{}", self.records.len());

            self.records.push(expr::JsExpr::Ident { name });
            Ok(())
        })
        .unwrap();

        ty
    }
}

fn for_each_type_var(
    db: &dyn HirDatabase,
    owner: DefWithBodyId,
    ty: Ty,
    mut f: impl FnMut() -> io::Result<()>,
) -> io::Result<Ty> {
    if let TyKind::ForAll(kinds, inner, _) = ty.lookup(db) {
        let lib = owner.module(db.upcast()).lib;
        let symbol_ctor = db.lang_item(lib, "symbol-kind").unwrap();
        let symbol_ctor = symbol_ctor.as_type_ctor().unwrap();

        for kind in kinds.iter() {
            if kind.lookup(db) == TyKind::Ctor(symbol_ctor) {
                f()?;
            }
        }

        return Ok(inner);
    }

    Ok(ty)
}

fn for_each_record(db: &dyn HirDatabase, ty: Ty, mut f: impl FnMut() -> io::Result<()>) -> io::Result<Ty> {
    if let TyKind::Where(clause, inner) = ty.lookup(db) {
        for ctnt in clause.constraints.iter() {
            let class = db.class_data(ctnt.class);

            if !class.items.is_empty() {
                f()?;
            }
        }

        return Ok(inner);
    }

    Ok(ty)
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
