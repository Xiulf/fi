use crate::pattern::*;
use crate::LowerDatabase;
use hir::ir as hir;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter, Write};
use std::sync::Arc;

#[derive(Debug, PartialEq, Eq)]
pub struct JsModule {
    exports: HashMap<String, JsDef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsDef {
    Func { name: String, params: Vec<String>, body: JsBlock },
    Var { name: String, val: Box<JsExpr> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsBlock {
    stmts: Vec<JsExpr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsPlace {
    var: String,
    elems: Vec<JsPlaceElem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsPlaceElem {
    Field(String),
    Index(JsExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsExpr {
    Const { c: JsConst },
    Def { def: JsDef },
    Place { place: JsPlace },
    Assign { place: JsPlace, val: Box<JsExpr> },
    Op { name: String, left: Box<JsExpr>, right: Box<JsExpr> },
    Call { base: Box<JsExpr>, args: Vec<JsExpr> },
    InstanceOf { place: JsPlace, class: JsPlace },
    IfElse { cond: Box<JsExpr>, then: JsBlock, else_: JsBlock },
    Throw { ex: Box<JsExpr> },
    Return { expr: Box<JsExpr> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsConst {
    Int { val: u128 },
    Float { bits: u64 },
    Bool { val: bool },
    String { val: String },
    Undefined,
    Null,
}

pub fn lower(db: &dyn LowerDatabase, lib: hir::LibId, module: hir::ModuleId) -> Arc<JsModule> {
    let file = db.module_tree(lib).file(module);
    let hir = db.module_hir(file);
    let mut exports = HashMap::new();

    for item in hir.items.values() {
        let mut ctx = LowerCtx {
            db,
            hir: &hir,
            locals: HashMap::new(),
        };

        if let Some(def) = ctx.lower_item(item) {
            exports.insert(item.name.to_string(), def);
        }
    }

    Arc::new(JsModule { exports })
}

struct LowerCtx<'db> {
    db: &'db dyn LowerDatabase,
    hir: &'db hir::Module,
    locals: HashMap<hir::HirId, String>,
}

impl<'db> LowerCtx<'db> {
    pub fn lower_item(&mut self, item: &hir::Item) -> Option<JsDef> {
        let name = item.name.to_string();

        match &item.kind {
            | hir::ItemKind::Func { body, .. } => {
                let body = &self.hir.bodies[body];
                let params = body
                    .params
                    .iter()
                    .map(|p| {
                        let var = format!("p{}", p.id.local_id.0);

                        self.locals.insert(p.id, var.clone());
                        var
                    })
                    .collect();
                let mut block = JsBlock { stmts: Vec::new() };

                self.lower_body(body, &mut block);

                Some(JsDef::Func { name, params, body: block })
            },
            | hir::ItemKind::Static { body: _, .. } => {
                unimplemented!();
            },
            | _ => None,
        }
    }

    pub fn lower_body(&mut self, body: &hir::Body, block: &mut JsBlock) {
        let res = self.lower_expr(&body.value, block);

        block.return_(res);
    }

    pub fn lower_expr(&mut self, expr: &hir::Expr, block: &mut JsBlock) -> JsExpr {
        match &expr.kind {
            | hir::ExprKind::Error => unreachable!(),
            | hir::ExprKind::Hole { .. } => unreachable!(),
            | hir::ExprKind::Int { val } => JsExpr::Const { c: JsConst::Int { val: *val } },
            | hir::ExprKind::Float { bits } => JsExpr::Const {
                c: JsConst::Float { bits: *bits },
            },
            | hir::ExprKind::Char { val } => JsExpr::Const {
                c: JsConst::String { val: val.to_string() },
            },
            | hir::ExprKind::Str { val } => JsExpr::Const {
                c: JsConst::String { val: val.clone() },
            },
            | hir::ExprKind::Ident { name, res } => match res {
                | hir::Res::Error => unreachable!(),
                | hir::Res::Local(id) => JsExpr::Place {
                    place: JsPlace::new(self.locals[id].clone()),
                },
                | hir::Res::Def(_, id) => JsExpr::Place {
                    place: if id.module == self.hir.id {
                        JsPlace::new(name.to_string())
                    } else if id.lib == self.db.lib() {
                        let tree = self.db.module_tree(id.lib);
                        let module = tree.data(id.module);
                        let module = (&**module.name.symbol).replace(".", "_");

                        JsPlace::new(module).field(name.to_string())
                    } else {
                        let module = self.db.external_modules(self.db.lib()).iter().find(|m| m.id == id.module).unwrap().name;
                        let module = (&**module.symbol).replace(".", "_");

                        JsPlace::new(module).field(name.to_string())
                    },
                },
            },
            | hir::ExprKind::App { base, arg } => {
                let mut base = base;
                let mut args = vec![&**arg];

                while let hir::ExprKind::App { base: b, arg } = &base.kind {
                    base = b;
                    args.push(&**arg);
                }

                args.reverse();
                self.lower_app(base, args, block)
            },
            | hir::ExprKind::Case { pred, arms } => {
                let preds = pred
                    .iter()
                    .map(|p| match self.lower_expr(p, block) {
                        | JsExpr::Place { place } => place,
                        | expr => {
                            let var = format!("v{}", p.id.local_id.0);
                            let place = JsPlace::new(var.clone());

                            block.assign(place.clone(), expr);
                            place
                        },
                    })
                    .collect();

                let case = self.convert_arms(preds, arms);

                self.lower_case(expr.id, case, block)
            },
            | _ => JsExpr::Const { c: JsConst::Undefined },
        }
    }

    pub fn lower_app(&mut self, base: &hir::Expr, args: Vec<&hir::Expr>, block: &mut JsBlock) -> JsExpr {
        let base = self.lower_expr(base, block);
        let args = args.into_iter().map(|a| self.lower_expr(a, block)).collect();

        JsExpr::Call { base: Box::new(base), args }
    }

    pub fn lower_case(&mut self, id: hir::HirId, case: Case<String, JsPlace, JsExpr>, block: &mut JsBlock) -> JsExpr {
        if !case.arms.is_empty() && case.arms[0].matches_all() {
            self.lower_guarded(case.arms[0].guard, None, block)
        } else {
            let res = format!("v{}", id.local_id.0);
            let res = JsPlace::new(res);
            let last = case.arms.len() - 1;

            fn arm_rec<'hir>(
                this: &mut LowerCtx,
                it: &mut impl Iterator<Item = (usize, Arm<'hir, String, JsPlace, JsExpr>)>,
                last: usize,
                res: JsPlace,
                block: &mut JsBlock,
            ) {
                if let Some((i, arm)) = it.next() {
                    if arm.matches_all() {
                        let _ = this.lower_pattern(arm.pat, block);

                        this.lower_guarded(arm.guard, Some(res.clone()), block);
                    } else if i == last {
                        let cond = this.lower_pattern(arm.pat, block);
                        let mut then = JsBlock { stmts: Vec::new() };
                        let mut else_ = JsBlock { stmts: Vec::new() };

                        this.lower_guarded(arm.guard, Some(res.clone()), &mut then);
                        else_.unreachable();
                        block.if_else(cond.unwrap(), then, else_);
                    } else {
                        let cond = this.lower_pattern(arm.pat, block);
                        let mut then = JsBlock { stmts: Vec::new() };
                        let mut else_ = JsBlock { stmts: Vec::new() };

                        this.lower_guarded(arm.guard, Some(res.clone()), &mut then);
                        arm_rec(this, it, last, res, &mut else_);
                        block.if_else(cond.unwrap(), then, else_);
                    }
                }
            }

            arm_rec(self, &mut case.arms.into_iter().enumerate(), last, res.clone(), block);

            JsExpr::Place { place: res }
        }
    }

    pub fn lower_pattern(&mut self, pat: Pattern<String, JsPlace, JsExpr>, block: &mut JsBlock) -> Option<JsExpr> {
        match pat {
            | Pattern::Bind(local, place) => {
                block.assign(JsPlace::new(local), JsExpr::Place { place });
                None
            },
            | Pattern::Switch(cond, _) => Some(cond),
            | Pattern::Seq(mut pats) => {
                if !pats.is_empty() && pats[0].checked() {
                    let cond = self.lower_pattern(pats.remove(0), block);

                    for pat in pats {
                        self.lower_pattern(pat, block).unwrap();
                    }

                    cond
                } else {
                    for pat in pats {
                        self.lower_pattern(pat, block).unwrap();
                    }

                    None
                }
            },
            | Pattern::And(pats) => pats.into_iter().filter_map(|p| self.lower_pattern(p, block)).fold_first(|a, b| JsExpr::Op {
                name: "&&".into(),
                left: Box::new(a),
                right: Box::new(b),
            }),
            | Pattern::Or(pats) => pats.into_iter().filter_map(|p| self.lower_pattern(p, block)).fold_first(|a, b| JsExpr::Op {
                name: "||".into(),
                left: Box::new(a),
                right: Box::new(b),
            }),
        }
    }

    pub fn lower_guarded(&mut self, guard: &hir::Guarded, place: Option<JsPlace>, block: &mut JsBlock) -> JsExpr {
        match guard {
            | hir::Guarded::Unconditional(expr) => {
                if let Some(place) = place {
                    let expr = self.lower_expr(expr, block);

                    block.assign(place, expr.clone());
                    expr
                } else {
                    self.lower_expr(expr, block)
                }
            },
            | hir::Guarded::Guarded(_) => unimplemented!(),
        }
    }

    fn convert_arms<'hir>(&mut self, preds: Vec<JsPlace>, arms: &'hir [hir::CaseArm]) -> Case<'hir, String, JsPlace, JsExpr> {
        Case {
            arms: arms
                .iter()
                .map(|arm| Arm {
                    pat: self.convert_pats(&preds, &arm.pats),
                    guard: &arm.val,
                })
                .collect(),
        }
    }

    fn convert_pats(&mut self, preds: &[JsPlace], pats: &[hir::Pat]) -> Pattern<String, JsPlace, JsExpr> {
        let mut pats = pats
            .iter()
            .zip(preds.to_owned())
            .filter_map(|(pat, pred)| self.convert_pat(pat, pred))
            .collect::<Vec<_>>();

        let _ = pats.sort_by_key(Pattern::checked);
        let i = pats.partition_point(Pattern::checked);
        let mut rest = pats.split_off(i);

        if rest.is_empty() && pats.len() == 1 {
            pats.pop().unwrap()
        } else if pats.is_empty() {
            Pattern::Seq(rest)
        } else if rest.is_empty() {
            Pattern::And(pats)
        } else {
            rest.insert(0, Pattern::And(pats));
            Pattern::Seq(rest)
        }
    }

    fn convert_pat(&mut self, pat: &hir::Pat, pred: JsPlace) -> Option<Pattern<String, JsPlace, JsExpr>> {
        match &pat.kind {
            | hir::PatKind::Wildcard => None,
            | hir::PatKind::Bind { sub: None, .. } => {
                if pred.elems.is_empty() {
                    self.locals.insert(pat.id, pred.var);
                    None
                } else {
                    let local = format!("v{}", pat.id.local_id.0);
                    let _ = self.locals.insert(pat.id, local.clone());

                    Some(Pattern::Bind(local, pred))
                }
            },
            | hir::PatKind::Int { val } => Some(Pattern::Switch(JsExpr::Place { place: pred }, *val)),
            | hir::PatKind::Ctor { ctor, pats } => {
                // @TODO
                let file = self.db.module_tree(ctor.lib).file(ctor.module);
                let hir = self.db.module_hir(file);
                let ctor = &hir.items[&(*ctor).into()];
                let class = if hir.id == self.hir.id {
                    JsPlace::new(hir.name.to_string()).field(ctor.name.to_string())
                } else {
                    JsPlace::new(ctor.name.to_string())
                };

                let discr = JsExpr::InstanceOf { place: pred.clone(), class };
                let preds = (0..pats.len()).map(|i| pred.clone().field(format!("f{}", i))).collect::<Vec<_>>();
                let sub = self.convert_pats(&preds, pats);
                let pat = Pattern::Switch(discr, 0);

                match sub {
                    | Pattern::Or(mut pats) => {
                        pats.insert(0, pat);

                        Some(Pattern::Or(pats))
                    },
                    | Pattern::And(mut pats) => {
                        pats.insert(0, pat);

                        Some(Pattern::And(pats))
                    },
                    | Pattern::Seq(mut pats) if !pats.is_empty() => {
                        if let Pattern::And(pats2) = &mut pats[0] {
                            pats2.insert(0, pat);
                        } else {
                            pats.insert(0, pat);
                        }

                        Some(Pattern::Seq(pats))
                    },
                    | _ => Some(pat),
                }
            },
            | _ => unimplemented!(),
        }
    }
}

impl JsBlock {
    pub fn const_(&mut self, c: JsConst) {
        self.stmts.push(JsExpr::Const { c });
    }

    pub fn def(&mut self, def: JsDef) {
        self.stmts.push(JsExpr::Def { def });
    }

    pub fn place(&mut self, place: JsPlace) {
        self.stmts.push(JsExpr::Place { place });
    }

    pub fn assign(&mut self, place: JsPlace, val: JsExpr) {
        self.stmts.push(JsExpr::Assign { place, val: Box::new(val) });
    }

    pub fn op(&mut self, name: String, left: JsExpr, right: JsExpr) {
        self.stmts.push(JsExpr::Op {
            name,
            left: Box::new(left),
            right: Box::new(right),
        });
    }

    pub fn call(&mut self, base: JsExpr, args: Vec<JsExpr>) {
        self.stmts.push(JsExpr::Call { base: Box::new(base), args });
    }

    pub fn instance_of(&mut self, place: JsPlace, class: JsPlace) {
        self.stmts.push(JsExpr::InstanceOf { place, class });
    }

    pub fn if_else(&mut self, cond: JsExpr, then: JsBlock, else_: JsBlock) {
        self.stmts.push(JsExpr::IfElse {
            cond: Box::new(cond),
            then,
            else_,
        });
    }

    pub fn unreachable(&mut self) {
        self.stmts.push(JsExpr::Throw {
            ex: Box::new(JsExpr::Const {
                c: JsConst::String {
                    val: "unreachable code reached".into(),
                },
            }),
        });
    }

    pub fn return_(&mut self, expr: JsExpr) {
        self.stmts.push(JsExpr::Return { expr: Box::new(expr) });
    }
}

impl JsPlace {
    pub fn new(var: String) -> Self {
        JsPlace { var, elems: Vec::new() }
    }

    pub fn field(mut self, name: String) -> Self {
        self.elems.push(JsPlaceElem::Field(name));
        self
    }

    pub fn index(mut self, idx: JsExpr) -> Self {
        self.elems.push(JsPlaceElem::Index(idx));
        self
    }
}

impl Display for JsModule {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for def in self.exports.values() {
            writeln!(f, "{}", def)?;
        }

        for name in self.exports.keys() {
            writeln!(f, "module.exports.{} = {};", name, name)?;
        }

        Ok(())
    }
}

impl Display for JsDef {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            | JsDef::Func { name, params, body } => {
                let params = params.join(", ");

                write!(f, "function {}({}) {}", name, params, body)
            },
            | JsDef::Var { name, val } => {
                write!(f, "var {} = {};", name, val)
            },
        }
    }
}

impl Display for JsBlock {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{{")?;

        if !self.stmts.is_empty() {
            writeln!(f)?;
        }

        for stmt in &self.stmts {
            writeln!(indent(f), "{};", stmt)?;
        }

        write!(f, "}}")
    }
}

impl Display for JsPlace {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.var.fmt(f)?;

        for elem in &self.elems {
            match elem {
                | JsPlaceElem::Field(name) => write!(f, ".{}", name)?,
                | JsPlaceElem::Index(idx) => write!(f, "[{}]", idx)?,
            }
        }

        Ok(())
    }
}

impl Display for JsExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            | JsExpr::Const { c } => c.fmt(f),
            | JsExpr::Def { def } => def.fmt(f),
            | JsExpr::Place { place } => place.fmt(f),
            | JsExpr::Assign { place, val } => write!(f, "{} = {}", place, val),
            | JsExpr::Op { name, left, right } => write!(f, "{} {} {}", left, name, right),
            | JsExpr::Call { base, args } => write!(f, "{}({})", base, args.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")),
            | JsExpr::InstanceOf { place, class } => write!(f, "{} instanceof {}", place, class),
            | JsExpr::IfElse { cond, then, else_ } => write!(f, "if ({}) {} else {}", cond, then, else_),
            | JsExpr::Throw { ex } => write!(f, "throw {}", ex),
            | JsExpr::Return { expr } => write!(f, "return {}", expr),
        }
    }
}

impl Display for JsConst {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            | JsConst::Int { val } => val.fmt(f),
            | JsConst::Float { bits } => f64::from_bits(*bits).fmt(f),
            | JsConst::Bool { val } if *val => write!(f, "true"),
            | JsConst::Bool { .. } => write!(f, "false"),
            | JsConst::String { val } => write!(f, "{:?}", val),
            | JsConst::Undefined => write!(f, "undefined"),
            | JsConst::Null => write!(f, "null"),
        }
    }
}

fn indent<'a, W: Write>(f: &'a mut W) -> Indent<'a, W> {
    Indent(f, true, "    ")
}

struct Indent<'a, W: Write>(&'a mut W, bool, &'a str);

impl<'a, W: Write> Write for Indent<'a, W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
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
