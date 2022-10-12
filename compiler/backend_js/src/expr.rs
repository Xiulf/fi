use std::borrow::Cow;
use std::io::{self, Write};

use hir::id::HasModule;
use hir::{
    Const, Ctor, Expr, Func, HasResolver, Literal, MethodSource, Path, Resolver, Static, Stmt, TypeCtor, ValueNs,
};

use crate::indent::IndentWriter;
use crate::BodyCtx;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum JsExpr {
    Undefined,
    Ident {
        name: String,
    },
    Literal {
        lit: Literal,
    },
    Field {
        base: Box<JsExpr>,
        field: String,
    },
    Index {
        base: Box<JsExpr>,
        idx: Box<JsExpr>,
    },
    Call {
        base: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    Assign {
        place: Box<JsExpr>,
        expr: Box<JsExpr>,
    },
    BinOp {
        op: &'static str,
        lhs: Box<JsExpr>,
        rhs: Box<JsExpr>,
    },
    UnOp {
        op: &'static str,
        rhs: Box<JsExpr>,
    },
    New {
        class: Cow<'static, str>,
        args: Vec<JsExpr>,
    },
    Array {
        exprs: Vec<JsExpr>,
    },
    Object {
        fields: Vec<(String, JsExpr)>,
    },
    If {
        cond: Box<JsExpr>,
        then: Box<JsExpr>,
        else_: Option<Box<JsExpr>>,
    },
    Block {
        exprs: Vec<JsExpr>,
    },
    Var {
        name: String,
        expr: Option<Box<JsExpr>>,
    },
    Lambda {
        name: Option<String>,
        params: Vec<String>,
        body: Box<JsExpr>,
    },
    Return {
        expr: Box<JsExpr>,
    },
    Throw {
        expr: Box<JsExpr>,
    },
}

pub enum Arg {
    ExprId(hir::ExprId),
    JsExpr(JsExpr),
}

impl JsExpr {
    pub fn is_inline(&self) -> bool {
        match self {
            | Self::Undefined | Self::Ident { .. } | Self::Literal { .. } => true,
            | Self::Field { base, .. } | Self::Index { base, .. } => base.is_inline(),
            | Self::Array { exprs } => exprs.iter().all(Self::is_inline),
            | Self::Object { fields } => fields.iter().all(|f| f.1.is_inline()),
            | Self::If {
                cond,
                then,
                else_: Some(else_),
            } => cond.is_inline() && then.is_inline() && else_.is_inline(),
            | Self::BinOp { lhs, rhs, .. } => lhs.is_inline() && rhs.is_inline(),
            | Self::UnOp { rhs, .. } => rhs.is_inline(),
            | Self::New { args, .. } => args.iter().any(Self::is_inline),
            | Self::Call { base, args } => base.is_inline() || args.iter().any(Self::is_inline),
            | Self::Block { exprs } if exprs.len() == 1 => exprs[0].is_inline(),
            | Self::Block { exprs } => exprs.is_empty(),
            | _ => false,
        }
    }

    pub fn is_place(&self) -> bool {
        if !self.is_inline() {
            return true;
        }

        match self {
            | Self::Ident { .. } | Self::Field { .. } | Self::Index { .. } => true,
            | _ => false,
        }
    }

    pub fn is_effectful(&self) -> bool {
        match self {
            | Self::Assign { .. }
            | Self::Call { .. }
            | Self::Var { .. }
            | Self::Lambda { .. }
            | Self::Return { .. }
            | Self::Throw { .. } => true,
            | Self::Field { base, .. } | Self::Index { base, .. } => base.is_effectful(),
            | Self::BinOp { lhs, rhs, .. } => lhs.is_effectful() || rhs.is_effectful(),
            | Self::UnOp { rhs, .. } => rhs.is_effectful(),
            | Self::Array { exprs } => exprs.iter().any(Self::is_effectful),
            | Self::Object { fields } => fields.iter().all(|f| f.1.is_effectful()),
            | Self::If {
                cond,
                then,
                else_: Some(else_),
            } => cond.is_effectful() || then.is_effectful() || else_.is_effectful(),
            | Self::If {
                cond,
                then,
                else_: None,
            } => cond.is_effectful() || then.is_effectful(),
            | Self::Block { exprs } => exprs.iter().any(Self::is_effectful),
            | _ => false,
        }
    }

    pub fn is_terminator(&self) -> bool {
        match self {
            | Self::Return { .. } | Self::Throw { .. } => true,
            | _ => false,
        }
    }

    pub fn write<W: Write>(&self, out: &mut IndentWriter<W>, in_block: bool) -> io::Result<()> {
        if !self.is_inline() && !in_block {
            writeln!(out, "(function() {{")?;
            out.indent();
        }

        self.write_inner(out, in_block || !self.is_inline())?;

        if !self.is_inline() && !in_block {
            out.dedent();
            write!(out, "\n}})()")?;
        }

        Ok(())
    }

    fn write_inner<W: Write>(&self, out: &mut IndentWriter<W>, in_block: bool) -> io::Result<()> {
        match self {
            | JsExpr::Undefined => write!(out, "undefined"),
            | JsExpr::Literal { lit } => match lit {
                | Literal::Int(v) => write!(out, "{}", v),
                | Literal::Float(v) => write!(out, "{}", v),
                | Literal::Char(v) => write!(out, "{:?}", v),
                | Literal::String(v) => write!(out, "{:?}", v),
            },
            | JsExpr::Ident { name } => write!(out, "{}", name),
            | JsExpr::Field { base, field } => {
                base.write_inner(out, false)?;
                write!(out, ".{}", field)
            },
            | JsExpr::Index { base, idx } => {
                base.write_inner(out, false)?;
                write!(out, "[")?;
                idx.write_inner(out, false)?;
                write!(out, "]")
            },
            | JsExpr::Assign { place, expr } => {
                place.write_inner(out, false)?;
                write!(out, " = ")?;
                expr.write_inner(out, false)
            },
            | JsExpr::BinOp { lhs, rhs, op } => {
                lhs.write_inner(out, false)?;
                write!(out, " {} ", op)?;
                rhs.write_inner(out, false)
            },
            | JsExpr::UnOp { op, rhs } => {
                write!(out, "{}", op)?;
                rhs.write_inner(out, false)
            },
            | JsExpr::New { class, args } => {
                write!(out, "new {}(", class)?;

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(out, ", ")?;
                    }

                    arg.write_inner(out, false)?;
                }

                write!(out, ")")
            },
            | JsExpr::Array { exprs } => {
                write!(out, "[")?;

                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }

                    expr.write_inner(out, false)?;
                }

                write!(out, "]")
            },
            | JsExpr::Object { fields } => {
                write!(out, "{{ ")?;

                for (i, (name, expr)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }

                    write!(out, "{}: ", name)?;
                    expr.write_inner(out, false)?;
                }

                write!(out, " }}")
            },
            | JsExpr::Call { base, args } => {
                base.write_inner(out, false)?;
                write!(out, "(")?;

                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }

                    arg.write_inner(out, false)?;
                }

                write!(out, ")")
            },
            | JsExpr::Block { exprs } if exprs.is_empty() => Ok(()),
            | JsExpr::Block { exprs } if exprs.len() == 1 => exprs[0].write_inner(out, in_block),
            | JsExpr::Block { exprs } => {
                for (i, expr) in exprs.iter().enumerate() {
                    if expr.is_effectful() {
                        expr.write_inner(out, true)?;

                        if expr.is_terminator() {
                            break;
                        }

                        if i < exprs.len() - 1 {
                            writeln!(out, ";")?;
                        }
                    }
                }

                Ok(())
            },
            | JsExpr::Lambda {
                name: None,
                params,
                body,
            } => {
                write!(out, "(")?;

                for param in params {
                    write!(out, "{} => ", param)?;
                }

                if body.is_inline() {
                    body.write_inner(out, false)?;
                } else {
                    writeln!(out, "{{")?;
                    out.indent();
                    body.write_inner(out, true)?;
                    out.dedent();
                    write!(out, ";\n}}")?;
                }

                write!(out, ")")
            },
            | JsExpr::Lambda {
                name: Some(name),
                params,
                body,
            } => {
                write!(out, "function {}(", name)?;

                for (i, param) in params.iter().enumerate() {
                    write!(out, "{}", param)?;

                    if i < params.len() - 1 {
                        writeln!(out, ") {{")?;
                        out.indent();
                        write!(out, "return function(")?;
                    }
                }

                writeln!(out, ") {{")?;
                out.indent();
                body.write_inner(out, true)?;
                out.dedent();
                write!(out, ";\n}}")?;

                for _ in 0..params.len() - 1 {
                    out.dedent();
                    write!(out, ";\n}}")?;
                }

                Ok(())
            },
            | JsExpr::Var { name, expr: Some(expr) } => {
                write!(out, "var {} = ", name)?;
                expr.write_inner(out, false)
            },
            | JsExpr::Var { name, expr: None } => {
                write!(out, "var {}", name)
            },
            | JsExpr::If { cond, then, else_ } => match else_ {
                | Some(else_) if cond.is_inline() && then.is_inline() && else_.is_inline() => {
                    cond.write_inner(out, false)?;
                    write!(out, " ? ")?;
                    then.write_inner(out, false)?;
                    write!(out, " : ")?;
                    else_.write_inner(out, false)
                },
                | Some(else_) => {
                    write!(out, "if (")?;
                    cond.write_inner(out, false)?;
                    writeln!(out, ") {{")?;
                    out.indent();
                    then.write_inner(out, true)?;
                    out.dedent();

                    if matches!(**else_, JsExpr::If { .. }) && !else_.is_inline() {
                        write!(out, ";\n}} else ")?;
                        else_.write_inner(out, false)
                    } else {
                        writeln!(out, ";\n}} else {{")?;
                        out.indent();
                        else_.write_inner(out, true)?;
                        out.dedent();
                        write!(out, ";\n}}")
                    }
                },
                | None => {
                    write!(out, "if (")?;
                    cond.write_inner(out, false)?;
                    writeln!(out, ") {{")?;
                    out.indent();
                    then.write_inner(out, true)?;
                    out.dedent();
                    write!(out, ";\n}}")
                },
            },
            | JsExpr::Return { expr } => {
                write!(out, "return ")?;
                expr.write_inner(out, false)
            },
            | JsExpr::Throw { expr } => {
                write!(out, "throw ")?;
                expr.write_inner(out, false)
            },
        }
    }
}

impl BodyCtx<'_, '_> {
    pub fn lower_expr(&mut self, expr: hir::ExprId, block: &mut Vec<JsExpr>) -> JsExpr {
        let body = self.body.clone();

        match body[expr] {
            | Expr::Hole => unreachable!(),
            | Expr::Typed { expr, .. } => self.lower_expr(expr, block),
            | Expr::Lit { ref lit } => JsExpr::Literal { lit: lit.clone() },
            | Expr::Unit => {
                let lib = self.owner.module(self.db.upcast()).lib;
                let unit = self.db.lang_item(lib, "unit-type").unwrap();
                let unit = TypeCtor::from(unit.as_type_ctor().unwrap());
                let unit = self.mangle((unit.ctors(self.db)[0].path(self.db).to_string(), true));

                JsExpr::Ident { name: unit }
            },
            | Expr::Path { ref path } => self.lower_path(
                &Resolver::for_expr(self.db.upcast(), self.owner, expr),
                (expr, 0),
                path,
                true,
                block,
            ),
            | Expr::Do { ref stmts } => self.lower_block(stmts, block),
            | Expr::Try { ref stmts } => self.lower_try(expr, stmts, block),
            | Expr::App { mut base, arg } => {
                let mut args = vec![Arg::ExprId(arg)];

                while let Expr::App { base: base1, arg } = self.body[base] {
                    base = base1;
                    args.push(Arg::ExprId(arg));
                }

                args.reverse();
                self.lower_app(Arg::ExprId(base), args, block)
            },
            | Expr::Infix { ref exprs, ref ops } => self.lower_expr_infix(expr, exprs, ops, block),
            | Expr::Field { base, ref field } => JsExpr::Field {
                base: Box::new(self.lower_expr(base, block)),
                field: field.to_string(),
            },
            | Expr::Array { ref exprs } => {
                let exprs = exprs.iter().map(|&e| self.lower_expr(e, block)).collect();

                JsExpr::Array { exprs }
            },
            | Expr::Record { ref fields } => {
                let fields = fields
                    .iter()
                    .map(|field| (field.name.to_string(), self.lower_expr(field.val, block)))
                    .collect();

                JsExpr::Object { fields }
            },
            | Expr::If { cond, then, else_ } => {
                let cond = self.lower_expr(cond, block);
                let cond = Box::new(self.is_true(cond));
                let then = Box::new(self.lower_expr_inline(then));
                let else_ = else_.map(|e| Box::new(self.lower_expr_inline(e)));
                let ifelse = JsExpr::If { cond, then, else_ };

                self.get_result(expr, ifelse, block)
            },
            | Expr::Lambda { ref pats, body } => {
                let name = format!("lambda{}", u32::from(expr.into_raw()));
                let mut params = Vec::with_capacity(pats.len());
                let mut exprs = Vec::new();

                for (i, &pat) in pats.iter().enumerate() {
                    let name = format!("lambda{}_param{}", u32::from(expr.into_raw()), i);
                    let param = JsExpr::Ident { name: name.clone() };

                    self.lower_pat(pat, param, &mut exprs);
                    params.push(name);
                }

                self.in_lambda.push(name);

                let expr = Box::new(self.lower_expr(body, &mut exprs));
                let _ = exprs.push(JsExpr::Return { expr });
                let body = Box::new(JsExpr::Block { exprs });
                let name = self.in_lambda.pop().unwrap();

                block.push(JsExpr::Lambda {
                    name: Some(name.clone()),
                    params,
                    body,
                });

                JsExpr::Ident { name }
            },
            | Expr::Recur => JsExpr::Ident {
                name: self.in_lambda.last().unwrap().clone(),
            },
            | Expr::Case { pred, ref arms } => self.lower_case(expr, pred, arms, block),
            | ref e => {
                tracing::warn!(target: "lower_expr", "not yet implemented: {:?}", e);
                JsExpr::Undefined
            },
        }
    }

    pub fn lower_expr_inline(&mut self, expr: hir::ExprId) -> JsExpr {
        let mut exprs = Vec::new();
        let res = self.lower_expr(expr, &mut exprs);

        if exprs.is_empty() {
            res
        } else {
            exprs.push(res);

            JsExpr::Block { exprs }
        }
    }

    pub fn is_true(&self, expr: JsExpr) -> JsExpr {
        let lib = self.owner.module(self.db.upcast()).lib;
        let bool = self.db.lang_item(lib, "bool-type").unwrap();
        let bool = TypeCtor::from(bool.as_type_ctor().unwrap());
        let true_ = self.mangle((bool.ctors(self.db)[1].path(self.db).to_string(), true));

        JsExpr::BinOp {
            op: "instanceof",
            lhs: Box::new(expr),
            rhs: Box::new(JsExpr::Ident { name: true_ }),
        }
    }

    pub fn lower_expr_infix(
        &mut self,
        expr: hir::ExprId,
        exprs: &[hir::ExprId],
        ops: &[hir::Path],
        block: &mut Vec<JsExpr>,
    ) -> JsExpr {
        use std::iter::{once, Enumerate, Peekable};
        let exprs = exprs.iter().map(|&e| Arg::ExprId(e));
        let resolver = Resolver::for_expr(self.db.upcast(), self.owner, expr);
        let db = self.db;
        let fixities = ops.iter().map(|op| {
            let (resolved, _, _) = resolver.resolve_value(db.upcast(), op).unwrap();

            match resolved {
                | ValueNs::Fixity(id) => match db.fixity_data(id).kind {
                    | hir::FixityKind::Infix { assoc, prec } => (id, assoc, prec),
                    | _ => unreachable!(),
                },
                | _ => unreachable!(),
            }
        });

        return go(
            self,
            fixities.peekable(),
            exprs,
            ops.iter().enumerate(),
            expr,
            &resolver,
            block,
        );

        fn go<'a>(
            ctx: &mut BodyCtx,
            mut fixities: Peekable<impl Iterator<Item = (hir::id::FixityId, hir::Assoc, hir::Prec)>>,
            mut exprs: impl Iterator<Item = Arg>,
            mut ops: Enumerate<impl Iterator<Item = &'a hir::Path>>,
            id: hir::ExprId,
            resolver: &Resolver,
            block: &mut Vec<JsExpr>,
        ) -> JsExpr {
            if let Some((fix, assoc, prec)) = fixities.next() {
                let (i, op) = ops.next().unwrap();
                let left = if let Some((fix2, _, prec2)) = fixities.peek() {
                    if fix == *fix2 {
                        match assoc {
                            | hir::Assoc::Left => true,
                            | hir::Assoc::Right => false,
                            | hir::Assoc::None => true,
                        }
                    } else {
                        prec >= *prec2
                    }
                } else {
                    let lhs = exprs.next().unwrap();
                    let rhs = exprs.next().unwrap();

                    return ctx.lower_path_app(resolver, (id, i), op, vec![lhs, rhs], block);
                };

                if left {
                    let lhs = exprs.next().unwrap();
                    let rhs = exprs.next().unwrap();
                    let exp = ctx.lower_path_app(resolver, (id, i), op, vec![lhs, rhs], block);
                    let exprs = once(Arg::JsExpr(exp)).chain(exprs).collect::<Vec<_>>();

                    go(ctx, fixities, exprs.into_iter(), ops, id, resolver, block)
                } else {
                    let lhs = exprs.next().unwrap();
                    let rhs = go(ctx, fixities, exprs, ops, id, resolver, block);

                    ctx.lower_path_app(resolver, (id, i), op, vec![lhs, Arg::JsExpr(rhs)], block)
                }
            } else {
                ctx.lower_arg(exprs.next().unwrap(), block)
            }
        }
    }

    pub fn lower_case(
        &mut self,
        expr: hir::ExprId,
        pred: hir::ExprId,
        arms: &[hir::CaseArm],
        block: &mut Vec<JsExpr>,
    ) -> JsExpr {
        let pred_ = self.lower_expr(pred, block);
        let pred = self.placed(pred, pred_, block);
        let mut checks = Vec::with_capacity(arms.len());
        let mut else_ = JsExpr::Throw {
            expr: Box::new(JsExpr::Literal {
                lit: Literal::String("failed pattern match".into()),
            }),
        };

        for arm in arms {
            let check = self.lower_pat(arm.pat, pred.clone(), block);
            let (check, then) = match arm.value {
                | hir::CaseValue::Normal(expr) => (check, self.lower_expr_inline(expr)),
                | hir::CaseValue::Guarded(ref guards, ref exprs) if guards.len() == 1 && exprs.len() == 1 => {
                    let guard = self.lower_expr(guards[0], block);
                    let guard2 = self.is_true(guard.clone());
                    let then = self.lower_expr_inline(exprs[0]);
                    let check = check.map_or_else(
                        || guard,
                        |c| JsExpr::BinOp {
                            op: "&&",
                            lhs: Box::new(c),
                            rhs: Box::new(guard2),
                        },
                    );

                    (Some(check), then)
                },
                | hir::CaseValue::Guarded(ref guards, ref exprs) => {
                    let mut block = Vec::new();
                    let mut exprs = exprs.iter();
                    let guards = guards.iter().zip(exprs.by_ref()).collect::<Vec<_>>();
                    let then = guards
                        .into_iter()
                        .rfold(exprs.next().map(|&e| self.lower_expr_inline(e)), |else_, (g, e)| {
                            let g = self.lower_expr(*g, &mut block);
                            let g = self.is_true(g);

                            Some(JsExpr::If {
                                cond: Box::new(g),
                                then: Box::new(self.lower_expr_inline(*e)),
                                else_: else_.map(Box::new),
                            })
                        })
                        .unwrap();

                    (check, then)
                },
            };

            if let Some(check) = check {
                checks.push((check, then));
            } else {
                else_ = then;
                break;
            }
        }

        let case = checks.into_iter().rfold(else_, |e, (c, t)| JsExpr::If {
            cond: Box::new(c),
            then: Box::new(t),
            else_: Some(Box::new(e)),
        });

        self.get_result(expr, case, block)
    }

    pub fn lower_block(&mut self, stmts: &[Stmt], exprs: &mut Vec<JsExpr>) -> JsExpr {
        for (i, stmt) in stmts.iter().enumerate() {
            match *stmt {
                | Stmt::Expr { expr } if i == stmts.len() - 1 => {
                    return self.lower_expr(expr, exprs);
                },
                | Stmt::Expr { expr } => {
                    let res = self.lower_expr(expr, exprs);

                    exprs.push(res);
                },
                | Stmt::Let { pat, val } => {
                    let expr = self.lower_expr(val, exprs);

                    if self.pat_is_ignored(pat) {
                        exprs.push(expr);
                    } else {
                        let expr = self.placed(val, expr, exprs);

                        self.lower_pat(pat, expr, exprs);
                    }
                },
                | Stmt::Bind { .. } => unreachable!(),
            }
        }

        JsExpr::Undefined
    }

    pub fn lower_try(&mut self, expr_id: hir::ExprId, stmts: &[Stmt], exprs: &mut Vec<JsExpr>) -> JsExpr {
        let infer = self.infer.clone();
        let mut methods = infer.methods[&(expr_id, 0)].iter().copied();
        let b = match methods.next().unwrap() {
            | MethodSource::Member(id) => self.member_ref(id.into(), &mut methods),
            | MethodSource::Record(idx) => self.records[idx].clone(),
        };

        let bind = JsExpr::Field {
            base: Box::new(b),
            field: self.mangle(("bind", true)),
        };

        for (i, stmt) in stmts.iter().enumerate() {
            match *stmt {
                | Stmt::Expr { expr } if i == stmts.len() - 1 => {
                    return self.lower_expr(expr, exprs);
                },
                | Stmt::Expr { expr } => {
                    let res = self.lower_expr(expr, exprs);

                    exprs.push(res);
                },
                | Stmt::Let { pat, val } => {
                    let expr = self.lower_expr(val, exprs);

                    if self.pat_is_ignored(pat) {
                        exprs.push(expr);
                    } else {
                        let expr = self.placed(val, expr, exprs);

                        self.lower_pat(pat, expr, exprs);
                    }
                },
                | Stmt::Bind { pat, val } => {
                    let mut body = Vec::new();
                    let expr = self.lower_expr(val, exprs);
                    let param = format!(
                        "try_{}_bind_{}",
                        u32::from(expr_id.into_raw()),
                        u32::from(pat.into_raw())
                    );

                    self.lower_pat(pat, JsExpr::Ident { name: param.clone() }, &mut body);
                    let rest = self.lower_try(expr_id, &stmts[i + 1..], &mut body);
                    body.push(JsExpr::Return { expr: Box::new(rest) });
                    let body = JsExpr::Block { exprs: body };
                    let lam = JsExpr::Lambda {
                        name: None,
                        params: vec![param],
                        body: Box::new(body),
                    };

                    return JsExpr::Call {
                        base: Box::new(bind.clone()),
                        args: vec![expr, lam],
                    };
                },
            }
        }

        JsExpr::Undefined
    }

    pub fn member_ref(&mut self, member: hir::Member, sources: &mut dyn Iterator<Item = MethodSource>) -> JsExpr {
        let lower = self.db.lower_member(member.into());
        let record = JsExpr::Ident {
            name: self.mangle((member.link_name(self.db), true)),
        };

        let skip = lower
            .member
            .where_clause
            .constraints
            .iter()
            .map(|c| self.db.class_data(c.class).items.is_empty())
            .collect::<Vec<_>>();

        if skip.iter().any(|b| !*b) {
            let args = skip
                .into_iter()
                .filter_map(|s| {
                    if s {
                        sources.next().unwrap();
                        None
                    } else {
                        Some(match sources.next().unwrap() {
                            | MethodSource::Member(m) => self.member_ref(m.into(), sources),
                            | MethodSource::Record(r) => self.records[r].clone(),
                        })
                    }
                })
                .collect();

            return JsExpr::Call {
                base: Box::new(record),
                args,
            };
        }

        for s in skip {
            if s {
                sources.next().unwrap();
            }
        }

        record
    }

    pub fn lower_app(&mut self, base: Arg, mut args: Vec<Arg>, block: &mut Vec<JsExpr>) -> JsExpr {
        let body = self.body.clone();

        if let Arg::ExprId(id) = base {
            if let Some(types) = self.infer.instances.get(&id) {
                let mut i = 0;

                for ty in types {
                    if let hir::ty::TyKind::Symbol(s) = ty.lookup(self.db) {
                        args.insert(
                            i,
                            Arg::JsExpr(JsExpr::Literal {
                                lit: Literal::String(s.into_string()),
                            }),
                        );

                        i += 1;
                    }
                }
            }

            if let Expr::Path { ref path } = body[id] {
                let resolver = Resolver::for_expr(self.db.upcast(), self.owner, id);

                return self.lower_path_app(&resolver, (id, 0), path, args, block);
            }
        }

        let base = self.lower_arg(base, block);

        args.into_iter()
            .map(|a| match a {
                | Arg::ExprId(id) => self.lower_expr(id, block),
                | Arg::JsExpr(expr) => expr,
            })
            .fold(base, |base, a| JsExpr::Call {
                base: Box::new(base),
                args: vec![a],
            })
    }

    fn func_params(&self, id: hir::id::FuncId) -> usize {
        let infer = self.db.infer(id.into());
        let mut ty = infer.self_type.ty;

        loop {
            match ty.lookup(self.db) {
                | hir::ty::TyKind::ForAll(_, inner, _) => {
                    ty = inner;
                },
                | hir::ty::TyKind::Where(_, inner) => {
                    ty = inner;
                },
                | _ => break,
            }
        }

        if self.db.func_data(id).has_body {
            let body = self.db.body(id.into());

            body.params().len()
        } else {
            use hir::id::Lookup;
            let lib = id.lookup(self.db.upcast()).module(self.db.upcast()).lib;
            let func_ctor = self.db.lang_item(lib, "fn-type").unwrap().as_type_ctor().unwrap();
            let mut params = 0;

            while let Some([_, ret]) = ty.match_ctor(self.db, func_ctor).as_deref() {
                params += 1;
                ty = *ret;
            }

            params
        }
    }

    pub fn lower_path_app(
        &mut self,
        resolver: &Resolver,
        expr: (hir::ExprId, usize),
        path: &Path,
        mut args: Vec<Arg>,
        block: &mut Vec<JsExpr>,
    ) -> JsExpr {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();
        let (mut base, params) = match resolved {
            | ValueNs::Local(id) => (self.locals[id].clone(), 1),
            | ValueNs::Fixity(id) => {
                let resolver = id.resolver(self.db.upcast());
                let data = self.db.fixity_data(id);

                return self.lower_path_app(&resolver, expr, &data.func, args, block);
            },
            | ValueNs::Func(id) => {
                let func = Func::from(id);

                if func.is_intrinsic(self.db) {
                    return self.lower_intrinsic(&func.name(self.db).to_string(), args, block);
                }

                (
                    self.lower_path(resolver, expr, path, false, block),
                    self.func_params(id),
                )
            },
            | ValueNs::Ctor(id) => {
                let args = args.into_iter().map(|a| self.lower_arg(a, block)).collect();

                return JsExpr::New {
                    class: Cow::Owned(self.mangle((Ctor::from(id).path(self.db).to_string(), true))),
                    args,
                };
            },
            | _ => (self.lower_path(resolver, expr, path, false, block), args.len()),
        };

        if params > 0 {
            let args2 = args
                .drain(..params)
                .map(|a| match a {
                    | Arg::ExprId(id) => self.lower_expr(id, block),
                    | Arg::JsExpr(expr) => expr,
                })
                .collect();

            base = JsExpr::Call {
                base: Box::new(base),
                args: args2,
            };
        }

        if !args.is_empty() {
            self.lower_app(Arg::JsExpr(base), args, block)
        } else {
            base
        }
    }

    pub fn lower_path(
        &mut self,
        resolver: &Resolver,
        expr: (hir::ExprId, usize),
        path: &Path,
        is_arg: bool,
        block: &mut Vec<JsExpr>,
    ) -> JsExpr {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();

        match resolved {
            | ValueNs::Local(id) => self.locals[id].clone(),
            | ValueNs::Func(id) => {
                let func = Func::from(id);
                let infer = self.infer.clone();
                let is_method = func.as_assoc_item(self.db).is_some();
                let mut methods = infer.methods.get(&expr).map(|m| m.iter().copied());
                let mut base = if is_method {
                    let methods = methods.as_mut().unwrap();
                    let b = match methods.next().unwrap() {
                        | MethodSource::Member(id) => self.member_ref(id.into(), methods),
                        | MethodSource::Record(idx) => self.records[idx].clone(),
                    };

                    JsExpr::Field {
                        base: Box::new(b),
                        field: self.mangle((path.segments().last().unwrap(), true)),
                    }
                } else {
                    JsExpr::Ident {
                        name: self.mangle(Func::from(id).link_name(self.db)),
                    }
                };

                let mut args = Vec::new();

                if let Some(mut methods) = methods {
                    while let Some(m) = methods.next() {
                        match m {
                            | MethodSource::Member(id) => {
                                let lower = self.db.lower_member(id);

                                if !self.db.class_data(lower.member.class).items.is_empty() {
                                    args.push(self.member_ref(id.into(), &mut methods));
                                }
                            },
                            | MethodSource::Record(idx) => args.push(self.records[idx].clone()),
                        }
                    }
                }

                if !args.is_empty() {
                    base = JsExpr::Call {
                        base: Box::new(base),
                        args,
                    };
                }

                let params = self.func_params(id);

                if params == 0 {
                    base = JsExpr::Call {
                        base: Box::new(base),
                        args: Vec::new(),
                    };
                } else if is_arg {
                    let params = (0..params).map(|i| format!("_{}", i)).collect::<Vec<_>>();

                    base = JsExpr::Lambda {
                        name: None,
                        params: params.clone(),
                        body: Box::new(JsExpr::Call {
                            base: Box::new(base),
                            args: params.into_iter().map(|p| JsExpr::Ident { name: p }).collect(),
                        }),
                    };
                }

                base
            },
            | ValueNs::Const(id) => JsExpr::Ident {
                name: self.mangle((Const::from(id).path(self.db).to_string(), true)),
            },
            | ValueNs::Static(id) => JsExpr::Ident {
                name: self.mangle(Static::from(id).link_name(self.db)),
            },
            | ValueNs::Ctor(id) => JsExpr::New {
                class: Cow::Owned(self.mangle((Ctor::from(id).path(self.db).to_string(), true))),
                args: Vec::new(),
            },
            | ValueNs::Fixity(id) => {
                let resolver = id.resolver(self.db.upcast());
                let data = self.db.fixity_data(id);

                self.lower_path(&resolver, expr, &data.func, is_arg, block)
            },
        }
    }

    pub fn lower_arg(&mut self, arg: Arg, block: &mut Vec<JsExpr>) -> JsExpr {
        match arg {
            | Arg::ExprId(id) => self.lower_expr(id, block),
            | Arg::JsExpr(expr) => expr,
        }
    }

    pub fn placed(&mut self, id: hir::ExprId, expr: JsExpr, block: &mut Vec<JsExpr>) -> JsExpr {
        if expr.is_place() {
            return expr;
        }

        let name = format!("${}", u32::from(id.into_raw()));

        block.push(JsExpr::Var {
            name: name.clone(),
            expr: Some(Box::new(expr)),
        });

        JsExpr::Ident { name }
    }
}
