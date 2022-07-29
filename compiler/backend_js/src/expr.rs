use std::io::{self, Write};

use hir::id::HasModule;
use hir::{Const, Expr, Func, HasResolver, Literal, MethodSource, Module, Path, Resolver, Static, Stmt, ValueNs};

use crate::indent::IndentWriter;
use crate::BodyCtx;

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
    Array {
        exprs: Vec<JsExpr>,
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
        name: String,
        params: Vec<String>,
        body: Box<JsExpr>,
    },
    Return {
        expr: Box<JsExpr>,
    },
    Throw {
        expr: Box<JsExpr>,
    },
    Labeled {
        label: String,
        expr: Box<JsExpr>,
    },
    Goto {
        label: String,
        end: bool,
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
            | Self::If {
                cond,
                then,
                else_: Some(else_),
            } => cond.is_inline() && then.is_inline() && else_.is_inline(),
            | Self::BinOp { lhs, rhs, .. } => lhs.is_inline() && rhs.is_inline(),
            | Self::UnOp { rhs, .. } => rhs.is_inline(),
            | Self::Call { base, args } => base.is_inline() || args.iter().any(Self::is_inline),
            | Self::Block { exprs } => exprs.len() <= 1,
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
            | Self::Throw { .. }
            | Self::Goto { .. } => true,
            | Self::Field { base, .. } | Self::Index { base, .. } => base.is_effectful(),
            | Self::BinOp { lhs, rhs, .. } => lhs.is_effectful() || rhs.is_effectful(),
            | Self::UnOp { rhs, .. } => rhs.is_effectful(),
            | Self::Array { exprs } => exprs.iter().any(Self::is_effectful),
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
            | Self::Labeled { expr, .. } => expr.is_effectful(),
            | _ => false,
        }
    }

    pub fn is_terminator(&self) -> bool {
        match self {
            | Self::Return { .. } | Self::Throw { .. } | Self::Goto { .. } => true,
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
            | JsExpr::Lambda { name, params, body } => {
                write!(out, "function {}(", name)?;

                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }

                    write!(out, "{}", param)?;
                }

                writeln!(out, ") {{")?;
                out.indent();
                body.write_inner(out, true)?;
                out.dedent();
                write!(out, ";\n}}")
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
                    writeln!(out, "\n}} else {{")?;
                    out.indent();
                    else_.write_inner(out, true)?;
                    out.dedent();
                    write!(out, "\n}}")
                },
                | None => {
                    write!(out, "if (")?;
                    cond.write_inner(out, false)?;
                    writeln!(out, ") {{")?;
                    out.indent();
                    then.write_inner(out, true)?;
                    out.dedent();
                    write!(out, "\n}}")
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
            | JsExpr::Labeled { label, expr } => {
                writeln!(out, "{}: do {{", label)?;
                out.indent();
                expr.write_inner(out, true)?;
                out.dedent();
                write!(out, ";\n}} while(0)")
            },
            | JsExpr::Goto { label, end } if *end => {
                write!(out, "break {}", label)
            },
            | JsExpr::Goto { label, .. } => write!(out, "continue {}", label),
        }
    }
}

impl BodyCtx<'_, '_> {
    pub fn lower_expr(&mut self, expr: hir::ExprId, block: &mut Vec<JsExpr>) -> JsExpr {
        let body = self.body.clone();

        match body[expr] {
            | Expr::Hole => JsExpr::Undefined,
            | Expr::Typed { expr, .. } => self.lower_expr(expr, block),
            | Expr::Lit { ref lit } => JsExpr::Literal { lit: lit.clone() },
            | Expr::Unit => JsExpr::Undefined,
            | Expr::Path { ref path } => self.lower_path(&Resolver::for_expr(self.db.upcast(), self.owner, expr), path),
            | Expr::Do { ref stmts } => self.lower_block(stmts, block),
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
            | Expr::If { cond, then, else_ } => {
                let cond = Box::new(self.lower_expr(cond, block));
                let mut then = self.lower_expr_inline(then);
                let mut else_ = else_.map(|e| self.lower_expr_inline(e));

                if !then.is_inline() || !else_.as_ref().map(JsExpr::is_inline).unwrap_or(true) {
                    let name = format!("$e{}", u32::from(expr.into_raw()));
                    let var = JsExpr::Ident { name: name.clone() };

                    match &mut then {
                        | JsExpr::Block { exprs } => {
                            let last = exprs.pop().unwrap();

                            exprs.push(JsExpr::Assign {
                                place: Box::new(var.clone()),
                                expr: Box::new(last),
                            });
                        },
                        | other => {
                            *other = JsExpr::Assign {
                                place: Box::new(var.clone()),
                                expr: Box::new(other.clone()),
                            };
                        },
                    }

                    match else_.as_mut() {
                        | Some(JsExpr::Block { exprs }) => {
                            let last = exprs.pop().unwrap();

                            exprs.push(JsExpr::Assign {
                                place: Box::new(var.clone()),
                                expr: Box::new(last),
                            });
                        },
                        | Some(other) => {
                            *other = JsExpr::Assign {
                                place: Box::new(var.clone()),
                                expr: Box::new(other.clone()),
                            };
                        },
                        | None => {},
                    }

                    block.push(JsExpr::Var { name, expr: None });
                    block.push(JsExpr::If {
                        cond,
                        then: Box::new(then),
                        else_: else_.map(Box::new),
                    });

                    return var;
                }

                JsExpr::If {
                    cond,
                    then: Box::new(then),
                    else_: else_.map(Box::new),
                }
            },
            | Expr::Lambda { ref pats, body } => {
                let name = format!("$l{}", u32::from(expr.into_raw()));
                let mut params = Vec::with_capacity(pats.len());
                let mut exprs = Vec::new();

                for (i, &pat) in pats.iter().enumerate() {
                    let name = format!("$l{}p{}", u32::from(expr.into_raw()), i);
                    let param = JsExpr::Ident { name: name.clone() };

                    self.lower_pat(pat, param, &mut exprs);
                    params.push(name);
                }

                self.in_lambda.push(name);

                let expr = Box::new(self.lower_expr(body, &mut exprs));
                let body = Box::new(JsExpr::Return { expr });
                let name = self.in_lambda.pop().unwrap();

                block.push(JsExpr::Lambda {
                    name: name.clone(),
                    params,
                    body,
                });

                JsExpr::Ident { name }
            },
            | Expr::Recur => JsExpr::Ident {
                name: self.in_lambda.last().unwrap().clone(),
            },
            | Expr::Case { pred, ref arms } => self.lower_case(pred, arms, block),
            | ref e => {
                log::warn!(target: "lower_expr", "not yet implemented: {:?}", e);
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
                    | hir::FixityKind::Infix { assoc, prec } => ((id, assoc, prec)),
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

                    return ctx.lower_path_app(resolver, Some((id, i)), op, vec![lhs, rhs], block);
                };

                if left {
                    let lhs = exprs.next().unwrap();
                    let rhs = exprs.next().unwrap();
                    let exp = ctx.lower_path_app(resolver, Some((id, i)), op, vec![lhs, rhs], block);
                    let exprs = once(Arg::JsExpr(exp)).chain(exprs).collect::<Vec<_>>();

                    go(ctx, fixities, exprs.into_iter(), ops, id, resolver, block)
                } else {
                    let lhs = exprs.next().unwrap();
                    let rhs = go(ctx, fixities, exprs, ops, id, resolver, block);

                    ctx.lower_path_app(resolver, Some((id, i)), op, vec![lhs, Arg::JsExpr(rhs)], block)
                }
            } else {
                ctx.lower_arg(exprs.next().unwrap(), block)
            }
        }
    }

    pub fn lower_case(&mut self, pred: hir::ExprId, arms: &[hir::CaseArm], block: &mut Vec<JsExpr>) -> JsExpr {
        let name = format!("$e{}", u32::from(pred.into_raw()));
        let label = format!("$l{}", u32::from(pred.into_raw()));
        let res = JsExpr::Ident { name: name.clone() };
        let pred = self.lower_expr(pred, block);
        let mut out = Vec::new();

        block.push(JsExpr::Var { name, expr: None });

        for arm in arms {
            let mut then = Vec::new();

            if let Some(cond) = self.lower_pat(arm.pat, pred.clone(), &mut then) {
                if let Some(guard) = arm.guard {
                    let guard = self.lower_expr(guard, &mut then);
                    let mut in_guard = Vec::new();
                    let expr = self.lower_expr(arm.expr, &mut in_guard);

                    in_guard.push(JsExpr::Assign {
                        place: Box::new(res.clone()),
                        expr: Box::new(expr),
                    });

                    in_guard.push(JsExpr::Goto {
                        label: label.clone(),
                        end: true,
                    });

                    then.push(JsExpr::If {
                        cond: Box::new(guard),
                        then: Box::new(JsExpr::Block { exprs: in_guard }),
                        else_: None,
                    });
                } else {
                    let expr = self.lower_expr(arm.expr, &mut then);

                    then.push(JsExpr::Assign {
                        place: Box::new(res.clone()),
                        expr: Box::new(expr),
                    });

                    then.push(JsExpr::Goto {
                        label: label.clone(),
                        end: true,
                    });
                }

                out.push(JsExpr::If {
                    cond: Box::new(cond),
                    then: Box::new(JsExpr::Block { exprs: then }),
                    else_: None,
                });
            } else if let Some(guard) = arm.guard {
                let guard = self.lower_expr(guard, &mut out);
                let expr = self.lower_expr(arm.expr, &mut then);

                then.push(JsExpr::Assign {
                    place: Box::new(res.clone()),
                    expr: Box::new(expr),
                });

                then.push(JsExpr::Goto {
                    label: label.clone(),
                    end: true,
                });

                out.push(JsExpr::If {
                    cond: Box::new(guard),
                    then: Box::new(JsExpr::Block { exprs: then }),
                    else_: None,
                });
            } else {
                out.append(&mut then);

                let expr = self.lower_expr(arm.expr, &mut out);

                out.push(JsExpr::Assign {
                    place: Box::new(res.clone()),
                    expr: Box::new(expr),
                });

                break;
            }
        }

        block.push(JsExpr::Labeled {
            label,
            expr: Box::new(JsExpr::Block { exprs: out }),
        });

        res
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

                    if expr.is_place() {
                        self.lower_pat(pat, expr, exprs);
                    } else if self.pat_is_ignored(pat) {
                        exprs.push(expr);
                    } else {
                        let ident = format!("$p{}", u32::from(val.into_raw()));

                        exprs.push(JsExpr::Var {
                            name: ident.clone(),
                            expr: Some(Box::new(expr)),
                        });

                        self.lower_pat(pat, JsExpr::Ident { name: ident }, exprs);
                    }
                },
                | Stmt::Bind { .. } => unreachable!(),
            }
        }

        JsExpr::Undefined
    }

    pub fn member_ref(&mut self, member: hir::Member, sources: &mut dyn Iterator<Item = MethodSource>) -> JsExpr {
        let module = member.module(self.db);
        let lower = self.db.lower_member(member.into());
        let record = if module != self.owner.module(self.db.upcast()).into() {
            let base = Box::new(JsExpr::Ident {
                name: String::from("$shade"),
            });

            let base = Box::new(JsExpr::Index {
                base,
                idx: Box::new(JsExpr::Literal {
                    lit: Literal::String(module.name(self.db).to_string()),
                }),
            });

            JsExpr::Field {
                base,
                field: self.mangle(member.link_name(self.db)),
            }
        } else {
            JsExpr::Ident {
                name: self.mangle(member.link_name(self.db)),
            }
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

    pub fn lower_app(&mut self, base: Arg, args: Vec<Arg>, block: &mut Vec<JsExpr>) -> JsExpr {
        let body = self.body.clone();

        if let Arg::ExprId(id) = base {
            if let Expr::Path { ref path } = body[id] {
                let resolver = Resolver::for_expr(self.db.upcast(), self.owner, id);

                return self.lower_path_app(&resolver, Some((id, 0)), path, args, block);
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

    fn func_params(&self, id: hir::id::FuncId, is_method: bool) -> usize {
        let infer = self.db.infer(id.into());
        let mut ty = infer.self_type.ty;
        let mut first_where = true;
        let mut params = 0;

        loop {
            match ty.lookup(self.db) {
                | hir::ty::TyKind::ForAll(_, inner, _) => ty = inner,
                | hir::ty::TyKind::Where(clause, inner) => {
                    if !(is_method && first_where) {
                        params += clause
                            .constraints
                            .iter()
                            .filter(|c| !self.db.class_data(c.class).items.is_empty())
                            .count();
                    }

                    ty = inner;
                    first_where = false;
                },
                | _ => break,
            }
        }

        if self.db.func_data(id).has_body {
            let body = self.db.body(id.into());

            params + body.params().len()
        } else {
            use hir::id::Lookup;
            let lib = id.lookup(self.db.upcast()).module(self.db.upcast()).lib;
            let func_ctor = self
                .db
                .lang_item(lib, "fn-type".into())
                .unwrap()
                .as_type_ctor()
                .unwrap();

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
        expr: Option<(hir::ExprId, usize)>,
        path: &Path,
        mut args: Vec<Arg>,
        block: &mut Vec<JsExpr>,
    ) -> JsExpr {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();
        let (mut base, params, is_method) = match resolved {
            | ValueNs::Local(id) => (self.locals[id].clone(), args.len(), false),
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

                let is_method = matches!(func.as_assoc_item(self.db), Some(hir::AssocItem::Func(_)));

                (
                    self.lower_path(resolver, path),
                    self.func_params(id, is_method),
                    is_method,
                )
            },
            | ValueNs::Ctor(id) => {
                let args = args.into_iter().map(|a| self.lower_arg(a, block)).collect();

                return self.construct(id.into(), args);
            },
            | _ => (self.lower_path(resolver, path), args.len(), false),
        };

        if let Some(id) = expr {
            let infer = self.infer.clone();

            if let Some(methods) = infer.methods.get(&id) {
                let mut methods = methods.iter().copied();

                if is_method {
                    let b = match methods.next().unwrap() {
                        | MethodSource::Member(id) => self.member_ref(id.into(), &mut methods),
                        | MethodSource::Record(idx) => self.records[idx].clone(),
                    };

                    base = JsExpr::Field {
                        base: Box::new(b),
                        field: self.mangle(path.segments().last().unwrap()),
                    };
                }

                while let Some(m) = methods.next() {
                    args.insert(0, match m {
                        | MethodSource::Member(id) => Arg::JsExpr(self.member_ref(id.into(), &mut methods)),
                        | MethodSource::Record(idx) => Arg::JsExpr(self.records[idx].clone()),
                    });
                }
            }
        }

        let args2 = args
            .drain(..params)
            .map(|a| match a {
                | Arg::ExprId(id) => self.lower_expr(id, block),
                | Arg::JsExpr(expr) => expr,
            })
            .collect();

        let base = JsExpr::Call {
            base: Box::new(base),
            args: args2,
        };

        if !args.is_empty() {
            self.lower_app(Arg::JsExpr(base), args, block)
        } else {
            base
        }
    }

    pub fn lower_path(&mut self, resolver: &Resolver, path: &Path) -> JsExpr {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();
        let module = Module::from(self.owner.module(self.db.upcast()));

        match resolved {
            | ValueNs::Local(id) => self.locals[id].clone(),
            | ValueNs::Func(id) => {
                let base = if Func::from(id).module(self.db) == module {
                    let attrs = self.db.attrs(id.into());
                    let mut link_name = attrs.by_key("link_name").string_value();

                    if let Some(name) = link_name.next() {
                        JsExpr::Ident { name: name.to_string() }
                    } else if attrs.by_key("no_mangle").exists() {
                        JsExpr::Ident {
                            name: path.segments().last().unwrap().to_string(),
                        }
                    } else {
                        JsExpr::Ident {
                            name: self.mangle(path.segments().last().unwrap()),
                        }
                    }
                } else {
                    let path = Func::from(id).path(self.db);
                    self.path_expr(path)
                };

                let is_method = matches!(Func::from(id).as_assoc_item(self.db), Some(hir::AssocItem::Func(_)));

                if self.func_params(id, is_method) == 0 {
                    return JsExpr::Call {
                        base: Box::new(base),
                        args: Vec::new(),
                    };
                }

                base
            },
            | ValueNs::Const(id) => {
                if Const::from(id).module(self.db) == module {
                    JsExpr::Ident {
                        name: path.segments().last().unwrap().to_string(),
                    }
                } else {
                    let path = Const::from(id).path(self.db);
                    self.path_expr(path)
                }
            },
            | ValueNs::Static(id) => {
                let base = if Static::from(id).module(self.db) == module {
                    JsExpr::Ident {
                        name: path.segments().last().unwrap().to_string(),
                    }
                } else {
                    let path = Static::from(id).path(self.db);
                    self.path_expr(path)
                };

                JsExpr::Field {
                    base: Box::new(base),
                    field: String::from("$"),
                }
            },
            | ValueNs::Ctor(id) => self.construct(id.into(), Vec::new()),
            | ValueNs::Fixity(id) => {
                let resolver = id.resolver(self.db.upcast());
                let data = self.db.fixity_data(id);

                self.lower_path(&resolver, &data.func)
            },
        }
    }

    pub fn path_expr(&self, path: Path) -> JsExpr {
        let (ident, module) = path.segments().split_last().unwrap();
        let module = module.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("/");

        JsExpr::Field {
            base: Box::new(JsExpr::Index {
                base: Box::new(JsExpr::Ident {
                    name: String::from("$shade"),
                }),
                idx: Box::new(JsExpr::Literal {
                    lit: Literal::String(module),
                }),
            }),
            field: ident.to_string(),
        }
    }

    fn lower_arg(&mut self, arg: Arg, block: &mut Vec<JsExpr>) -> JsExpr {
        match arg {
            | Arg::ExprId(id) => self.lower_expr(id, block),
            | Arg::JsExpr(expr) => expr,
        }
    }

    pub fn lower_intrinsic(&mut self, name: &str, mut args: Vec<Arg>, block: &mut Vec<JsExpr>) -> JsExpr {
        match name {
            | "transmute" => self.lower_arg(args.remove(0), block),
            | "partial" => self.lower_arg(args.remove(0), block),
            | "unsafe" => self.lower_arg(args.remove(0), block),
            | "apply" => {
                let base = args.remove(0);

                self.lower_app(base, args, block)
            },
            | "crash" => {
                let arg = self.lower_arg(args.remove(0), block);

                block.push(JsExpr::Throw { expr: Box::new(arg) });
                JsExpr::Undefined
            },
            | "new" => match self.lower_arg(args.remove(0), block) {
                | JsExpr::Literal {
                    lit: Literal::String(s),
                } => JsExpr::Call {
                    base: Box::new(JsExpr::UnOp {
                        op: "new ",
                        rhs: Box::new(JsExpr::Ident { name: s }),
                    }),
                    args: Vec::new(),
                },
                | _ => unreachable!(),
            },
            | "spread" => JsExpr::UnOp {
                op: "...",
                rhs: Box::new(self.lower_arg(args.remove(0), block)),
            },
            | "iadd" => self.intrinsic_binop("+", args, block),
            | "isub" => self.intrinsic_binop("-", args, block),
            | "imul" => self.intrinsic_binop("*", args, block),
            | "idiv" => self.intrinsic_binop("/", args, block),
            | "irem" => self.intrinsic_binop("%", args, block),
            | "ieq" => self.intrinsic_binop("==", args, block),
            | "ge_i32" => self.intrinsic_binop(">=", args, block),
            | _ => {
                log::warn!(target: "lower_intrinsic", "todo: {:?}", name);
                JsExpr::Undefined
            },
        }
    }

    fn intrinsic_binop(&mut self, op: &'static str, mut args: Vec<Arg>, block: &mut Vec<JsExpr>) -> JsExpr {
        let rhs = args.remove(1);
        let lhs = args.remove(0);
        let lhs = Box::new(self.lower_arg(lhs, block));
        let rhs = Box::new(self.lower_arg(rhs, block));

        JsExpr::BinOp { op, lhs, rhs }
    }
}
