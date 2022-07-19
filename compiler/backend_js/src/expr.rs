use std::io::{self, Write};

use hir::id::HasModule;
use hir::{Const, Expr, Func, HasResolver, Literal, Module, Pat, Path, Resolver, Static, Stmt, ValueNs};

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
    Return {
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
            | Self::If {
                cond,
                then,
                else_: Some(else_),
            } => cond.is_inline() && then.is_inline() && else_.is_inline(),
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
            | Self::Assign { .. } | Self::Call { .. } | Self::Var { .. } | Self::Return { .. } => true,
            | Self::Field { base, .. } | Self::Index { base, .. } => base.is_effectful(),
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

                        if i < exprs.len() - 1 {
                            writeln!(out, ";")?;
                        }
                    }
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
            | Expr::Infix { ref exprs, ref ops } => {
                let mut exprs = exprs.iter().copied();
                let mut lhs = Arg::ExprId(exprs.next().unwrap());
                let resolver = Resolver::for_expr(self.db.upcast(), self.owner, expr);

                for (rhs, op) in exprs.zip(ops.iter()) {
                    lhs = Arg::JsExpr(self.lower_path_app(&resolver, op, vec![lhs, Arg::ExprId(rhs)], block));
                }

                self.lower_arg(lhs, block)
            },
            | Expr::If { cond, then, else_ } => {
                let cond = Box::new(self.lower_expr(cond, block));
                let mut then = self.lower_expr_inline(then);
                let mut else_ = else_.map(|e| self.lower_expr_inline(e));

                if !then.is_inline() || !else_.as_ref().map(JsExpr::is_inline).unwrap_or(true) {
                    let name = format!("${}", u32::from(expr.into_raw()));
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

    pub fn lower_case(&mut self, pred: hir::ExprId, _arms: &[hir::CaseArm], block: &mut Vec<JsExpr>) -> JsExpr {
        let pred = self.lower_expr(pred, block);

        pred
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
                        let ident = format!("${}", u32::from(val.into_raw()));

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

    fn pat_is_ignored(&self, pat: hir::PatId) -> bool {
        match self.body[pat] {
            | Pat::Missing | Pat::Wildcard => true,
            | Pat::Typed { pat, .. } => self.pat_is_ignored(pat),
            | Pat::App { base, ref args } => self.pat_is_ignored(base) && args.iter().all(|&p| self.pat_is_ignored(p)),
            | Pat::Infix { ref pats, .. } => pats.iter().all(|&p| self.pat_is_ignored(p)),
            | Pat::Record { ref fields, .. } => fields.iter().all(|f| self.pat_is_ignored(f.val)),
            | _ => false,
        }
    }

    pub fn lower_pat(&mut self, pat: hir::PatId, place: JsExpr, exprs: &mut Vec<JsExpr>) {
        let body = self.body.clone();

        match body[pat] {
            | Pat::Missing => {},
            | Pat::Wildcard => {},
            | Pat::Typed { pat, .. } => self.lower_pat(pat, place, exprs),
            | Pat::Bind { subpat, .. } => {
                self.locals.insert(pat, place.clone());

                if let Some(subpat) = subpat {
                    self.lower_pat(subpat, place, exprs);
                }
            },
            | ref p => {
                log::warn!(target: "lower_pat", "not yet implemented: {:?}", p);
            },
        }
    }

    pub fn lower_app(&mut self, base: Arg, args: Vec<Arg>, block: &mut Vec<JsExpr>) -> JsExpr {
        let body = self.body.clone();

        if let Arg::ExprId(id) = base {
            if let Expr::Path { ref path } = body[id] {
                let resolver = Resolver::for_expr(self.db.upcast(), self.owner, id);

                return self.lower_path_app(&resolver, path, args, block);
            }
        }

        let base = Box::new(self.lower_arg(base, block));
        let args = args
            .into_iter()
            .map(|a| match a {
                | Arg::ExprId(id) => self.lower_expr(id, block),
                | Arg::JsExpr(expr) => expr,
            })
            .collect();

        JsExpr::Call { base, args }
    }

    pub fn lower_path_app(
        &mut self,
        resolver: &Resolver,
        path: &Path,
        args: Vec<Arg>,
        block: &mut Vec<JsExpr>,
    ) -> JsExpr {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();
        let base = match resolved {
            | ValueNs::Local(id) => self.locals[id].clone(),
            | ValueNs::Fixity(id) => {
                let resolver = id.resolver(self.db.upcast());
                let data = self.db.fixity_data(id);

                return self.lower_path_app(&resolver, &data.func, args, block);
            },
            | ValueNs::Func(id) => {
                let func = Func::from(id);

                if func.is_intrinsic(self.db) {
                    return self.lower_intrinsic(&func.name(self.db).to_string(), args, block);
                }

                self.lower_path(resolver, path)
            },
            | _ => self.lower_path(resolver, path),
        };

        let args = args
            .into_iter()
            .map(|a| match a {
                | Arg::ExprId(id) => self.lower_expr(id, block),
                | Arg::JsExpr(expr) => expr,
            })
            .collect();

        JsExpr::Call {
            base: Box::new(base),
            args,
        }
    }

    pub fn lower_path(&mut self, resolver: &Resolver, path: &Path) -> JsExpr {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();
        let module = Module::from(self.owner.module(self.db.upcast()));

        match resolved {
            | ValueNs::Local(id) => self.locals[id].clone(),
            | ValueNs::Func(id) => {
                if Func::from(id).module(self.db) == module {
                    JsExpr::Ident {
                        name: path.segments().last().unwrap().to_string(),
                    }
                } else {
                    let path = Func::from(id).path(self.db);
                    self.path_expr(path)
                }
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
                if Static::from(id).module(self.db) == module {
                    JsExpr::Ident {
                        name: path.segments().last().unwrap().to_string(),
                    }
                } else {
                    let path = Static::from(id).path(self.db);
                    self.path_expr(path)
                }
            },
            | ValueNs::Ctor(_id) => JsExpr::Undefined,
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

    pub fn lower_arg(&mut self, arg: Arg, block: &mut Vec<JsExpr>) -> JsExpr {
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
            | "ge_i32" => {
                let rhs = args.remove(1);
                let lhs = args.remove(0);
                let lhs = Box::new(self.lower_arg(lhs, block));
                let rhs = Box::new(self.lower_arg(rhs, block));

                JsExpr::BinOp { op: ">=", lhs, rhs }
            },
            | _ => {
                log::warn!(target: "lower_intrinsic", "todo: {:?}", name);
                JsExpr::Undefined
            },
        }
    }
}
