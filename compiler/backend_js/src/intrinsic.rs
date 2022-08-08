use hir::ty::{TyKind, TypeVar};
use hir::Literal;

use crate::expr::{Arg, JsExpr};
use crate::BodyCtx;

impl BodyCtx<'_, '_> {
    pub fn get_type_var(&mut self, var: TypeVar) -> JsExpr {
        self.type_vars[var.idx() as usize].clone()
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
            | "applyFlipped" => {
                let base = args.remove(1);

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
            | "symToStr" => match args.remove(0) {
                | Arg::ExprId(expr) => {
                    let sym = match self.infer.type_of_expr[expr].lookup(self.db) {
                        | TyKind::App(_, a) => match a[0].lookup(self.db) {
                            | TyKind::Symbol(s) => s,
                            | TyKind::TypeVar(id) => return self.get_type_var(id),
                            | _ => unreachable!(),
                        },
                        | _ => unreachable!(),
                    };

                    JsExpr::Literal {
                        lit: Literal::String(sym.into()),
                    }
                },
                | Arg::JsExpr(_) => unreachable!(),
            },
            | "assign" => JsExpr::Assign {
                place: Box::new(self.lower_arg(args.remove(0), block)),
                expr: Box::new(self.lower_arg(args.remove(0), block)),
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
            | "icmp" => self.intrinsic_icmp(args, block),
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

    fn intrinsic_icmp(&mut self, mut args: Vec<Arg>, block: &mut Vec<JsExpr>) -> JsExpr {
        let rhs = args.remove(1);
        let lhs = args.remove(0);
        let lhs = Box::new(self.lower_arg(lhs, block));
        let rhs = Box::new(self.lower_arg(rhs, block));

        JsExpr::If {
            cond: Box::new(JsExpr::BinOp {
                op: "==",
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            then: Box::new(JsExpr::Literal { lit: Literal::Int(1) }),
            else_: Some(Box::new(JsExpr::If {
                cond: Box::new(JsExpr::BinOp { op: "<", lhs, rhs }),
                then: Box::new(JsExpr::Literal { lit: Literal::Int(-1) }),
                else_: Some(Box::new(JsExpr::Literal { lit: Literal::Int(1) })),
            })),
        }
    }
}
