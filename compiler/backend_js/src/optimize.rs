use super::*;
use crate::expr::JsExpr;

impl BodyCtx<'_, '_> {
    pub fn wrap_return(&self, expr: JsExpr) -> JsExpr {
        match expr {
            | JsExpr::Block { mut exprs } => match exprs[..] {
                | [.., JsExpr::Var { ref name, expr: None }, JsExpr::If {
                    ref cond,
                    ref then,
                    else_: Some(ref else_),
                }, JsExpr::Ident { name: ref name2 }]
                    if name == name2 =>
                {
                    let ifelse = JsExpr::If {
                        cond: cond.clone(),
                        then: Box::new(self.return_assign(name, *then.clone())),
                        else_: Some(Box::new(self.return_assign(name, *else_.clone()))),
                    };

                    exprs.resize(exprs.len() - 3, JsExpr::Undefined);
                    exprs.push(ifelse);

                    JsExpr::Block { exprs }
                },
                | _ => {
                    let last = exprs.pop().unwrap();

                    exprs.push(JsExpr::Return { expr: Box::new(last) });
                    JsExpr::Block { exprs }
                },
            },
            | JsExpr::If { cond, then, else_ } => {
                let then = Box::new(self.wrap_return(*then));
                let else_ = else_.map(|e| Box::new(self.wrap_return(*e)));

                JsExpr::If { cond, then, else_ }
            },
            | _ => JsExpr::Return { expr: Box::new(expr) },
        }
    }

    fn return_assign(&self, name: &String, expr: JsExpr) -> JsExpr {
        match expr {
            | JsExpr::Assign { place, expr } => match &*place {
                | JsExpr::Ident { name: name2 } if name == name2 => JsExpr::Return { expr },
                | _ => JsExpr::Assign { place, expr },
            },
            | JsExpr::Block { mut exprs } => {
                let last = exprs.pop().unwrap();

                exprs.push(self.return_assign(name, last));
                JsExpr::Block { exprs }
            },
            | JsExpr::If {
                cond,
                then,
                else_: Some(else_),
            } if !expr.is_inline() => {
                let then = Box::new(self.return_assign(name, *then));
                let else_ = Box::new(self.return_assign(name, *else_));

                JsExpr::If {
                    cond,
                    then,
                    else_: Some(else_),
                }
            },
            | _ => expr,
        }
    }

    pub fn get_result(&self, id: hir::ExprId, expr: JsExpr, block: &mut Vec<JsExpr>) -> JsExpr {
        match expr {
            | JsExpr::If { else_: Some(_), .. } if !expr.is_inline() => {
                let name = format!("${}", u32::from(id.into_raw()));

                block.push(JsExpr::Var {
                    name: name.clone(),
                    expr: None,
                });

                block.push(self.get_result_inner(name.clone(), expr));

                JsExpr::Ident { name }
            },
            | _ => expr,
        }
    }

    fn get_result_inner(&self, name: String, expr: JsExpr) -> JsExpr {
        match expr {
            | JsExpr::Block { mut exprs } => {
                let last = exprs.pop().unwrap();

                exprs.push(self.get_result_inner(name, last));
                JsExpr::Block { exprs }
            },
            | JsExpr::If {
                cond,
                then,
                else_: Some(else_),
            } if !expr.is_inline() => {
                let then = Box::new(self.get_result_inner(name.clone(), *then));
                let else_ = Box::new(self.get_result_inner(name.clone(), *else_));

                JsExpr::If {
                    cond,
                    then,
                    else_: Some(else_),
                }
            },
            | _ if expr.is_terminator() => expr,
            | _ => JsExpr::Assign {
                place: Box::new(JsExpr::Ident { name }),
                expr: Box::new(expr),
            },
        }
    }
}
