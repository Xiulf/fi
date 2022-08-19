use arena::Idx;

use crate::name::Name;
use crate::pat::PatId;
use crate::path::Path;
use crate::type_ref::LocalTypeRefId;

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Typed {
        expr: ExprId,
        ty: LocalTypeRefId,
    },
    Hole,
    Unit,
    Path {
        path: Path,
    },
    Lit {
        lit: Literal,
    },
    Infix {
        exprs: Box<[ExprId]>,
        ops: Box<[Path]>,
    },
    App {
        base: ExprId,
        arg: ExprId,
    },
    Field {
        base: ExprId,
        field: Name,
    },
    Index {
        base: ExprId,
        index: ExprId,
    },
    Record {
        fields: Box<[RecordField<ExprId>]>,
    },
    Array {
        exprs: Box<[ExprId]>,
    },
    Do {
        stmts: Box<[Stmt]>,
    },
    Try {
        stmts: Box<[Stmt]>,
    },
    Lambda {
        pats: Box<[PatId]>,
        body: ExprId,
    },
    If {
        cond: ExprId,
        then: ExprId,
        else_: Option<ExprId>,
    },
    Case {
        pred: ExprId,
        arms: Box<[CaseArm]>,
    },
    Recur,
    Return {
        expr: ExprId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    String(String),
    Char(char),
    Int(i128),
    Float(u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordField<ID> {
    pub name: Name,
    pub val: ID,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stmt {
    Let { pat: PatId, val: ExprId },
    Bind { pat: PatId, val: ExprId },
    Expr { expr: ExprId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArm {
    pub pat: PatId,
    pub value: CaseValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CaseValue {
    Normal(ExprId),
    Guarded(Box<[ExprId]>, Box<[ExprId]>),
}

impl Expr {
    pub fn walk(&self, mut f: impl FnMut(ExprId)) {
        match self {
            | Expr::Missing | Expr::Hole | Expr::Unit | Expr::Path { .. } | Expr::Lit { .. } | Expr::Recur => {},
            | Expr::Typed { expr, .. } => f(*expr),
            | Expr::Infix { exprs, .. } => {
                exprs.iter().copied().for_each(f);
            },
            | Expr::App { base, arg } => {
                f(*base);
                f(*arg);
            },
            | Expr::Field { base, .. } => f(*base),
            | Expr::Index { base, index } => {
                f(*base);
                f(*index);
            },
            | Expr::Array { exprs } => {
                exprs.iter().copied().for_each(f);
            },
            | Expr::Record { fields } => {
                fields.iter().for_each(|i| f(i.val));
            },
            | Expr::Do { stmts } | Expr::Try { stmts } => {
                stmts.iter().for_each(|stmt| match stmt {
                    | Stmt::Let { val, .. } => f(*val),
                    | Stmt::Bind { val, .. } => f(*val),
                    | Stmt::Expr { expr } => f(*expr),
                });
            },
            | Expr::Lambda { pats: _, body } => f(*body),
            | Expr::If { cond, then, else_, .. } => {
                f(*cond);
                f(*then);
                else_.map(f);
            },
            | Expr::Case { pred, arms } => {
                f(*pred);

                arms.iter().for_each(|arm| match &arm.value {
                    | CaseValue::Normal(e) => f(*e),
                    | CaseValue::Guarded(guards, exprs) => {
                        let guards = guards.iter().copied();
                        let mut exprs = exprs.iter().copied();

                        guards.zip(exprs.by_ref()).for_each(|(g, e)| {
                            f(g);
                            f(e);
                        });

                        exprs.next().map(&mut f);
                    },
                });
            },
            | Expr::Return { expr } => {
                f(*expr);
            },
        }
    }
}
