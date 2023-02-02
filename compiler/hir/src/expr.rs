use arena::Idx;

use crate::id::ValueDefId;
use crate::name::Name;
use crate::pat::{DecisionTree, PatId};
use crate::path::Path;

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Missing,
    Hole(Name),
    Unit,
    Path {
        path: Path,
        def: Option<ValueDefId>,
    },
    Lit {
        lit: Literal,
    },
    Lambda {
        params: Box<[PatId]>,
        body: ExprId,
    },
    App {
        base: ExprId,
        args: Box<[ExprId]>,
    },
    Block {
        stmts: Box<[Stmt]>,
        expr: Option<ExprId>,
    },
    If {
        cond: ExprId,
        then: ExprId,
        else_: Option<ExprId>,
    },
    Match {
        expr: ExprId,
        branches: Box<[ExprId]>,
        decision_tree: DecisionTree,
    },
    Return {
        expr: ExprId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i128),
    Float(u64),
    Char(char),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Let(PatId, ExprId),
    Expr(ExprId),
}
