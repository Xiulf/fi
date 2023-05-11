use arena::Idx;

use crate::id::ValueDefId;
use crate::name::Name;
use crate::pat::{DecisionTree, PatId};
use crate::path::Path;
use crate::type_ref::TypeRefId;

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Missing,
    Hole(Name),
    Unit,
    Recur,
    Path {
        path: Path,
        def: Option<ValueDefId>,
    },
    Lit {
        lit: Literal,
    },
    Array {
        exprs: Box<[ExprId]>,
    },
    Lambda {
        env: Box<[PatId]>,
        params: Box<[PatId]>,
        body: ExprId,
    },
    Ref {
        expr: ExprId,
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
        branches: Box<[(PatId, ExprId)]>,
        decision_tree: DecisionTree,
    },
    Return {
        expr: ExprId,
    },
    Typed {
        expr: ExprId,
        ty: TypeRefId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Literal {
    Int(i128),
    Float(u64),
    Char(char),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Stmt {
    Let(PatId, ExprId),
    Expr(ExprId),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Int(l) => l.fmt(f),
            | Self::Float(l) => f64::from_bits(*l).fmt(f),
            | Self::Char(l) => write!(f, "{l:?}"),
            | Self::String(l) => write!(f, "{l:?}"),
        }
    }
}
