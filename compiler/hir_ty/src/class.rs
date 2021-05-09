use crate::lower::diagnostics::LowerDiagnostic;
use crate::ty::{Constraint, Ty, TypeVar};
use hir_def::id::{ClassId, InstanceId};

#[derive(Debug, PartialEq, Eq)]
pub struct Class {
    pub id: ClassId,
    pub vars: Box<[Ty]>,
    pub fundeps: Box<[FunDep]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunDep {
    pub determiners: Box<[TypeVar]>,
    pub determined: Box<[TypeVar]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instances {
    matchers: Box<[InstanceMatcher]>,
}

#[derive(Debug, PartialEq, Eq)]
struct InstanceMatcher {
    types: Box<[Ty]>,
    constraints: Box<[InstanceConstraint]>,
    instance: Option<InstanceId>,
}

#[derive(Debug, PartialEq, Eq)]
struct InstanceConstraint {
    constraint: Constraint,
    constraints: Box<[InstanceConstraint]>,
    instance: Option<InstanceId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassLowerResult {
    pub class: Class,
    pub diagnostics: Vec<LowerDiagnostic>,
}
