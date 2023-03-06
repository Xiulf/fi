use std::collections::{BTreeMap, BTreeSet};

use arena::Idx;

use crate::expr::{ExprId, Literal};
use crate::id::CtorId;
use crate::name::Name;
use crate::path::Path;
use crate::type_ref::TypeRefId;
use crate::Db;

pub type PatId = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Missing,
    Wildcard,
    Lit {
        lit: Literal,
    },
    Bind {
        name: Name,
        subpat: Option<PatId>,
    },
    Ctor {
        path: Path,
        ctor: Option<CtorId>,
        args: Box<[PatId]>,
    },
    Typed {
        pat: PatId,
        ty: TypeRefId,
    },
}

#[derive(Default, Debug, PartialEq, Eq, Hash)]
pub struct PatternMatrix {
    pub(super) rows: Vec<(PatternStack, usize)>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternStack(pub(super) Vec<(Constructor, PatId)>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constructor {
    MatchAll,
    Variant(VariantTag, PatternStack),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VariantTag {
    Ctor(CtorId),
    Literal(Literal),
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub enum DecisionTree {
    #[default]
    Fail,
    Leaf(usize),
    Guard(ExprId, Box<DecisionTree>),
    Switch(PatId, Vec<Case>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Case {
    pub tag: Option<VariantTag>,
    pub fields: Vec<Vec<PatId>>,
    pub branch: DecisionTree,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct DecisionTreeResult {
    pub tree: DecisionTree,
    pub reachable_branches: BTreeSet<usize>,
    pub missed_case_count: usize,
}

impl PatternMatrix {
    pub fn compile(&mut self, db: &dyn Db) -> DecisionTreeResult {
        if self.rows.is_empty() {
            DecisionTreeResult {
                missed_case_count: 1,
                ..Default::default()
            }
        } else if self.first_row_matches_all() {
            let mut reachable_branches = BTreeSet::default();
            reachable_branches.insert(self.rows[0].1);

            DecisionTreeResult {
                tree: DecisionTree::Leaf(self.rows[0].1),
                reachable_branches,
                missed_case_count: 0,
            }
        } else {
            for (row, _) in self.rows.iter() {
                match row.head() {
                    | Some((Constructor::Variant(_, _), _)) => return self.switch_on_pattern(db),
                    | Some((Constructor::MatchAll, _)) => continue,
                    | None => unreachable!(),
                }
            }

            match self.find_first_non_default_column() {
                | Some(column) => self.swap_column(db, column),
                | None => self.default_specialize(db).0,
            }
        }
    }

    fn swap_column(&mut self, db: &dyn Db, column: usize) -> DecisionTreeResult {
        for (row, _) in self.rows.iter_mut() {
            row.0.swap(0, column);
        }

        self.compile(db)
    }

    fn first_row_matches_all(&self) -> bool {
        (self.rows[0].0).0.iter().all(|(ctor, _)| ctor.is_match_all())
    }

    fn find_first_non_default_column(&self) -> Option<usize> {
        let len = self.rows[0].0.len();

        for col in (1..len).rev() {
            for (row, _) in self.rows.iter() {
                match row.0.get(col) {
                    | Some((Constructor::MatchAll, _)) => continue,
                    | _ => return Some(col),
                }
            }
        }

        None
    }

    fn specialize(&self, tag: &VariantTag, arity: usize, fields: &mut Vec<Vec<PatId>>) -> Self {
        let mut matrix = Self::default();

        for (row, branch) in self.rows.iter() {
            if let Some(row) = row.specialize_row(tag, arity, fields) {
                matrix.rows.push((row, *branch));
            }
        }

        matrix
    }

    fn default_specialize(&self, db: &dyn Db) -> (DecisionTreeResult, Vec<PatId>) {
        let mut matrix = Self::default();
        let mut variables = Vec::new();

        for (row, branch) in self.rows.iter() {
            if let Some((row, var)) = row.default_specialize_row() {
                matrix.rows.push((row, *branch));
                variables.push(var);
            }
        }

        (matrix.compile(db), variables)
    }

    fn switch_on_pattern(&mut self, db: &dyn Db) -> DecisionTreeResult {
        let mut matched_variants = BTreeMap::<_, Vec<_>>::default();
        let mut switch_on = None;

        for (row, _) in self.rows.iter() {
            if let Some((Constructor::Variant(tag, fields), var)) = row.head() {
                switch_on = Some(*var);
                matched_variants.entry(tag).or_default().push(fields);
            }
        }

        let missed_cases = Self::get_missing_cases(db, &matched_variants);
        let mut missed_case_count = 0;
        let mut reachable_branches = BTreeSet::default();
        let mut cases = matched_variants
            .into_iter()
            .map(|(tag, fields)| {
                let arity = fields[0].len();
                let mut fields = Self::collect_fields(fields);
                let branch = self.specialize(tag, arity, &mut fields).compile(db);

                fields.reverse();
                missed_case_count += branch.missed_case_count;
                reachable_branches = reachable_branches.union(&branch.reachable_branches).copied().collect();

                Case {
                    tag: Some(tag.clone()),
                    fields,
                    branch: branch.tree,
                }
            })
            .collect::<Vec<_>>();

        if !missed_cases.is_empty() {
            let (branch, fields) = self.default_specialize(db);
            switch_on = fields.get(0).copied().or(switch_on);
            missed_case_count += branch.missed_case_count;
            reachable_branches = reachable_branches.union(&branch.reachable_branches).copied().collect();
            cases.push(Case {
                tag: None,
                fields: vec![fields],
                branch: branch.tree,
            });
        }

        tracing::debug!("{missed_cases:?}, {missed_case_count}, {reachable_branches:?}");

        DecisionTreeResult {
            tree: DecisionTree::Switch(switch_on.unwrap(), cases),
            reachable_branches,
            missed_case_count,
        }
    }

    fn collect_fields(rows: Vec<&PatternStack>) -> Vec<Vec<PatId>> {
        let mut variables = Vec::new();

        for col in 0..rows[0].len() {
            variables.push(Vec::new());

            for row in rows.iter().copied() {
                if let Some((Constructor::MatchAll, id)) = row.0.get(col) {
                    variables[col].push(*id);
                }
            }
        }

        variables
    }

    fn get_missing_cases<T>(db: &dyn Db, variants: &BTreeMap<&VariantTag, T>) -> BTreeSet<VariantTag> {
        let mut result = BTreeSet::default();

        match variants.iter().next().map(|(tag, _)| *tag).unwrap() {
            | VariantTag::Literal(lit) => {
                result.insert(VariantTag::Literal(lit.clone()));
            },
            | VariantTag::Ctor(id) => {
                let type_ctor = id.type_ctor(db);
                let it = type_ctor.it(db);
                let item_tree = crate::item_tree::query(db, it.file);
                let data = &item_tree[it.value];

                for &ctor in data.ctors.iter() {
                    let ctor_id = CtorId::new(db, type_ctor, ctor);
                    result.insert(VariantTag::Ctor(ctor_id));
                }

                for (tag, _) in variants.iter() {
                    result.remove(tag);
                }
            },
        }

        result
    }
}

impl IntoIterator for PatternStack {
    type Item = (Constructor, PatId);
    type IntoIter = std::iter::Rev<std::vec::IntoIter<Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().rev()
    }
}

impl PatternStack {
    fn len(&self) -> usize {
        self.0.len()
    }

    pub fn set_id(&mut self, id: PatId) {
        self.0.last_mut().unwrap().1 = id;
    }

    pub fn head(&self) -> Option<&(Constructor, PatId)> {
        self.0.last()
    }

    fn specialize_row(&self, tag: &VariantTag, arity: usize, fields: &mut Vec<Vec<PatId>>) -> Option<Self> {
        match self.head() {
            | Some((head, _)) if head.matches(tag) => {
                let mut new_stack = self.0.clone();
                let (head, _) = new_stack.pop().unwrap();

                new_stack.append(&mut head.take_n_fields(arity, fields));

                Some(Self(new_stack))
            },
            | _ => None,
        }
    }

    fn default_specialize_row(&self) -> Option<(Self, PatId)> {
        self.head().filter(|(ctor, _)| ctor.is_match_all()).map(|ctor| {
            let stack = self.0.iter().take(self.0.len() - 1).cloned().collect();

            (Self(stack), ctor.1)
        })
    }
}

impl Constructor {
    fn is_match_all(&self) -> bool {
        matches!(self, Self::MatchAll)
    }

    fn matches(&self, tag: &VariantTag) -> bool {
        match self {
            | Self::MatchAll => true,
            | Self::Variant(t, _) => t == tag,
        }
    }

    fn repeat_matchall(n: usize, field_ids: &[Vec<PatId>]) -> Vec<(Constructor, PatId)> {
        assert_eq!(field_ids.len(), n);

        (0..n)
            .map(|i| {
                let id = field_ids[i].get(0).copied().unwrap();

                (Self::MatchAll, id)
            })
            .collect()
    }

    fn set_id(pair: &mut (Constructor, PatId), new_ids: &mut Vec<PatId>) {
        match new_ids.get_mut(0) {
            | Some(new_id) => pair.1 = *new_id,
            | None => new_ids.push(pair.1),
        }
    }

    fn take_n_fields(self, n: usize, field_ids: &mut Vec<Vec<PatId>>) -> Vec<(Constructor, PatId)> {
        match self {
            | Self::MatchAll => Constructor::repeat_matchall(n, field_ids),
            | Self::Variant(_, mut fields) => {
                assert_eq!(fields.0.len(), n);
                assert_eq!(field_ids.len(), n);

                for (field, ids) in fields.0.iter_mut().zip(field_ids.iter_mut()) {
                    Self::set_id(field, ids);
                }

                fields.0
            },
        }
    }
}
