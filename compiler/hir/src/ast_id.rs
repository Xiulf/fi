use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::Arc;

use arena::{Arena, Idx};
use syntax::ast::{self, AstNode};
use syntax::ptr::{AstPtr, SyntaxNodePtr};
use syntax::SyntaxNode;
use vfs::{File, InFile};

use crate::Db;

#[derive(Default, Debug, PartialEq, Eq, Hash)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
}

#[derive(Debug)]
pub struct AstId<N: AstNode>(pub InFile<FileAstId<N>>);

#[derive(Debug)]
pub struct FileAstId<N: AstNode> {
    raw: Idx<SyntaxNodePtr>,
    _marker: PhantomData<fn() -> N>,
}

impl<N: AstNode> AstId<N> {
    pub fn to_node(self, db: &dyn Db) -> N {
        let root = base_db::parse(db, self.0.file);

        query(db, self.0.file).get(self.0.value).to_node(root.syntax())
    }
}

impl<N: AstNode> FileAstId<N> {
    pub fn with_file(self, file: File) -> AstId<N> {
        AstId(InFile::new(file, self))
    }
}

#[salsa::tracked]
pub fn query(db: &dyn Db, file: File) -> Arc<AstIdMap> {
    let root = base_db::parse(db, file);
    Arc::new(AstIdMap::from_source(root.syntax()))
}

impl AstIdMap {
    pub fn ast_id<N: AstNode>(&self, item: &N) -> FileAstId<N> {
        let ptr = SyntaxNodePtr::new(item.syntax());
        let raw = match self.arena.iter().find(|(_, i)| **i == ptr) {
            | Some((it, _)) => it,
            | None => unreachable!(),
        };

        FileAstId {
            raw,
            _marker: PhantomData,
        }
    }

    pub fn get<N: AstNode>(&self, id: FileAstId<N>) -> AstPtr<N> {
        self.arena[id.raw].cast().unwrap()
    }

    fn alloc(&mut self, item: &SyntaxNode) -> Idx<SyntaxNodePtr> {
        self.arena.alloc(SyntaxNodePtr::new(item))
    }

    fn from_source(node: &SyntaxNode) -> Self {
        let mut res = Self::default();

        res.alloc(node);

        bfs(node, |it| {
            if let Some(item) = ast::Item::cast(it) {
                res.alloc(item.syntax());
            } else if let Some(item) = ast::Ctor::cast(it) {
                res.alloc(item.syntax());
            } else if let Some(item) = ast::CtorField::cast(it) {
                res.alloc(item.syntax());
            }
        });

        res
    }
}

fn bfs(node: &SyntaxNode, mut f: impl FnMut(&SyntaxNode)) {
    let mut curr_layer = vec![node];
    let mut next_layer = Vec::new();

    while !curr_layer.is_empty() {
        curr_layer.drain(..).for_each(|node| {
            next_layer.extend(node.children());
            f(node);
        });

        std::mem::swap(&mut curr_layer, &mut next_layer);
    }
}

impl<N: AstNode> Clone for FileAstId<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: AstNode> Copy for FileAstId<N> {
}

impl<N: AstNode> PartialEq for FileAstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode> Eq for FileAstId<N> {
}

impl<N: AstNode> Hash for FileAstId<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

impl<N: AstNode> Clone for AstId<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: AstNode> Copy for AstId<N> {
}

impl<N: AstNode> PartialEq for AstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<N: AstNode> Eq for AstId<N> {
}

impl<N: AstNode> Hash for AstId<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
