use crate::db::DefDatabase;
use crate::in_file::InFile;
use arena::{Arena, Idx};
use base_db::input::FileId;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::Arc;
use syntax::ast::{self, AstNode};
use syntax::ptr::{AstPtr, SyntaxNodePtr};
use syntax::syntax_node::SyntaxNode;

pub type AstId<N> = InFile<FileAstId<N>>;

#[derive(Debug)]
pub struct FileAstId<N: AstNode> {
    raw: ErasedFileAstId,
    _marker: PhantomData<fn() -> N>,
}

type ErasedFileAstId = Idx<SyntaxNodePtr>;

#[derive(Default, Debug, PartialEq, Eq)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
}

impl<N: AstNode> AstId<N> {
    pub fn to_node(&self, db: &dyn DefDatabase) -> N {
        let root = db.parse(self.file_id);

        db.ast_id_map(self.file_id).get(self.value).to_node(&root.syntax_node())
    }
}

impl<N: AstNode> FileAstId<N> {
    pub const DUMMY: Self = FileAstId {
        raw: Idx::DUMMY,
        _marker: PhantomData,
    };

    pub fn with_file_id(self, file_id: FileId) -> AstId<N> {
        AstId::new(file_id, self)
    }
}

impl AstIdMap {
    pub fn ast_id_map_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let map = Self::from_source(&db.parse(file_id).tree().syntax());

        Arc::new(map)
    }

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

    fn from_source(node: &SyntaxNode) -> Self {
        let mut res = AstIdMap::default();

        res.alloc(node);

        bfs(node, |it| {
            if let Some(module) = ast::Module::cast(it.clone()) {
                res.alloc(module.syntax());
            } else if let Some(item) = ast::Item::cast(it.clone()) {
                res.alloc(item.syntax());
            } else if let Some(item) = ast::Ctor::cast(it) {
                res.alloc(item.syntax());
            }
        });

        res
    }

    pub fn get<N: AstNode>(&self, id: FileAstId<N>) -> AstPtr<N> {
        self.arena[id.raw].cast::<N>().unwrap()
    }

    fn alloc(&mut self, item: &SyntaxNode) -> ErasedFileAstId {
        self.arena.alloc(SyntaxNodePtr::new(item))
    }
}

fn bfs(node: &SyntaxNode, mut f: impl FnMut(SyntaxNode)) {
    let mut curr_layer = vec![node.clone()];
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
