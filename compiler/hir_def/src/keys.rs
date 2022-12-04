use std::marker::PhantomData;

use rustc_hash::FxHashMap;
use syntax::{ast, AstNode, AstPtr};

use crate::dyn_map::Policy;
use crate::id::{FuncId, MemberId};
use crate::in_file::InFile;

pub type Key<K, V> = crate::dyn_map::Key<InFile<K>, V, AstPtrPolicy<K, V>>;

pub const FUNC: Key<ast::ItemFunc, FuncId> = Key::new();
pub const MEMBER: Key<ast::ItemMember, MemberId> = Key::new();

pub struct AstPtrPolicy<AST, ID> {
    _marker: PhantomData<(AST, ID)>,
}

impl<AST: AstNode + 'static, ID: 'static> Policy for AstPtrPolicy<AST, ID> {
    type K = InFile<AST>;
    type V = ID;

    fn insert(map: &mut crate::dyn_map::DynMap, key: Self::K, value: Self::V) {
        let key = key.as_ref().map(AstPtr::new);

        map.map
            .entry::<FxHashMap<InFile<AstPtr<AST>>, ID>>()
            .or_insert_with(Default::default)
            .insert(key, value);
    }

    fn get<'a>(map: &'a crate::dyn_map::DynMap, key: &Self::K) -> Option<&'a Self::V> {
        let key = key.as_ref().map(AstPtr::new);

        map.map.get::<FxHashMap<InFile<AstPtr<AST>>, ID>>()?.get(&key)
    }
}
