use std::marker::PhantomData;

use rustc_hash::FxHashMap;
use syntax::{ast, AstNode, AstPtr};

use crate::dyn_map::Policy;
use crate::id::{ClassId, ConstId, CtorId, FuncId, MemberId, StaticId};

pub type Key<K, V> = crate::dyn_map::Key<K, V, AstPtrPolicy<K, V>>;

pub const FUNC: Key<ast::ItemFunc, FuncId> = Key::new();
pub const CONST: Key<ast::ItemConst, ConstId> = Key::new();
pub const STATIC: Key<ast::ItemStatic, StaticId> = Key::new();
pub const CTOR: Key<ast::Ctor, CtorId> = Key::new();
pub const CLASS: Key<ast::ItemClass, ClassId> = Key::new();
pub const MEMBER: Key<ast::ItemMember, MemberId> = Key::new();

pub struct AstPtrPolicy<AST, ID> {
    _marker: PhantomData<(AST, ID)>,
}

impl<AST: AstNode + 'static, ID: 'static> Policy for AstPtrPolicy<AST, ID> {
    type K = AST;
    type V = ID;

    fn insert(map: &mut crate::dyn_map::DynMap, key: Self::K, value: Self::V) {
        let key = AstPtr::new(&key);

        map.map
            .entry::<FxHashMap<AstPtr<AST>, ID>>()
            .or_insert_with(Default::default)
            .insert(key, value);
    }

    fn get<'a>(map: &'a crate::dyn_map::DynMap, key: &Self::K) -> Option<&'a Self::V> {
        let key = AstPtr::new(key);

        map.map.get::<FxHashMap<AstPtr<AST>, ID>>()?.get(&key)
    }
}
