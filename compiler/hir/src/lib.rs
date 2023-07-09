#![feature(trait_upcasting)]

use base_db::libs::LibId;
use diagnostics::{DiagnosticSink, Diagnostics};
use hir_def::attrs::Attrs;
use hir_def::body::Body;
use hir_def::data;
use hir_def::id::{
    ContainerId, CtorId, FieldId, FixityId, HasModule, ImplId, ItemId, ModuleId, ModuleParentId, TraitId, TypeAliasId,
    TypeCtorId, TypeVarId, ValueId,
};
use hir_def::item_tree::FixityKind;
use hir_def::name::{AsName, Name};
use hir_def::path::Path;
pub use hir_def::{attrs, body, display, expr, id, pat, source};
use hir_ty::ty::{Constraint, Generalized, GeneralizedType, Ty};
use ra_ap_stdx::hash::NoHashHashMap;
use salsa::AsId;
use triomphe::Arc;

pub trait Db: hir_ty::Db + salsa::DbWithJar<Jar> {}

impl<T: hir_ty::Db + salsa::DbWithJar<Jar>> Db for T {
}

#[salsa::jar(db = Db)]
pub struct Jar(lib_diagnostics);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lib {
    id: LibId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    id: ModuleId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Item {
    Fixity(Fixity),
    Value(Value),
    TypeCtor(TypeCtor),
    TypeAlias(TypeAlias),
    Trait(Trait),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Container {
    Module(Module),
    Trait(Trait),
    Impl(Impl),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Fixity {
    id: FixityId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value {
    id: ValueId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ctor {
    id: CtorId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Field {
    id: FieldId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeCtor {
    id: TypeCtorId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeAlias {
    id: TypeAliasId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Trait {
    id: TraitId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Impl {
    id: ImplId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar {
    id: TypeVarId,
}

#[salsa::tracked]
fn lib_diagnostics(db: &dyn Db, id: LibId) {
    let lib = Lib { id };

    for module in lib.modules(db) {
        module.run_all_queries(db);
    }
}

impl Lib {
    pub fn name(self, db: &dyn Db) -> Name {
        self.id.name(db).as_name(db)
    }

    pub fn deps(self, db: &dyn Db) -> Vec<Lib> {
        self.id.deps(db).into_iter().map(|&l| l.into()).collect()
    }

    pub fn modules(self, db: &dyn Db) -> Vec<Module> {
        let def_map = hir_def::def_map::query(db, self.id);

        def_map.modules().map(|(id, _)| Module { id }).collect()
    }

    pub fn diagnostics(self, db: &dyn Db, sink: &mut dyn DiagnosticSink) {
        lib_diagnostics(db, self.id);
        let diagnostics = lib_diagnostics::accumulated::<Diagnostics>(db, self.id);

        for diag in diagnostics.into_iter() {
            sink.add_diagnostic(diag);
        }
    }
}

impl Module {
    pub fn id(self) -> ModuleId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.id.lib(db).into()
    }

    pub fn parent(self, db: &dyn Db) -> Option<Module> {
        match self.id.parent(db) {
            | ModuleParentId::ModuleId(id) => Some(id.into()),
            | _ => None,
        }
    }

    pub fn path(self, db: &dyn Db) -> Path {
        let mut segments = std::iter::successors(Some(self), |m| m.parent(db))
            .map(|m| m.name(db))
            .collect::<Vec<_>>();

        segments.reverse();
        Path::from(segments)
    }

    pub fn name(self, db: &dyn Db) -> Name {
        self.id.name(db)
    }

    pub fn items(self, db: &dyn Db) -> impl Iterator<Item = Item> + '_ {
        self.data(db).scope(db).items().filter_map(|it| match it {
            | ItemId::FixityId(id) => Some(Item::Fixity(id.into())),
            | ItemId::ValueId(id) => Some(Item::Value(id.into())),
            | ItemId::TypeCtorId(id) => Some(Item::TypeCtor(id.into())),
            | ItemId::TypeAliasId(id) => Some(Item::TypeAlias(id.into())),
            | ItemId::TraitId(id) => Some(Item::Trait(id.into())),
            | _ => None,
        })
    }

    pub fn impls(self, db: &dyn Db) -> impl Iterator<Item = Impl> + '_ {
        self.data(db).scope(db).impls().map(Into::into)
    }

    pub fn exports(self, db: &dyn Db) -> Vec<Item> {
        self.items(db)
            .filter(|i| i.is_exported(db))
            .chain(
                self.impls(db)
                    .flat_map(|imp| imp.items(db).into_iter().map(Item::Value)),
            )
            .collect()
    }

    pub fn is_virtual(self, _db: &dyn Db) -> bool {
        false
    }

    fn is_item_exported(self, db: &dyn Db, name: Name) -> bool {
        self.data(db).scope(db).is_exported(name)
    }

    fn data(self, db: &dyn Db) -> data::ModuleData {
        hir_def::def_map::query(db, self.id.lib(db))[self.id]
    }

    fn run_all_queries(self, db: &dyn Db) {
        for item in self.items(db) {
            item.run_all_queries(db);
        }

        for impl_ in self.impls(db) {
            impl_.run_all_queries(db);
        }
    }
}

impl Item {
    pub fn module(self, db: &dyn Db) -> Module {
        match self {
            | Self::Fixity(it) => it.module(db),
            | Self::Value(it) => it.module(db),
            | Self::TypeAlias(it) => it.module(db),
            | Self::TypeCtor(it) => it.module(db),
            | Self::Trait(it) => it.module(db),
        }
    }

    pub fn name(self, db: &dyn Db) -> Name {
        match self {
            | Self::Fixity(it) => it.name(db),
            | Self::Value(it) => it.name(db),
            | Self::TypeAlias(it) => it.name(db),
            | Self::TypeCtor(it) => it.name(db),
            | Self::Trait(it) => it.name(db),
        }
    }

    pub fn is_exported(self, db: &dyn Db) -> bool {
        self.module(db).is_item_exported(db, self.name(db))
    }

    fn run_all_queries(self, db: &dyn Db) {
        match self {
            | Self::Fixity(_) => {},
            | Self::Value(it) => it.run_all_queries(db),
            | Self::TypeCtor(it) => it.run_all_queries(db),
            | Self::TypeAlias(it) => it.run_all_queries(db),
            | Self::Trait(it) => it.run_all_queries(db),
        }
    }
}

impl Fixity {
    pub fn id(self) -> FixityId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.module(db).lib(db)
    }

    pub fn module(self, db: &dyn Db) -> Module {
        self.id.module(db).into()
    }

    pub fn path(self, db: &dyn Db) -> Path {
        let mut path = self.module(db).path(db);
        path.push(self.name(db));
        path
    }

    pub fn name(self, db: &dyn Db) -> Name {
        let it = self.id.it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);

        item_tree[it.value].name
    }

    pub fn kind(self, db: &dyn Db) -> FixityKind {
        self.data(db).kind(db)
    }

    pub fn attrs(self, db: &dyn Db) -> &Attrs {
        self.data(db).attrs(db)
    }

    fn data(self, db: &dyn Db) -> data::FixityData {
        data::fixity_data(db, self.id)
    }
}

impl Value {
    pub fn id(self) -> ValueId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.id.container(db).module(db).lib(db).into()
    }

    pub fn module(self, db: &dyn Db) -> Module {
        self.id.container(db).module(db).into()
    }

    pub fn container(self, db: &dyn Db) -> Container {
        match self.id.container(db) {
            | ContainerId::ModuleId(id) => Container::Module(id.into()),
            | ContainerId::TraitId(id) => Container::Trait(id.into()),
            | ContainerId::ImplId(id) => Container::Impl(id.into()),
        }
    }

    pub fn path(self, db: &dyn Db) -> Path {
        let mut path = self.module(db).path(db);

        if let Container::Impl(impl_) = self.container(db) {
            path.push("$impl$".as_name(db));
            path.push(impl_.id().as_id().as_u32().to_string().as_name(db));
        }

        path.push(self.name(db));
        path
    }

    pub fn name(self, db: &dyn Db) -> Name {
        let it = self.id.it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);

        item_tree[it.value].name
    }

    pub fn link_name(self, db: &dyn Db) -> String {
        if let Some(name) = self.attrs(db).by_key("link_name").string_value().next() {
            return name.to_string();
        }

        if self.is_foreign(db) || self.attrs(db).by_key("no_mangle").exists() {
            return self.name(db).display(db).to_string();
        }

        let name = self.path(db).display(db).to_string();

        if cfg!(debug_assertions) {
            name
        } else {
            mangling::mangle(name.bytes())
        }
    }

    pub fn ty(self, db: &dyn Db) -> GeneralizedType {
        hir_ty::infer(db, self.id).ty.clone()
    }

    pub fn type_vars(self, db: &dyn Db) -> Vec<TypeVar> {
        match hir_ty::infer(db, self.id).ty {
            | Generalized::Mono(_) => Vec::new(),
            | Generalized::Poly(ref vars, _) => vars.iter().map(|&v| v.into()).collect(),
        }
    }

    pub fn is_func(self, db: &dyn Db) -> bool {
        if self.has_body(db) {
            !self.body(db).params().is_empty()
        } else {
            let ty = match hir_ty::infer(db, self.id).ty {
                | Generalized::Mono(t) => t,
                | Generalized::Poly(_, t) => t,
            };

            matches!(ty.kind(db), hir_ty::ty::TyKind::Func(_))
        }
    }

    pub fn body(self, db: &dyn Db) -> Arc<Body> {
        hir_def::body::query(db, self.id).0
    }

    pub fn attrs(self, db: &dyn Db) -> &Attrs {
        self.data(db).attrs(db)
    }

    pub fn has_body(self, db: &dyn Db) -> bool {
        self.data(db).has_body(db)
    }

    pub fn is_foreign(self, db: &dyn Db) -> bool {
        self.data(db).is_foreign(db)
    }

    pub fn is_intrinsic(self, db: &dyn Db) -> bool {
        self.attrs(db).by_key("intrinsic").exists()
    }

    pub fn is_exported(self, db: &dyn Db) -> bool {
        self.module(db).is_item_exported(db, self.name(db)) || matches!(self.container(db), Container::Impl(_))
    }

    pub fn is_main(self, db: &dyn Db) -> bool {
        self.attrs(db).by_key("main").exists() || {
            let module = self.module(db);
            module.parent(db).is_none() && module.name(db).as_str(db) == "Main" && self.name(db).as_str(db) == "main"
        }
    }

    fn data(self, db: &dyn Db) -> data::ValueData {
        data::value_data(db, self.id)
    }

    fn run_all_queries(self, db: &dyn Db) {
        let item = hir_def::id::ITypedItemId::new(db, self.id.into());

        hir_def::body::query(db, self.id);
        hir_def::type_ref::query(db, item);

        use hir_def::display::HirDisplay;
        let result = hir_ty::infer(db, self.id);
        tracing::debug!("{}", self.link_name(db));
        tracing::debug!("{}", result.ty.display(db));

        if !result.constraints.is_empty() {
            tracing::debug!(
                "where {}",
                result
                    .constraints
                    .iter()
                    .map(|c| c.display(db).to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        for (e, meth) in result.methods.iter() {
            tracing::debug!("method #{:0>3} -> {:?}", u32::from(e.into_raw()), meth);
        }

        for (e, inst) in result.instances.iter() {
            tracing::debug!("instance #{:0>3} -> {:?}", u32::from(e.into_raw()), inst);
        }

        for (p, ty) in result.type_of_pat.iter() {
            tracing::debug!("${:0>2} :: {}", u32::from(p.into_raw()), ty.display(db));
        }

        for (e, ty) in result.type_of_expr.iter() {
            tracing::debug!("#{:0>3} :: {}", u32::from(e.into_raw()), ty.display(db));
        }
    }
}

impl Ctor {
    pub fn id(self) -> CtorId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.module(db).lib(db)
    }

    pub fn module(self, db: &dyn Db) -> Module {
        self.type_ctor(db).module(db)
    }

    pub fn path(self, db: &dyn Db) -> Path {
        let mut path = self.module(db).path(db);
        path.push(self.name(db));
        path
    }

    pub fn name(self, db: &dyn Db) -> Name {
        let it = self.id.type_ctor(db).it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);

        item_tree[self.id.local_id(db)].name
    }

    pub fn link_name(self, db: &dyn Db) -> String {
        let name = self.path(db).display(db).to_string();

        if cfg!(debug_assertions) {
            name
        } else {
            mangling::mangle(name.bytes())
        }
    }

    pub fn type_ctor(self, db: &dyn Db) -> TypeCtor {
        self.id.type_ctor(db).into()
    }

    pub fn ty(self, db: &dyn Db) -> GeneralizedType {
        hir_ty::ctor_ty(db, self.id)
    }

    pub fn ret(self, db: &dyn Db) -> Ty {
        let ty = match self.ty(db) {
            | Generalized::Mono(ty) => ty,
            | Generalized::Poly(_, ty) => ty,
        };

        match ty.kind(db) {
            | hir_ty::ty::TyKind::Func(func) => func.ret,
            | _ => ty,
        }
    }

    pub fn types(self, db: &dyn Db) -> &[Ty] {
        let ty = match self.ty(db) {
            | Generalized::Mono(ty) => ty,
            | Generalized::Poly(_, ty) => ty,
        };

        match ty.kind(db) {
            | hir_ty::ty::TyKind::Func(func) => &*func.params,
            | _ => &[],
        }
    }

    pub fn attrs(self, db: &dyn Db) -> &Attrs {
        self.data(db).attrs(db)
    }

    pub fn is_exported(self, db: &dyn Db) -> bool {
        self.module(db).is_item_exported(db, self.name(db))
    }

    fn data(self, db: &dyn Db) -> data::CtorData {
        data::ctor_data(db, self.id)
    }
}

impl Field {
    pub fn id(self) -> FieldId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.module(db).lib(db)
    }

    pub fn module(self, db: &dyn Db) -> Module {
        self.type_ctor(db).module(db)
    }

    pub fn path(self, db: &dyn Db) -> Path {
        let mut path = self.module(db).path(db);
        path.push(self.name(db));
        path
    }

    pub fn name(self, db: &dyn Db) -> Name {
        let it = self.id.ctor(db).type_ctor(db).it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);

        item_tree[self.id.local_id(db)].name
    }

    pub fn link_name(self, db: &dyn Db) -> String {
        let name = self.path(db).display(db).to_string();

        if cfg!(debug_assertions) {
            name
        } else {
            mangling::mangle(name.bytes())
        }
    }

    pub fn type_ctor(self, db: &dyn Db) -> TypeCtor {
        self.id.ctor(db).type_ctor(db).into()
    }

    pub fn ctor(self, db: &dyn Db) -> Ctor {
        self.id.ctor(db).into()
    }

    pub fn ty(self, db: &dyn Db) -> GeneralizedType {
        hir_ty::field_ty(db, self.id)
    }

    pub fn attrs(self, db: &dyn Db) -> &Attrs {
        self.data(db).attrs(db)
    }

    pub fn is_exported(self, db: &dyn Db) -> bool {
        self.module(db).is_item_exported(db, self.name(db))
    }

    fn data(self, db: &dyn Db) -> data::FieldData {
        data::field_data(db, self.id)
    }
}

impl TypeCtor {
    pub fn id(self) -> TypeCtorId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.module(db).lib(db)
    }

    pub fn module(self, db: &dyn Db) -> Module {
        self.id.module(db).into()
    }

    pub fn path(self, db: &dyn Db) -> Path {
        let mut path = self.module(db).path(db);
        path.push(self.name(db));
        path
    }

    pub fn name(self, db: &dyn Db) -> Name {
        let it = self.id.it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);

        item_tree[it.value].name
    }

    pub fn type_vars(self, db: &dyn Db) -> Vec<TypeVar> {
        self.data(db).type_vars(db).iter().map(|&v| v.into()).collect()
    }

    pub fn kind(self, db: &dyn Db) -> Ty {
        hir_ty::type_ctor_ty(db, self.id).kind
    }

    pub fn ctors(self, db: &dyn Db) -> Vec<Ctor> {
        let it = self.id.it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);

        item_tree[it.value]
            .ctors
            .iter()
            .map(|&id| CtorId::new(db, self.id, id).into())
            .collect()
    }

    pub fn is_boxed(self, db: &dyn Db) -> bool {
        self.attrs(db).by_key("boxed").exists()
    }

    pub fn attrs(self, db: &dyn Db) -> &Attrs {
        self.data(db).attrs(db)
    }

    fn data(self, db: &dyn Db) -> data::TypeCtorData {
        data::type_ctor_data(db, self.id)
    }

    fn run_all_queries(self, db: &dyn Db) {
        hir_ty::type_ctor_ty(db, self.id);
    }
}

impl TypeAlias {
    pub fn id(self) -> TypeAliasId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.module(db).lib(db)
    }

    pub fn module(self, db: &dyn Db) -> Module {
        self.id.module(db).into()
    }

    pub fn path(self, db: &dyn Db) -> Path {
        let mut path = self.module(db).path(db);
        path.push(self.name(db));
        path
    }

    pub fn name(self, db: &dyn Db) -> Name {
        let it = self.id.it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);

        item_tree[it.value].name
    }

    pub fn type_vars(self, db: &dyn Db) -> Vec<TypeVar> {
        self.data(db).type_vars(db).iter().map(|&v| v.into()).collect()
    }

    pub fn ty(self, db: &dyn Db) -> GeneralizedType {
        hir_ty::alias_ty(db, self.id)
    }

    pub fn attrs(self, db: &dyn Db) -> &Attrs {
        self.data(db).attrs(db)
    }

    fn data(self, db: &dyn Db) -> data::TypeAliasData {
        data::type_alias_data(db, self.id)
    }

    fn run_all_queries(self, db: &dyn Db) {
        hir_ty::alias_ty(db, self.id);
    }
}

impl Trait {
    pub fn id(self) -> TraitId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.module(db).lib(db)
    }

    pub fn module(self, db: &dyn Db) -> Module {
        self.id.module(db).into()
    }

    pub fn path(self, db: &dyn Db) -> Path {
        let mut path = self.module(db).path(db);
        path.push(self.name(db));
        path
    }

    pub fn name(self, db: &dyn Db) -> Name {
        let it = self.id.it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);

        item_tree[it.value].name
    }

    pub fn constraints(self, db: &dyn Db) -> &[Constraint] {
        &hir_ty::trait_types(db, self.id).1
    }

    pub fn type_vars(self, db: &dyn Db) -> Vec<TypeVar> {
        self.data(db).type_vars(db).iter().map(|&v| v.into()).collect()
    }

    pub fn items(self, db: &dyn Db) -> Vec<Value> {
        self.data(db).items(db).values().map(|&id| id.into()).collect()
    }

    pub fn attrs(self, db: &dyn Db) -> &Attrs {
        self.data(db).attrs(db)
    }

    pub fn impls(self, db: &dyn Db, in_lib: Lib) -> Vec<Impl> {
        hir_ty::traits::trait_impls(db, in_lib.id, self.id)
            .iter()
            .map(|&id| id.into())
            .collect()
    }

    fn data(self, db: &dyn Db) -> data::TraitData {
        data::trait_data(db, self.id)
    }

    fn run_all_queries(self, db: &dyn Db) {
        hir_ty::trait_types(db, self.id);

        for item in self.items(db) {
            item.run_all_queries(db);
        }
    }
}

impl Impl {
    pub fn id(self) -> ImplId {
        self.id
    }

    pub fn lib(self, db: &dyn Db) -> Lib {
        self.module(db).lib(db)
    }

    pub fn module(self, db: &dyn Db) -> Module {
        self.id.module(db).into()
    }

    pub fn trait_(self, db: &dyn Db) -> Option<Trait> {
        self.data(db).trait_id(db).map(Into::into)
    }

    pub fn vtable_link_name(self, db: &dyn Db) -> String {
        let name = match self.trait_(db) {
            | Some(trait_) => format!("{}.$impl$.{}", trait_.path(db).display(db), self.id.as_id().as_u32()),
            | None => format!(
                "{}.$impl$.{}",
                self.module(db).path(db).display(db),
                self.id.as_id().as_u32(),
            ),
        };

        if cfg!(debug_assertions) {
            name
        } else {
            mangling::mangle(name.bytes())
        }
    }

    pub fn types(self, db: &dyn Db) -> &[Ty] {
        &hir_ty::impl_types(db, self.id).0
    }

    pub fn constraints(self, db: &dyn Db) -> &[Constraint] {
        &hir_ty::impl_types(db, self.id).1
    }

    pub fn type_vars(self, db: &dyn Db) -> Vec<TypeVar> {
        self.data(db).type_vars(db).iter().map(|&v| v.into()).collect()
    }

    pub fn items(self, db: &dyn Db) -> Vec<Value> {
        self.data(db).items(db).values().map(|&id| id.into()).collect()
    }

    pub fn attrs(self, db: &dyn Db) -> &Attrs {
        self.data(db).attrs(db)
    }

    pub fn bind_vars(self, db: &dyn Db, types: &[Ty]) -> NoHashHashMap<TypeVarId, Ty> {
        let mut ctx = hir_ty::ctx::Ctx::new(db, self.id.into());
        let mut bindings = hir_ty::unify::UnifyBindings::default();
        let mut map = NoHashHashMap::default();

        for var in self.type_vars(db) {
            let ty = ctx.fresh_type(Default::default(), false);
            map.insert(var.id(), ty);
        }

        let impl_types = self
            .types(db)
            .iter()
            .map(|t| t.replace_vars(db, &map))
            .collect::<Vec<_>>();

        assert_eq!(types.len(), impl_types.len());
        let res = ctx.unify_all(types.iter(), impl_types.iter(), &mut bindings);
        assert_eq!(res, hir_ty::unify::UnifyResult::Ok);

        for ty in map.values_mut() {
            *ty = bindings.resolve_type_fully(db, *ty);
        }

        map
    }

    fn data(self, db: &dyn Db) -> data::ImplData {
        data::impl_data(db, self.id)
    }

    fn run_all_queries(self, db: &dyn Db) {
        hir_ty::impl_types(db, self.id);

        for item in self.items(db) {
            item.run_all_queries(db);
        }
    }
}

impl TypeVar {
    pub fn id(self) -> TypeVarId {
        self.id
    }

    pub fn name(&self, db: &dyn Db) -> Name {
        let owner = self.id.owner(db);
        let type_map = owner.type_map(db).0;

        match self.id.local_id(db) {
            | either::Either::Left(local_id) => type_map[local_id].name,
            | either::Either::Right(name) => name,
        }
    }
}

impl From<LibId> for Lib {
    fn from(id: LibId) -> Self {
        Self { id }
    }
}

impl From<ModuleId> for Module {
    fn from(id: ModuleId) -> Self {
        Self { id }
    }
}

impl From<FixityId> for Fixity {
    fn from(id: FixityId) -> Self {
        Self { id }
    }
}

impl From<ValueId> for Value {
    fn from(id: ValueId) -> Self {
        Self { id }
    }
}

impl From<CtorId> for Ctor {
    fn from(id: CtorId) -> Self {
        Self { id }
    }
}

impl From<TypeCtorId> for TypeCtor {
    fn from(id: TypeCtorId) -> Self {
        Self { id }
    }
}

impl From<TypeAliasId> for TypeAlias {
    fn from(id: TypeAliasId) -> Self {
        Self { id }
    }
}

impl From<TraitId> for Trait {
    fn from(id: TraitId) -> Self {
        Self { id }
    }
}

impl From<ImplId> for Impl {
    fn from(id: ImplId) -> Self {
        Self { id }
    }
}

impl From<TypeVarId> for TypeVar {
    fn from(id: TypeVarId) -> Self {
        Self { id }
    }
}
