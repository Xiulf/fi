use syntax::ast::NameOwner;
use triomphe::Arc;
use vfs::File;

use super::*;
use crate::ast_id::AstIdMap;
use crate::id::{LocalCtorId, LocalFieldId};
use crate::name::AsName;
use crate::path::Path;
use crate::Db;

#[salsa::tracked]
pub fn query(db: &dyn Db, file: File) -> Arc<ItemTree> {
    let mut ctx = Ctx {
        db,
        tree: ItemTree {
            file,
            items: Vec::new(),
            data: ItemTreeData::default(),
            attrs: FxHashMap::default(),
        },
        ast_map: crate::ast_id::query(db, file),
    };

    let source_file = base_db::parse(db, file);

    // use syntax::ast::AstNode;
    // let resolver = db.syntax_interner().read();
    // tracing::debug!("{}", source_file.syntax().debug(&*resolver, true));

    ctx.lower_source_file(source_file);

    Arc::new(ctx.tree)
}

fn id<N: ItemTreeNode>(index: Idx<N>) -> LocalItemTreeId<N> {
    LocalItemTreeId {
        index,
        _marker: PhantomData,
    }
}

struct Ctx<'a> {
    db: &'a dyn Db,
    tree: ItemTree,
    ast_map: Arc<AstIdMap>,
}

impl Ctx<'_> {
    fn lower_source_file(&mut self, source_file: ast::SourceFile) {
        self.tree.items = source_file
            .module()
            .map(|m| self.lower_items(m.items()))
            .unwrap_or_default();
    }

    fn lower_items(&mut self, items: impl Iterator<Item = ast::Item>) -> Vec<Item> {
        items.flat_map(|item| self.lower_item(item)).flatten().collect()
    }

    fn lower_item(&mut self, item: ast::Item) -> Option<Vec<Item>> {
        let attrs = RawAttrs::new(self.db, &item);
        let items = match item {
            | ast::Item::Module(it) => self.lower_module(it),
            | ast::Item::Import(it) => self.lower_import(it),
            | ast::Item::Fixity(it) => self.lower_fixity(it),
            | ast::Item::Value(it) => self.lower_value(it),
            | ast::Item::Type(it) => self.lower_type(it),
            | ast::Item::Trait(it) => self.lower_trait(it),
            | ast::Item::Impl(it) => self.lower_impl(it),
        };

        if !attrs.is_empty() {
            for item in items.iter().flatten() {
                self.tree.attrs.insert((*item).into(), attrs.clone());
            }
        }

        items
    }

    fn lower_module(&mut self, module: ast::ItemModule) -> Option<Vec<Item>> {
        let ast_id = self.ast_map.ast_id(&module);
        let name = Path::from_ast(self.db, module.name()?).as_name()?;
        let items = self.lower_items(module.items()).into_boxed_slice();
        let data = Module { ast_id, name, items };

        Some(vec![id(self.tree.data.modules.alloc(data)).into()])
    }

    fn lower_import(&mut self, import: ast::ItemImport) -> Option<Vec<Item>> {
        let ast_id = self.ast_map.ast_id(&import);
        let mut imports = Vec::with_capacity(1);

        expand_import(
            self.db,
            InFile::new(self.tree.file, import),
            |path, _, all, rename, qualify, hiding| {
                imports.push(
                    id(self.tree.data.imports.alloc(Import {
                        index: imports.len(),
                        ast_id,
                        path,
                        rename,
                        qualify,
                        hiding,
                        all,
                    }))
                    .into(),
                );
            },
        );

        Some(imports)
    }

    fn lower_fixity(&mut self, fixity: ast::ItemFixity) -> Option<Vec<Item>> {
        let resolver = self.db.syntax_interner().read();
        let ast_id = self.ast_map.ast_id(&fixity);
        let name = fixity.name()?.as_name(self.db);
        let value = Path::from_ast(self.db, fixity.value()?);
        let is_type = fixity.is_type();
        let kind = if fixity.is_prefix() {
            FixityKind::Prefix
        } else if fixity.is_postfix() {
            FixityKind::Postfix
        } else {
            FixityKind::Infix(fixity.assoc()?, fixity.prec(&*resolver)?)
        };

        let data = Fixity {
            ast_id,
            name,
            value,
            is_type,
            kind,
        };

        Some(vec![id(self.tree.data.fixities.alloc(data)).into()])
    }

    fn lower_value(&mut self, value: ast::ItemValue) -> Option<Vec<Item>> {
        let ast_id = self.ast_map.ast_id(&value);
        let name = value.name()?.as_name(self.db);
        let is_foreign = value.foreign_token().is_some();
        let has_body = value.body().is_some();
        let data = Value {
            ast_id,
            name,
            is_foreign,
            has_body,
        };

        Some(vec![id(self.tree.data.values.alloc(data)).into()])
    }

    fn lower_type(&mut self, typ: ast::ItemType) -> Option<Vec<Item>> {
        let ast_id = self.ast_map.ast_id(&typ);
        let name = typ.name()?.as_name(self.db);
        let ctors = typ.ctors().filter_map(|c| self.lower_ctor(c)).collect();
        let data = TypeCtor { ast_id, name, ctors };

        Some(vec![id(self.tree.data.type_ctors.alloc(data)).into()])
    }

    fn lower_ctor(&mut self, ctor: ast::Ctor) -> Option<LocalCtorId> {
        let ast_id = self.ast_map.ast_id(&ctor);
        let name = ctor.name()?.as_name(self.db);
        let fields = ctor
            .record()
            .map(|record| record.fields().filter_map(|f| self.lower_field(f)).collect());
        let data = Ctor { ast_id, name, fields };

        Some(self.tree.data.ctors.alloc(data))
    }

    fn lower_field(&mut self, field: ast::CtorField) -> Option<LocalFieldId> {
        let ast_id = self.ast_map.ast_id(&field);
        let name = field.name()?.as_name(self.db);
        let data = Field { ast_id, name };

        Some(self.tree.data.fields.alloc(data))
    }

    fn lower_trait(&mut self, trait_: ast::ItemTrait) -> Option<Vec<Item>> {
        let ast_id = self.ast_map.ast_id(&trait_);
        let name = trait_.name()?.as_name(self.db);
        let items = trait_
            .items()
            .filter_map(|it| self.lower_value(it))
            .flatten()
            .map(|it| match it {
                | Item::Value(it) => it,
                | _ => unreachable!(),
            })
            .collect();

        let data = Trait { ast_id, name, items };

        Some(vec![id(self.tree.data.traits.alloc(data)).into()])
    }

    fn lower_impl(&mut self, impl_: ast::ItemImpl) -> Option<Vec<Item>> {
        let ast_id = self.ast_map.ast_id(&impl_);
        let trait_ = Path::from_ast(self.db, impl_.trait_()?);
        let items = impl_
            .items()
            .filter_map(|it| self.lower_value(it))
            .flatten()
            .map(|it| match it {
                | Item::Value(it) => it,
                | _ => unreachable!(),
            })
            .collect();

        let data = Impl { ast_id, trait_, items };

        Some(vec![id(self.tree.data.impls.alloc(data)).into()])
    }
}

pub fn expand_import(
    db: &dyn Db,
    import: InFile<ast::ItemImport>,
    mut cb: impl FnMut(Path, Option<ast::ImportItem>, bool, Option<Name>, Option<Name>, Option<Box<[Name]>>),
) {
    if let Some(path) = import.value.module().map(|p| Path::from_ast(db, p)) {
        let qualify = import.value.rename().map(|n| n.as_name(db));

        if let Some(items) = import.value.items() {
            for item in items.iter() {
                if let Some(name) = item.name_ref() {
                    let rename = item.rename().map(|n| n.as_name(db));
                    let mut path = path.clone();
                    path.push(name.as_name(db));
                    cb(path, Some(item), false, rename, qualify, None);
                }
            }
        } else {
            let hiding = import.value.hiding().map(|h| h.iter().map(|n| n.as_name(db)).collect());

            cb(path, None, true, None, qualify, hiding);
        }
    }
}
