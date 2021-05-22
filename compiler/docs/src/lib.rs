pub mod db;
pub mod elems;

use db::DocDatabase;
use elems::*;
use hir::display::HirDisplay;
use relative_path::{RelativePath, RelativePathBuf};
use rustc_hash::FxHashMap;
use std::path::Path;
use std::sync::Arc;
use std::{fs, io};

const STYLESHEET: &'static str = include_str!("style.css");

pub fn generate(db: &dyn DocDatabase, lib: hir::Lib, target_dir: &Path) -> io::Result<()> {
    let cwd = std::env::current_dir().unwrap();
    let target_dir = target_dir.join("docs");
    let target_dir = target_dir.strip_prefix(cwd).unwrap_or(&target_dir);
    let target_dir = RelativePathBuf::from_path(target_dir).unwrap();
    let mut gen = Generator::new(db);

    gen.lib_docs(lib, &&target_dir);

    let style_dir = target_dir.join("style.css");

    fs::write(style_dir.to_path("."), STYLESHEET)
}

struct Generator<'a> {
    db: &'a dyn DocDatabase,
    hdb: &'a dyn hir::db::HirDatabase,
    modules: FxHashMap<hir::Module, PageId>,
    fixities: FxHashMap<hir::Fixity, PageId>,
    funcs: FxHashMap<hir::Func, PageId>,
    statics: FxHashMap<hir::Static, PageId>,
    consts: FxHashMap<hir::Const, PageId>,
    type_aliases: FxHashMap<hir::TypeAlias, PageId>,
    type_ctors: FxHashMap<hir::TypeCtor, PageId>,
    classes: FxHashMap<hir::Class, PageId>,
}

impl<'a> Generator<'a> {
    fn new(db: &'a dyn DocDatabase) -> Self {
        Generator {
            db,
            hdb: db.upcast(),
            modules: FxHashMap::default(),
            fixities: FxHashMap::default(),
            funcs: FxHashMap::default(),
            statics: FxHashMap::default(),
            consts: FxHashMap::default(),
            type_aliases: FxHashMap::default(),
            type_ctors: FxHashMap::default(),
            classes: FxHashMap::default(),
        }
    }

    fn lib_docs(&mut self, lib: hir::Lib, path: &RelativePath) -> io::Result<()> {
        let path = path.join(lib.name(self.hdb).to_string());

        for module in lib.modules(self.hdb) {
            self.module_docs(module);
        }

        for (m, &id) in &self.modules {
            if lib == m.lib().into() {
                let page = self.db.lookup_intern_page(id);

                page.render(&path, self.db)?;
            }
        }

        for (m, &id) in &self.fixities {
            if lib == m.lib(self.hdb).into() {
                let page = self.db.lookup_intern_page(id);

                page.render(&path, self.db)?;
            }
        }

        for (m, &id) in &self.funcs {
            if lib == m.lib(self.hdb).into() {
                let page = self.db.lookup_intern_page(id);

                page.render(&path, self.db)?;
            }
        }

        for (m, &id) in &self.statics {
            if lib == m.lib(self.hdb).into() {
                let page = self.db.lookup_intern_page(id);

                page.render(&path, self.db)?;
            }
        }

        for (m, &id) in &self.consts {
            if lib == m.lib(self.hdb).into() {
                let page = self.db.lookup_intern_page(id);

                page.render(&path, self.db)?;
            }
        }

        for (m, &id) in &self.type_aliases {
            if lib == m.lib(self.hdb).into() {
                let page = self.db.lookup_intern_page(id);

                page.render(&path, self.db)?;
            }
        }

        for (m, &id) in &self.type_ctors {
            if lib == m.lib(self.hdb).into() {
                let page = self.db.lookup_intern_page(id);

                page.render(&path, self.db)?;
            }
        }

        for (m, &id) in &self.classes {
            if lib == m.lib(self.hdb).into() {
                let page = self.db.lookup_intern_page(id);

                page.render(&path, self.db)?;
            }
        }

        Ok(())
    }

    fn module_docs(&mut self, module: hir::Module) -> PageId {
        if let Some(id) = self.modules.get(&module) {
            *id
        } else {
            let name = module.name(self.hdb);
            let mut page = Page::new(module.path(self.hdb), format!("Module {}", name));
            let mut sec = Section::new("Definitions");
            let mut entry = Entry::new();
            let mut modules = Vec::new();
            let mut fixities = Vec::new();
            let mut funcs = Vec::new();
            let mut statics = Vec::new();
            let mut consts = Vec::new();
            let mut type_aliases = Vec::new();
            let mut type_ctors = Vec::new();
            let mut classes = Vec::new();

            for def in module.declarations(self.hdb) {
                match def {
                    | hir::ModuleDef::Module(it) if !it.is_virtual(self.hdb) => {
                        let id = self.module_docs(it);

                        modules.push(Link::new(it.name(self.hdb).to_string(), id));
                    },
                    | hir::ModuleDef::Fixity(it) => {
                        let id = self.fixity_docs(it);

                        fixities.push(Link::new(it.name(self.hdb).to_string(), id));
                    },
                    | hir::ModuleDef::Func(it) => {
                        let id = self.func_docs(it);

                        funcs.push(Link::new(it.name(self.hdb).to_string(), id));
                    },
                    | hir::ModuleDef::Static(it) => {
                        let id = self.static_docs(it);

                        statics.push(Link::new(it.name(self.hdb).to_string(), id));
                    },
                    | hir::ModuleDef::Const(it) => {
                        let id = self.const_docs(it);

                        consts.push(Link::new(it.name(self.hdb).to_string(), id));
                    },
                    | hir::ModuleDef::TypeAlias(it) => {
                        let id = self.type_alias_docs(it);

                        type_aliases.push(Link::new(it.name(self.hdb).to_string(), id));
                    },
                    | hir::ModuleDef::TypeCtor(it) => {
                        let id = self.type_ctor_docs(it);

                        type_ctors.push(Link::new(it.name(self.hdb).to_string(), id));
                    },
                    | hir::ModuleDef::Class(it) => {
                        let id = self.class_docs(it);

                        classes.push(Link::new(it.name(self.hdb).to_string(), id));
                    },
                    | _ => {},
                }
            }

            if !modules.is_empty() {
                entry.title("Modules");
                entry.list().append(&mut modules);
            }

            if !fixities.is_empty() {
                entry.title("Infix Operators");
                entry.list().append(&mut fixities);
            }

            if !funcs.is_empty() {
                entry.title("Functions");
                entry.list().append(&mut funcs);
            }

            if !statics.is_empty() {
                entry.title("Statics");
                entry.list().append(&mut statics);
            }

            if !consts.is_empty() {
                entry.title("Constants");
                entry.list().append(&mut consts);
            }

            if !type_aliases.is_empty() {
                entry.title("Type Aliases");
                entry.list().append(&mut type_aliases);
            }

            if !type_ctors.is_empty() {
                entry.title("Types");
                entry.list().append(&mut type_ctors);
            }

            if !classes.is_empty() {
                entry.title("Type Classes");
                entry.list().append(&mut classes);
            }

            sec.entries.push(entry);
            page.sections.push(sec);

            let id = self.db.intern_page(Arc::new(page));

            self.modules.insert(module, id);
            id
        }
    }

    fn fixity_docs(&mut self, fixity: hir::Fixity) -> PageId {
        if let Some(id) = self.fixities.get(&fixity) {
            *id
        } else {
            let name = fixity.name(self.hdb);
            let mut page = Page::new(fixity.path(self.hdb), format!("Infix Operator {}", name));
            let mut sec = Section::new("Infix Operator");
            let mut entry = Entry::new();

            entry.title(name.to_string());

            let def = entry.code();

            def.keyword(match fixity.assoc(self.hdb) {
                | hir::Assoc::Left => "infixl",
                | hir::Assoc::Right => "infixr",
                | hir::Assoc::None => "infix",
            });

            def.constant(match fixity.prec(self.hdb) {
                | hir::Prec::Zero => '0',
                | hir::Prec::One => '1',
                | hir::Prec::Two => '2',
                | hir::Prec::Three => '3',
                | hir::Prec::Four => '4',
                | hir::Prec::Five => '5',
                | hir::Prec::Six => '6',
                | hir::Prec::Seven => '7',
                | hir::Prec::Eight => '8',
                | hir::Prec::Nine => '9',
            });

            def.func(fixity.func(self.hdb).name(self.hdb).to_string());
            def.keyword("as");
            def.symbol(name.to_string());

            sec.entries.push(entry);
            page.sections.push(sec);

            let id = self.db.intern_page(Arc::new(page));

            self.fixities.insert(fixity, id);
            id
        }
    }

    fn func_docs(&mut self, func: hir::Func) -> PageId {
        if let Some(id) = self.funcs.get(&func) {
            *id
        } else {
            let name = func.name(self.hdb);
            let mut page = Page::new(func.path(self.hdb), format!("Function {}", name));
            let mut sec = Section::new("Function");
            let mut entry = Entry::new();
            let ty = func.ty(self.hdb);

            entry.title(name.to_string());

            let def = entry.code();

            def.keyword("fun");
            def.func(name.to_string());
            def.text("::");
            def.type_(ty.display(self.hdb).to_string());

            sec.entries.push(entry);
            page.sections.push(sec);

            let id = self.db.intern_page(Arc::new(page));

            self.funcs.insert(func, id);
            id
        }
    }

    fn static_docs(&mut self, static_: hir::Static) -> PageId {
        if let Some(id) = self.statics.get(&static_) {
            *id
        } else {
            let name = static_.name(self.hdb);
            let mut page = Page::new(static_.path(self.hdb), format!("Static {}", name));
            let mut sec = Section::new("Static");
            let mut entry = Entry::new();

            sec.entries.push(entry);
            page.sections.push(sec);

            let id = self.db.intern_page(Arc::new(page));

            self.statics.insert(static_, id);
            id
        }
    }

    fn const_docs(&mut self, const_: hir::Const) -> PageId {
        if let Some(id) = self.consts.get(&const_) {
            *id
        } else {
            let name = const_.name(self.hdb);
            let mut page = Page::new(const_.path(self.hdb), format!("Const {}", name));
            let mut sec = Section::new("Constant");
            let mut entry = Entry::new();

            sec.entries.push(entry);
            page.sections.push(sec);

            let id = self.db.intern_page(Arc::new(page));

            self.consts.insert(const_, id);
            id
        }
    }

    fn type_alias_docs(&mut self, type_alias: hir::TypeAlias) -> PageId {
        if let Some(id) = self.type_aliases.get(&type_alias) {
            *id
        } else {
            let name = type_alias.name(self.hdb);
            let mut page = Page::new(type_alias.path(self.hdb), format!("Type Alias {}", name));
            let mut sec = Section::new("Type Alias");
            let mut entry = Entry::new();

            sec.entries.push(entry);
            page.sections.push(sec);

            let id = self.db.intern_page(Arc::new(page));

            self.type_aliases.insert(type_alias, id);
            id
        }
    }

    fn type_ctor_docs(&mut self, type_ctor: hir::TypeCtor) -> PageId {
        if let Some(id) = self.type_ctors.get(&type_ctor) {
            *id
        } else {
            let name = type_ctor.name(self.hdb);
            let mut page = Page::new(type_ctor.path(self.hdb), format!("Type {}", name));
            let mut sec = Section::new("Type");
            let mut entry = Entry::new();

            sec.entries.push(entry);
            page.sections.push(sec);

            let id = self.db.intern_page(Arc::new(page));

            self.type_ctors.insert(type_ctor, id);
            id
        }
    }

    fn class_docs(&mut self, class: hir::Class) -> PageId {
        if let Some(id) = self.classes.get(&class) {
            *id
        } else {
            let name = class.name(self.hdb);
            let mut page = Page::new(class.path(self.hdb), format!("Type Class {}", name));
            let mut sec = Section::new("Type Class");
            let mut entry = Entry::new();

            sec.entries.push(entry);
            page.sections.push(sec);

            let id = self.db.intern_page(Arc::new(page));

            self.classes.insert(class, id);
            id
        }
    }
}
