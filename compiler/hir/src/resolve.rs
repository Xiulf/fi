use crate::{Id, Symbol};
use diagnostics::{Diagnostic, Reporter, Severity, Span};
use std::collections::HashMap;

#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct Resolver<'a> {
    #[derivative(Debug = "ignore")]
    reporter: &'a Reporter,
    pub(crate) current_module: ModuleId,
    pub(crate) modules: HashMap<ModuleId, Module>,
}

#[derive(Debug)]
pub enum Module {
    Normal(ModuleInfo),
    Virtual(VirtualModule),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleId {
    Normal(Symbol),
    Virtual(usize),
}

#[derive(Debug)]
pub struct ModuleInfo {
    pub module: crate::Module,
    pub scopes: PerNs<Vec<Rib>>,
    pub imports: HashMap<Symbol, ModuleId>,
    pub exports: Vec<Export>,
    external: bool,
}

#[derive(Debug)]
pub struct VirtualModule {
    pub name: Symbol,
    pub exports: HashMap<Symbol, Export>,
}

#[derive(Debug, Clone, Copy)]
pub enum Export {
    Value(ModuleId, Id),
    Type(ModuleId, Id),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ModuleMeta {
    pub name: Symbol,
    pub items: HashMap<Symbol, (Id, bool)>,
}

#[derive(Debug, Clone)]
pub struct PerNs<T> {
    modules: T,
    values: T,
    types: T,
    labels: T,
}

#[derive(Debug, Clone, Copy)]
pub enum Ns {
    Modules,
    Values,
    Types,
    Labels,
}

#[derive(Debug, Clone)]
pub struct Rib {
    names: HashMap<Symbol, Res>,
    kind: RibKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RibKind {
    Global,
    Block,
    Local,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum Res {
    Module(ModuleId),
    Item(Id),
    Local(Id),
    Label(Id),
    PrimVal(PrimVal),
    PrimTy(PrimTy),
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum PrimVal {
    True,
    False,
    Undefined,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum PrimTy {
    Never,
    Bool,
    Str,
    Int(u8, bool),
    Float(u8),
}

impl<'a> Resolver<'a> {
    pub fn new<'b>(
        reporter: &'a Reporter,
        deps: impl Iterator<Item = &'b std::path::Path>,
    ) -> Self {
        let mut resolver = Resolver {
            reporter,
            current_module: ModuleId::Virtual(0),
            modules: HashMap::new(),
        };

        resolver.add_root();

        for dep in deps {
            let meta = ModuleMeta::load(dep);

            resolver.add_meta(meta);
        }

        resolver
    }

    fn add_meta(&mut self, meta: ModuleMeta) {
        self.add_module(meta.name, true);
        self.set_module(ModuleId::Normal(meta.name));

        for (name, (id, is_type)) in meta.items {
            self.define(
                if is_type { Ns::Types } else { Ns::Values },
                name,
                Span::default(),
                Res::Item(id),
            );

            self.module().info_mut().exports.push(if is_type {
                Export::Type(ModuleId::Normal(meta.name), id)
            } else {
                Export::Value(ModuleId::Normal(meta.name), id)
            });
        }
    }

    pub fn add_module(&mut self, name: Symbol, external: bool) {
        self.modules.insert(
            ModuleId::Normal(name),
            Module::Normal(ModuleInfo {
                module: crate::Module {
                    name,
                    items: Vec::new(),
                },
                scopes: PerNs {
                    modules: vec![Rib::new(RibKind::Global)],
                    values: vec![Rib::new(RibKind::Global)],
                    types: vec![Rib::new(RibKind::Global)],
                    labels: Vec::new(),
                },
                imports: HashMap::new(),
                exports: Vec::new(),
                external,
            }),
        );
    }

    pub fn set_module(&mut self, id: ModuleId) {
        self.current_module = id;
    }

    fn module(&mut self) -> &mut Module {
        self.modules
            .get_mut(&self.current_module)
            .expect("undefined module")
    }

    pub fn define(&mut self, ns: Ns, name: Symbol, span: Span, res: Res) {
        if self.check_duplicate(ns, &name, span) {
            self.module().info_mut().scopes[ns]
                .last_mut()
                .unwrap()
                .insert(name, res);
        }
    }

    fn check_duplicate(&self, ns: Ns, name: &Symbol, span: Span) -> bool {
        let last_rib = self.modules[&self.current_module].info().scopes[ns]
            .last()
            .unwrap();

        if last_rib.contains(name) {
            self.reporter.add(
                Diagnostic::new(
                    Severity::Error,
                    0003,
                    format!("duplicate declaration '{}'", name),
                )
                .label(Severity::Error, span, None::<String>),
            );

            false
        } else {
            true
        }
    }

    pub fn push_rib(&mut self, ns: Ns, kind: RibKind) {
        self.module().info_mut().scopes[ns].push(Rib::new(kind));
    }

    pub fn pop_rib(&mut self, ns: Ns) {
        self.module().info_mut().scopes[ns].pop().unwrap();
    }

    pub fn get(&self, ns: Ns, module: Option<ModuleId>, name: &Symbol) -> (Option<Res>, bool) {
        match self.get_inner(ns, module.unwrap_or(self.current_module), name) {
            (Some(res), l) => (Some(res), l),
            (None, _) => self.get_inner(ns, ModuleId::Virtual(0), name),
        }
    }

    pub fn get_virt(&self, ns: Ns, module: ModuleId, name: &Symbol) -> Option<Res> {
        let module = self.modules[&module].virt();

        module
            .exports
            .iter()
            .filter(|(_, e)| match (e, ns) {
                (Export::Value(..), Ns::Values) => true,
                (Export::Type(..), Ns::Types) => true,
                _ => false,
            })
            .find(|(n, _)| *n == name)
            .map(|(_, e)| Res::Item(*e.id()))
    }

    fn get_inner(&self, ns: Ns, module: ModuleId, name: &Symbol) -> (Option<Res>, bool) {
        let ribs = &self.modules[&module].info().scopes[ns];
        let mut is_local = false;
        let mut res = None;

        for rib in ribs {
            if let Some(r) = rib.get(name) {
                res = Some(r);
                is_local = rib.kind == RibKind::Local;
            }
        }

        (res, is_local)
    }

    pub fn all_module_meta(&self) -> Vec<ModuleMeta> {
        self.modules
            .iter()
            .filter_map(|(id, m)| {
                if let Module::Normal(info) = m {
                    if !info.external {
                        Some(self.module_meta(id))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn module_meta(&self, module: &ModuleId) -> ModuleMeta {
        let modules = &self.modules[module].info().scopes[Ns::Modules][0];
        let values = &self.modules[module].info().scopes[Ns::Values][0];
        let types = &self.modules[module].info().scopes[Ns::Types][0];
        let items = values
            .iter()
            .filter_map(|(name, res)| match res {
                Res::Item(id) => Some((*name, (*id, false))),
                _ => None,
            })
            .chain(types.iter().filter_map(|(name, res)| match res {
                Res::Item(id) => Some((*name, (*id, true))),
                _ => None,
            }))
            .collect();

        ModuleMeta {
            name: *self.modules[module].name(),
            items,
        }
    }

    fn add_root(&mut self) {
        self.modules.insert(
            ModuleId::Virtual(0),
            Module::Normal(ModuleInfo {
                module: crate::Module {
                    name: Symbol::dummy(),
                    items: Vec::new(),
                },
                scopes: PerNs {
                    modules: vec![Rib::new(RibKind::Global)],
                    values: vec![Rib::new(RibKind::Global)],
                    types: vec![Rib::new(RibKind::Global)],
                    labels: Vec::new(),
                },
                imports: HashMap::new(),
                exports: Vec::new(),
                external: true,
            }),
        );

        self.set_module(ModuleId::Virtual(0));
        self.define(
            Ns::Values,
            Symbol::new("true"),
            Span::default(),
            Res::PrimVal(PrimVal::True),
        );
        self.define(
            Ns::Values,
            Symbol::new("false"),
            Span::default(),
            Res::PrimVal(PrimVal::False),
        );
        self.define(
            Ns::Values,
            Symbol::new("undefined"),
            Span::default(),
            Res::PrimVal(PrimVal::Undefined),
        );
        self.define(
            Ns::Types,
            Symbol::new("never"),
            Span::default(),
            Res::PrimTy(PrimTy::Never),
        );
        self.define(
            Ns::Types,
            Symbol::new("bool"),
            Span::default(),
            Res::PrimTy(PrimTy::Bool),
        );
        self.define(
            Ns::Types,
            Symbol::new("str"),
            Span::default(),
            Res::PrimTy(PrimTy::Str),
        );
        self.define(
            Ns::Types,
            Symbol::new("uint"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(255, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("int"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(255, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("float"),
            Span::default(),
            Res::PrimTy(PrimTy::Float(255)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u8"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(8, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u16"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(16, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u32"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(32, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u64"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(64, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u128"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(128, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("usize"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(0, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i8"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(8, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i16"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(16, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i32"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(32, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i64"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(64, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i128"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(128, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("isize"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(0, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("f32"),
            Span::default(),
            Res::PrimTy(PrimTy::Float(32)),
        );
        self.define(
            Ns::Types,
            Symbol::new("f64"),
            Span::default(),
            Res::PrimTy(PrimTy::Float(64)),
        );
    }
}

impl Rib {
    pub fn new(kind: RibKind) -> Self {
        Rib {
            names: HashMap::new(),
            kind,
        }
    }

    pub fn insert(&mut self, name: Symbol, res: Res) {
        self.names.insert(name, res);
    }

    pub fn get(&self, name: &Symbol) -> Option<Res> {
        self.names.get(name).copied()
    }

    pub fn contains(&self, name: &Symbol) -> bool {
        self.names.contains_key(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Symbol, &Res)> {
        self.names.iter()
    }
}

impl<T> std::ops::Index<Ns> for PerNs<T> {
    type Output = T;

    fn index(&self, ns: Ns) -> &Self::Output {
        match ns {
            Ns::Modules => &self.modules,
            Ns::Values => &self.values,
            Ns::Types => &self.types,
            Ns::Labels => &self.labels,
        }
    }
}

impl<T> std::ops::IndexMut<Ns> for PerNs<T> {
    fn index_mut(&mut self, ns: Ns) -> &mut Self::Output {
        match ns {
            Ns::Modules => &mut self.modules,
            Ns::Values => &mut self.values,
            Ns::Types => &mut self.types,
            Ns::Labels => &mut self.labels,
        }
    }
}

impl Module {
    pub fn name(&self) -> &Symbol {
        match self {
            Module::Normal(info) => &info.module.name,
            Module::Virtual(virt) => &virt.name,
        }
    }

    pub fn info(&self) -> &ModuleInfo {
        match self {
            Module::Normal(info) => info,
            _ => unreachable!(),
        }
    }

    pub fn info_mut(&mut self) -> &mut ModuleInfo {
        match self {
            Module::Normal(info) => info,
            _ => unreachable!(),
        }
    }

    pub fn virt(&self) -> &VirtualModule {
        match self {
            Module::Virtual(virt) => virt,
            _ => unreachable!(),
        }
    }

    pub fn virt_mut(&mut self) -> &mut VirtualModule {
        match self {
            Module::Virtual(virt) => virt,
            _ => unreachable!(),
        }
    }
}

impl Export {
    pub fn module(&self) -> &ModuleId {
        match self {
            Export::Value(m, _) => m,
            Export::Type(m, _) => m,
        }
    }

    pub fn id(&self) -> &Id {
        match self {
            Export::Value(_, id) => id,
            Export::Type(_, id) => id,
        }
    }
}

impl ModuleMeta {
    pub fn store(&self, path: impl AsRef<std::path::Path>) {
        let file = std::fs::File::create(path).unwrap();

        bincode::serialize_into(file, self).unwrap();
    }

    pub fn load(path: impl AsRef<std::path::Path>) -> Self {
        let file = std::fs::File::open(path).unwrap();

        bincode::deserialize_from(file).unwrap()
    }
}

impl std::fmt::Display for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ModuleId::Normal(name) => name.fmt(f),
            ModuleId::Virtual(id) => write!(f, "Virtual({})", id),
        }
    }
}
