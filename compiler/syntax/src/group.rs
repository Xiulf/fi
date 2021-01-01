use crate::ast::*;

impl Module {
    pub fn decl_groups(&self) -> DeclGroups {
        DeclGroups {
            decls: &self.decls,
            start: 0,
        }
    }
}

pub struct DeclGroups<'ast> {
    decls: &'ast [Decl],
    start: usize,
}

#[derive(Clone, Copy)]
pub enum DeclGroupKind {
    Foreign,
    Func(bool),
    Const(bool),
    Static(bool),
    Alias(bool),
    Data(bool),
    Trait,
    Impl,
}

pub struct ImplDeclGroups<'ast> {
    decls: &'ast [ImplDecl],
    start: usize,
}

#[derive(Clone, Copy)]
pub enum ImplDeclGroupKind {
    Func(bool),
}

pub struct LetBindingGroups<'ast> {
    bindings: &'ast [LetBinding],
    start: usize,
}

impl<'ast> ImplDeclGroups<'ast> {
    pub fn new(decls: &'ast [ImplDecl]) -> Self {
        ImplDeclGroups { decls, start: 0 }
    }
}

impl<'ast> LetBindingGroups<'ast> {
    pub fn new(bindings: &'ast [LetBinding]) -> Self {
        LetBindingGroups { bindings, start: 0 }
    }
}

impl Decl {
    fn group_kind(&self) -> DeclGroupKind {
        match &self.kind {
            DeclKind::Foreign { .. } => DeclGroupKind::Foreign,
            DeclKind::FuncTy { .. } => DeclGroupKind::Func(true),
            DeclKind::Func { .. } => DeclGroupKind::Func(false),
            DeclKind::ConstTy { .. } => DeclGroupKind::Const(true),
            DeclKind::Const { .. } => DeclGroupKind::Const(false),
            DeclKind::StaticTy { .. } => DeclGroupKind::Static(true),
            DeclKind::Static { .. } => DeclGroupKind::Static(false),
            DeclKind::AliasKind { .. } => DeclGroupKind::Alias(true),
            DeclKind::Alias { .. } => DeclGroupKind::Alias(false),
            DeclKind::DataKind { .. } => DeclGroupKind::Data(true),
            DeclKind::Data { .. } => DeclGroupKind::Data(false),
            DeclKind::Trait { .. } => DeclGroupKind::Trait,
            DeclKind::ImplChain { .. } => DeclGroupKind::Impl,
        }
    }
}

impl DeclGroupKind {
    fn max(&self) -> usize {
        match self {
            DeclGroupKind::Foreign => 1,
            DeclGroupKind::Func(_) => usize::max_value(),
            DeclGroupKind::Const(_) => 2,
            DeclGroupKind::Static(_) => 2,
            DeclGroupKind::Alias(_) => 2,
            DeclGroupKind::Data(_) => 2,
            DeclGroupKind::Trait => 1,
            DeclGroupKind::Impl => 1,
        }
    }
}

impl PartialEq for DeclGroupKind {
    fn eq(&self, other: &Self) -> bool {
        use DeclGroupKind::*;

        match (self, other) {
            (Foreign, Foreign) => true,
            (Func(true), Func(false)) => true,
            (Func(false), Func(false)) => true,
            (Const(true), Const(false)) => true,
            // (Const(false), Const(false)) => true,
            (Static(true), Static(false)) => true,
            // (Static(false), Static(false)) => true,
            (Alias(true), Alias(false)) => true,
            // (Alias(false), Alias(false)) => true,
            (Data(true), Data(false)) => true,
            // (Data(false), Data(false)) => true,
            (Trait, Trait) => true,
            (Impl, Impl) => true,
            _ => false,
        }
    }
}

impl ImplDecl {
    fn group_kind(&self) -> ImplDeclGroupKind {
        match &self.kind {
            ImplDeclKind::FuncTy { .. } => ImplDeclGroupKind::Func(true),
            ImplDeclKind::Func { .. } => ImplDeclGroupKind::Func(false),
        }
    }
}

impl ImplDeclGroupKind {
    fn max(&self) -> usize {
        match self {
            ImplDeclGroupKind::Func(_) => usize::max_value(),
        }
    }
}

impl PartialEq for ImplDeclGroupKind {
    fn eq(&self, other: &Self) -> bool {
        use ImplDeclGroupKind::*;

        match (self, other) {
            (Func(true), Func(false)) => true,
            (Func(false), Func(false)) => true,
            _ => false,
        }
    }
}

impl<'ast> Iterator for DeclGroups<'ast> {
    type Item = (DeclGroupKind, &'ast [Decl]);

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.decls.len() {
            return None;
        }

        let mut pos = self.start;
        let name = self.decls[pos].name.symbol;
        let mut kind = self.decls[pos].group_kind();

        pos += 1;

        while pos < self.decls.len()
            && self.decls[pos].name.symbol == name
            && pos - self.start < kind.max()
        {
            let kind2 = self.decls[pos].group_kind();

            if kind == kind2 {
                kind = kind2;
                pos += 1;
            } else {
                break;
            }
        }

        let start = std::mem::replace(&mut self.start, pos);

        Some((kind, &self.decls[start..pos]))
    }
}

impl<'ast> Iterator for ImplDeclGroups<'ast> {
    type Item = (ImplDeclGroupKind, &'ast [ImplDecl]);

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.decls.len() {
            return None;
        }

        let mut pos = self.start;
        let name = self.decls[pos].name.symbol;
        let kind = self.decls[pos].group_kind();

        pos += 1;

        while pos < self.decls.len()
            && self.decls[pos].name.symbol == name
            && pos - self.start < kind.max()
        {
            let kind2 = self.decls[pos].group_kind();

            if kind == kind2 {
                pos += 1;
            } else {
                break;
            }
        }

        let start = std::mem::replace(&mut self.start, pos);

        Some((kind, &self.decls[start..pos]))
    }
}

impl<'ast> Iterator for LetBindingGroups<'ast> {
    type Item = &'ast [LetBinding];

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.bindings.len() {
            return None;
        }

        let start = self.start;
        let end;

        match self.bindings[self.start].kind {
            LetBindingKind::Type { .. } => match self.bindings[self.start + 1].kind {
                LetBindingKind::Type { .. } => end = start + 1,
                LetBindingKind::Value { .. } => end = start + 2,
            },
            LetBindingKind::Value { .. } => end = start + 1,
        }

        self.start = end;

        Some(&self.bindings[start..end])
    }
}
