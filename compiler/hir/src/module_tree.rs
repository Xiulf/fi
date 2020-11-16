use crate::HirDatabase;
use data_structures::index_vec::IndexVec;
use std::sync::Arc;
use syntax::symbol::Ident;

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleTree {
    pub data: IndexVec<ModuleIndex, ModuleData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleData {
    pub file: source::FileId,
    pub name: Ident,
    pub children: Vec<ModuleIndex>,
}

data_structures::index_vec::define_index_type! {
    pub struct ModuleIndex = u32;
}

impl ModuleTree {
    pub fn query(db: &dyn HirDatabase, lib: source::LibId) -> Arc<Self> {
        let files = db.lib_files(lib);
        let asts = files
            .iter()
            .map(|&file| (file, db.parse(file)))
            .collect::<Vec<_>>();
        let mut tree = ModuleTree {
            data: IndexVec::new(),
        };

        for (file, ast) in &asts {
            tree.data.push(ModuleData {
                file: *file,
                name: ast.name,
                children: Vec::new(),
            });
        }

        for (i, (file, ast)) in asts.into_iter().enumerate() {
            for import in &ast.imports {
                if let Some(index) = tree
                    .data
                    .iter()
                    .position(|d| d.name.symbol == import.module.symbol)
                {
                    tree.data[ModuleIndex::from(i)]
                        .children
                        .push(ModuleIndex::from(index));
                } else {
                    db.to_diag_db()
                        .error(format!("Unknown module '{}'", import.module))
                        .with_label(diagnostics::Label::primary(file, import.module.span))
                        .finish();
                }
            }
        }

        if db.has_errors() {
            db.print_and_exit();
        }

        Arc::new(tree)
    }

    pub fn find(&self, name: syntax::symbol::Symbol) -> Option<&ModuleData> {
        for data in &self.data {
            if data.name.symbol == name {
                return Some(data);
            }
        }

        None
    }

    pub fn toposort(&self, diags: &dyn diagnostics::Diagnostics) -> Vec<ModuleData> {
        let mut graph = self
            .data
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, n)| (ModuleIndex::from(i), n))
            .collect::<Vec<_>>();

        let mut res = Vec::with_capacity(graph.len());
        let mut bases = graph
            .drain_filter(|(_, d)| d.children.is_empty())
            .collect::<Vec<_>>();

        while let Some((node_idx, node)) = bases.pop() {
            res.push(node);

            while let Some(idx) = graph
                .iter()
                .position(|(_, n)| n.children.contains(&node_idx))
            {
                let cidx = graph[idx]
                    .1
                    .children
                    .iter()
                    .position(|c| c == &node_idx)
                    .unwrap();

                graph[idx].1.children.remove(cidx);

                if graph[idx].1.children.is_empty() {
                    bases.push(graph.remove(idx));
                }
            }
        }

        if graph.is_empty() {
            res
        } else {
            let mut error = diags.error("Cyclic modules");

            for (_, module) in graph {
                error =
                    error.with_label(diagnostics::Label::primary(module.file, module.name.span));
            }

            error.finish();
            diags.print_and_exit();
        }
    }
}
