pub mod db;

use base_db::input::SourceRoot;
use base_db::libs::LibSet;
use base_db::SourceDatabase;
use base_db::SourceDatabaseExt;
use hir::db::DefDatabase;
use syntax::ast::{self, AstNode, NameOwner};

pub fn build() {
    let mut rdb = db::RootDatabase::default();
    let mut libs = LibSet::default();
    let mut root = SourceRoot::new_local();
    let root_id = base_db::input::SourceRootId(0);
    let lib = libs.add_lib(root_id);
    let file = base_db::input::FileId(0);
    let path = "test/src/main.fc";

    root.insert_file(file, path);
    rdb.set_file_text(file, std::fs::read_to_string(path).unwrap().into());
    rdb.set_file_source_root(file, root_id);
    rdb.set_source_root(root_id, root.into());
    rdb.set_libs(libs.into());

    let ast_id_map = rdb.ast_id_map(file);

    println!("{:#?}", ast_id_map);
}
