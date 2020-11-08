use source::SourceDatabase;
use syntax::SyntaxDatabase;

#[salsa::database(source::SourceDatabaseStorage, syntax::SyntaxDatabaseStorage)]
#[derive(Default)]
pub struct CompilerDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for CompilerDatabase {}

pub fn run() {
    let mut db = CompilerDatabase::default();
    let mut files = source::Files::new();
    let main = "test/src/main.shade";
    let main_src = std::fs::read_to_string(main).unwrap();
    let main_file = files.add(main, main_src.into());

    db.set_files(std::sync::Arc::new(files));

    let content = db.parse(main_file);

    println!("{:?}", content);
}
