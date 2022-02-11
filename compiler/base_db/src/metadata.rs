use crate::{libs::LibId, SourceDatabaseExt};
use serde::{Deserialize, Serialize};
use std::{fs::File, sync::Arc};

#[derive(Default, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct Metadata {}

pub fn read_metadata(db: &dyn SourceDatabaseExt, lib: LibId) -> Arc<Metadata> {
    let target_dir = db.target_dir(lib);
    let metadata_dir = target_dir.join("input.shade-meta");

    if let Ok(file) = File::open(metadata_dir) {
        let meta: Metadata = bincode::deserialize_from(file).unwrap();

        Arc::new(meta)
    } else {
        Arc::new(Metadata::default())
    }
}
