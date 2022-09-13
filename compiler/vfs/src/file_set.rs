use fst::{IntoStreamer, Streamer};
use ra_ap_stdx::hash::NoHashHashMap;
use rustc_hash::FxHashMap;

use crate::vfs_path::VfsPath;
use crate::{FileId, VirtualFileSystem};

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct FileSet {
    files: FxHashMap<VfsPath, FileId>,
    paths: NoHashHashMap<FileId, VfsPath>,
}

#[derive(Debug)]
pub struct FileSetConfig {
    n_file_sets: usize,
    map: fst::Map<Vec<u8>>,
}

#[derive(Default)]
pub struct FileSetConfigBuilder {
    roots: Vec<Vec<VfsPath>>,
}

impl FileSet {
    pub fn len(&self) -> usize {
        self.files.len()
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.files.get(path).copied()
    }

    pub fn path_for_file(&self, file: FileId) -> Option<&VfsPath> {
        self.paths.get(&file)
    }

    pub fn insert(&mut self, file: FileId, path: VfsPath) {
        self.files.insert(path.clone(), file);
        self.paths.insert(file, path);
    }

    pub fn iter(&self) -> impl Iterator<Item = FileId> + '_ {
        self.paths.keys().copied()
    }
}

impl Default for FileSetConfig {
    fn default() -> Self {
        FileSetConfigBuilder::default().build()
    }
}

impl FileSetConfig {
    pub fn builder() -> FileSetConfigBuilder {
        FileSetConfigBuilder::default()
    }

    pub fn partition(&self, vfs: &VirtualFileSystem) -> Vec<FileSet> {
        let mut scratch = Vec::new();
        let mut res = vec![FileSet::default(); self.n_file_sets];

        for (file_id, path) in vfs.iter() {
            let root = self.classify(path, &mut scratch);

            res[root].insert(file_id, path.clone());
        }

        res
    }

    fn classify(&self, path: &VfsPath, scratch: &mut Vec<u8>) -> usize {
        scratch.clear();
        path.encode(scratch);

        let automaton = PrefixOf::new(scratch.as_slice());
        let mut longest_prefix = self.n_file_sets - 1;
        let mut stream = self.map.search(automaton).into_stream();

        while let Some((_, v)) = stream.next() {
            longest_prefix = v as usize;
        }

        longest_prefix
    }
}

impl FileSetConfigBuilder {
    pub fn len(&self) -> usize {
        self.roots.len()
    }

    pub fn add_file_set(&mut self, roots: Vec<VfsPath>) {
        self.roots.push(roots);
    }

    pub fn build(self) -> FileSetConfig {
        let n_file_sets = self.roots.len() + 1;
        let mut entries = Vec::new();

        for (i, paths) in self.roots.into_iter().enumerate() {
            for p in paths {
                let mut buf = Vec::new();

                p.encode(&mut buf);
                entries.push((buf, i as u64));
            }
        }

        entries.sort();
        entries.dedup_by(|(a, _), (b, _)| a == b);

        let map = fst::Map::from_iter(entries).unwrap();

        FileSetConfig { map, n_file_sets }
    }
}

struct PrefixOf<'a> {
    prefix_of: &'a [u8],
}

impl<'a> PrefixOf<'a> {
    fn new(prefix_of: &'a [u8]) -> Self {
        Self { prefix_of }
    }
}

impl fst::Automaton for PrefixOf<'_> {
    type State = usize;

    fn start(&self) -> Self::State {
        0
    }

    fn is_match(&self, state: &Self::State) -> bool {
        *state != 0
    }

    fn accept(&self, state: &Self::State, byte: u8) -> Self::State {
        if self.prefix_of.get(*state) == Some(&byte) {
            state + 1
        } else {
            !0
        }
    }
}
