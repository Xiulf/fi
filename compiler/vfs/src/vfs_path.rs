use std::ffi::OsStr;

use paths::{AbsPath, AbsPathBuf};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum VfsPath {
    PathBuf(AbsPathBuf),
    Virtual(VirtualPath),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(transparent)]
pub struct VirtualPath(String);

impl From<AbsPathBuf> for VfsPath {
    fn from(p: AbsPathBuf) -> Self {
        Self::PathBuf(p)
    }
}

impl VfsPath {
    pub fn new_virtual(path: String) -> Self {
        assert!(path.starts_with('/'));
        Self::Virtual(VirtualPath(path))
    }

    pub fn new_real(path: String) -> Self {
        Self::PathBuf(AbsPathBuf::assert(path.into()))
    }

    pub fn as_path(&self) -> Option<&AbsPath> {
        match self {
            | Self::PathBuf(p) => Some(p.as_path()),
            | Self::Virtual(_) => None,
        }
    }

    pub fn join(&self, path: &str) -> Option<Self> {
        match self {
            | Self::PathBuf(p) => Some(Self::PathBuf(p.join(path).normalize())),
            | Self::Virtual(p) => Some(Self::Virtual(p.join(path)?)),
        }
    }

    pub fn pop(&mut self) -> bool {
        match self {
            | Self::PathBuf(p) => p.pop(),
            | Self::Virtual(p) => p.pop(),
        }
    }

    pub fn starts_with(&self, other: &VfsPath) -> bool {
        match (self, other) {
            | (Self::PathBuf(a), Self::PathBuf(b)) => a.starts_with(b),
            | (Self::Virtual(a), Self::Virtual(b)) => a.starts_with(b),
            | (_, _) => false,
        }
    }

    pub fn parent(&self) -> Option<VfsPath> {
        let mut parent = self.clone();

        if parent.pop() {
            Some(parent)
        } else {
            None
        }
    }

    pub fn name_and_extension(&self) -> Option<(&str, Option<&str>)> {
        match self {
            | Self::PathBuf(p) => Some((p.file_stem()?.to_str()?, p.extension().and_then(OsStr::to_str))),
            | Self::Virtual(p) => p.name_and_extension(),
        }
    }

    pub(crate) fn encode(&self, buf: &mut Vec<u8>) {
        let tag = match self {
            | Self::PathBuf(_) => 0,
            | Self::Virtual(_) => 1,
        };

        buf.push(tag);

        match self {
            | Self::PathBuf(p) => {
                #[cfg(windows)]
                {
                    use windows_paths::Encode;
                    let path: &std::path::Path = p.as_ref();
                    let components = path.components();
                    let mut add_sep = false;

                    for component in components {
                        if add_sep {
                            windows_paths::SEP.encode(buf);
                        }

                        let len_before = buf.len();

                        match component {
                            | std::path::Component::Prefix(prefix) => prefix.kind().encode(buf),
                            | std::path::Component::RootDir => {
                                if !add_sep {
                                    component.as_os_str().encode(buf);
                                }
                            },
                            | _ => component.as_os_str().encode(buf),
                        }

                        add_sep = len_before != buf.len();
                    }
                }
                #[cfg(unix)]
                {
                    use std::os::unix::ffi::OsStrExt;
                    buf.extend(p.as_os_str().as_bytes());
                }
                #[cfg(not(any(windows, unix)))]
                buf.extend(p.as_os_str().to_string_lossy().as_bytes())
            },
            | Self::Virtual(p) => buf.extend(p.0.as_bytes()),
        }
    }
}

impl VirtualPath {
    fn starts_with(&self, other: &VirtualPath) -> bool {
        self.0.starts_with(&other.0)
    }

    fn pop(&mut self) -> bool {
        let pos = match self.0.rfind('/') {
            | Some(pos) => pos,
            | None => return false,
        };

        self.0 = self.0[..pos].to_string();
        true
    }

    fn join(&self, mut path: &str) -> Option<VirtualPath> {
        let mut res = self.clone();

        while path.starts_with("../") {
            if !res.pop() {
                return None;
            }
            path = &path["../".len()..];
        }

        path = path.trim_start_matches("./");
        res.0 = format!("{}/{}", res.0, path);
        Some(res)
    }

    fn name_and_extension(&self) -> Option<(&str, Option<&str>)> {
        let file_path = if self.0.ends_with('/') {
            &self.0[..&self.0.len() - 1]
        } else {
            &self.0
        };

        let file_name = match file_path.rfind('/') {
            | Some(position) => &file_path[position + 1..],
            | None => file_path,
        };

        if file_name.is_empty() {
            None
        } else {
            let mut file_stem_and_extension = file_name.rsplitn(2, '.');
            let extension = file_stem_and_extension.next();
            let file_stem = file_stem_and_extension.next();

            match (file_stem, extension) {
                | (None, None) => None,
                | (None | Some(""), Some(_)) => Some((file_name, None)),
                | (Some(file_stem), extension) => Some((file_stem, extension)),
            }
        }
    }
}

#[cfg(windows)]
mod windows_paths {
    pub(crate) trait Encode {
        fn encode(&self, buf: &mut Vec<u8>);
    }

    impl Encode for std::ffi::OsStr {
        fn encode(&self, buf: &mut Vec<u8>) {
            use std::os::windows::ffi::OsStrExt;
            for wchar in self.encode_wide() {
                buf.extend(wchar.to_le_bytes().iter().copied());
            }
        }
    }

    impl Encode for u8 {
        fn encode(&self, buf: &mut Vec<u8>) {
            let wide = *self as u16;
            buf.extend(wide.to_le_bytes().iter().copied())
        }
    }

    impl Encode for &str {
        fn encode(&self, buf: &mut Vec<u8>) {
            debug_assert!(self.is_ascii());
            for b in self.as_bytes() {
                b.encode(buf)
            }
        }
    }

    pub(crate) const SEP: &str = "\\";
    const VERBATIM: &str = "\\\\?\\";
    const UNC: &str = "UNC";
    const DEVICE: &str = "\\\\.\\";
    const COLON: &str = ":";

    impl Encode for std::path::Prefix<'_> {
        fn encode(&self, buf: &mut Vec<u8>) {
            match self {
                | std::path::Prefix::Verbatim(c) => {
                    VERBATIM.encode(buf);
                    c.encode(buf);
                },
                | std::path::Prefix::VerbatimUNC(server, share) => {
                    VERBATIM.encode(buf);
                    UNC.encode(buf);
                    SEP.encode(buf);
                    server.encode(buf);
                    SEP.encode(buf);
                    share.encode(buf);
                },
                | std::path::Prefix::VerbatimDisk(d) => {
                    VERBATIM.encode(buf);
                    d.encode(buf);
                    COLON.encode(buf);
                },
                | std::path::Prefix::DeviceNS(device) => {
                    DEVICE.encode(buf);
                    device.encode(buf);
                },
                | std::path::Prefix::UNC(server, share) => {
                    SEP.encode(buf);
                    SEP.encode(buf);
                    server.encode(buf);
                    SEP.encode(buf);
                    share.encode(buf);
                },
                | std::path::Prefix::Disk(d) => {
                    d.encode(buf);
                    COLON.encode(buf);
                },
            }
        }
    }
}
