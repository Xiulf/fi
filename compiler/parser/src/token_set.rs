use std::ops::BitOr;

use crate::syntax_kind::SyntaxKind;

#[derive(Clone, Copy)]
pub struct TokenSet(u128);

impl TokenSet {
    pub(crate) const EMPTY: Self = Self(0);

    pub(crate) const fn new(kinds: &[SyntaxKind]) -> Self {
        let mut res = 0u128;
        let mut i = 0;

        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1;
        }

        TokenSet(res)
    }

    pub(crate) const fn contains(&self, kind: SyntaxKind) -> bool {
        self.0 & mask(kind) != 0
    }

    const fn add(self, kind: SyntaxKind) -> Self {
        Self(self.0 | mask(kind))
    }
}

const fn mask(kind: SyntaxKind) -> u128 {
    1u128 << (kind as usize)
}

impl From<SyntaxKind> for TokenSet {
    fn from(kind: SyntaxKind) -> Self {
        Self(mask(kind))
    }
}

impl BitOr for SyntaxKind {
    type Output = TokenSet;

    fn bitor(self, rhs: Self) -> Self::Output {
        TokenSet::new(&[self, rhs])
    }
}

impl BitOr<SyntaxKind> for TokenSet {
    type Output = Self;

    fn bitor(self, rhs: SyntaxKind) -> Self {
        self.add(rhs)
    }
}
