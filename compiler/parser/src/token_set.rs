use std::ops::BitOr;

use crate::SyntaxKind;

#[derive(Clone, Copy)]
pub struct TokenSet(u128);

impl TokenSet {
    pub const EMPTY: Self = Self(0);

    pub const fn new(kinds: &[SyntaxKind]) -> Self {
        let mut res = 0u128;
        let mut i = 0;

        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1;
        }

        TokenSet(res)
    }

    pub const fn contains(&self, kind: SyntaxKind) -> bool {
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

impl BitOr<TokenSet> for TokenSet {
    type Output = Self;

    fn bitor(self, rhs: TokenSet) -> Self {
        Self(self.0 | rhs.0)
    }
}

pub struct TokenSetIter(u128, u16);

impl IntoIterator for TokenSet {
    type Item = SyntaxKind;
    type IntoIter = TokenSetIter;

    fn into_iter(self) -> Self::IntoIter {
        TokenSetIter(self.0, 0)
    }
}

impl Iterator for TokenSetIter {
    type Item = SyntaxKind;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            return None;
        }

        while self.0 & 1 == 0 {
            self.0 >>= 1;
            self.1 += 1;
        }

        let item = unsafe { std::mem::transmute(self.1) };
        self.0 >>= 1;
        self.1 += 1;
        Some(item)
    }
}
