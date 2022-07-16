use crate::syntax_kind::SyntaxKind;

#[derive(Clone, Copy)]
pub(crate) struct TokenSet(u128);

impl TokenSet {
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
}

const fn mask(kind: SyntaxKind) -> u128 {
    1u128 << (kind as usize)
}
