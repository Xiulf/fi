use crate::parsing::lexer::Token;
use parser::TokenSource;
use rowan::{TextRange, TextSize};

crate struct TextTokenSource<'t> {
    text: &'t str,
    token_offset_pairs: Vec<(Token, TextSize)>,
    curr: (parser::Token, usize),
}

impl<'t> TokenSource for TextTokenSource<'t> {
    fn current(&self) -> parser::Token {
        self.curr.0
    }

    fn lookahead_nth(&self, n: usize) -> parser::Token {
        mk_token(self.curr.1 + n, &self.token_offset_pairs)
    }

    fn bump(&mut self) {
        if self.curr.0.kind == parser::syntax_kind::SyntaxKind::EOF {
            return;
        }

        let pos = self.curr.1 + 1;

        self.curr = (mk_token(pos, &self.token_offset_pairs), pos);
    }

    fn is_text(&self, text: &str) -> bool {
        self.token_offset_pairs
            .get(self.curr.1)
            .map(|(token, offset)| &self.text[TextRange::at(*offset, token.len)] == text)
            .unwrap_or(false)
    }
}

fn mk_token(pos: usize, token_offset_pairs: &[(Token, TextSize)]) -> parser::Token {
    let kind = match token_offset_pairs.get(pos) {
        | Some((token, _)) => token.kind,
        | None => parser::syntax_kind::SyntaxKind::EOF,
    };

    parser::Token { kind }
}

impl<'t> TextTokenSource<'t> {
    crate fn new(text: &'t str, raw_tokens: &'t [Token]) -> Self {
        let token_offset_pairs = raw_tokens
            .iter()
            .filter_map({
                let mut len = 0.into();

                move |token| {
                    let pair = if token.kind.is_trivia() {
                        None
                    } else {
                        Some((*token, len))
                    };

                    len += token.len;
                    pair
                }
            })
            .collect::<Vec<_>>();

        let first = mk_token(0, &token_offset_pairs);

        TextTokenSource {
            text,
            token_offset_pairs,
            curr: (first, 0),
        }
    }
}
