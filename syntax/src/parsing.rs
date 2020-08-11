use crate::ast::*;
use parser::error::Result;
use parser::parse::{Parse, ParseStream};

parser::token![ident "mod" TMod];
parser::token![ident "extern" TExtern];
parser::token![ident "fn" TFn];
parser::token![ident "do" TDo];
parser::token![ident "ref" TRef];
parser::token![ident "gc" TGc];
parser::token![ident "deref" TDeref];
parser::token![ident "type" TType];
parser::token![ident "while" TWhile];
parser::token![ident "loop" TLoop];
parser::token![ident "break" TBreak];
parser::token![ident "continue" TContinue];
parser::token![ident "return" TReturn];
parser::token![ident "defer" TDefer];
parser::token![ident "and" TAnd];
parser::token![ident "or" TOr];

parser::token![punct "(" TLParen/1];
parser::token![punct ")" TRParen/1];
parser::token![punct "[" TLBracket/1];
parser::token![punct "]" TRBracket/1];
parser::token![punct "{" TLBrace/1];
parser::token![punct "}" TRBrace/1];

parser::token![punct ":" TColon/1];
parser::token![punct ";" TSemi/1];
parser::token![punct "," TComma/1];
parser::token![punct "." TDot/1];
parser::token![punct "=" TEquals/1];
parser::token![punct "`" TTick/1];
parser::token![punct "->" TArrow/2];

parser::token![punct "+" TAdd/1];
parser::token![punct "-" TSub/1];
parser::token![punct "*" TMul/1];
parser::token![punct "/" TDiv/1];
parser::token![punct "%" TRem/1];
parser::token![punct "<" TLt/1];
parser::token![punct "<=" TLe/2];
parser::token![punct ">" TGt/1];
parser::token![punct ">=" TGe/2];
parser::token![punct "==" TEq/2];
parser::token![punct "!=" TNe/2];
parser::token![punct "&" TBitAnd/1];
parser::token![punct "|" TBitOr/1];
parser::token![punct "^" TBitXOr/1];
parser::token![punct "<<" TShl/2];
parser::token![punct ">>" TShr/2];

impl Parse for Package {
    fn parse(input: ParseStream) -> Result<Self> {
        let module = input.parse::<Module>()?;

        Ok(Package {
            span: module.span,
            module,
        })
    }
}

impl Parse for Module {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut items = Vec::new();

        while !input.is_empty() {
            match input.parse() {
                Ok(item) => items.push(item),
                Err(e) => {
                    input.reporter.add(e);

                    while !input.is_empty() && !input.peek::<Ident>() && !input.peek::<TMod>() {
                        input.bump();
                    }
                }
            }
        }

        Ok(Module {
            span: start.to(input.prev_span()),
            items,
        })
    }
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();

        if let Ok(_) = input.parse::<TMod>() {
            let name = input.parse()?;

            if let Ok(_) = input.parse::<TLBrace>() {
                let module = Module::parse_content(input)?;

                Ok(Item {
                    span: start.to(input.prev_span()),
                    name,
                    kind: ItemKind::Module { module },
                })
            } else {
                input.parse::<TSemi>()?;

                Ok(Item {
                    span: start.to(input.prev_span()),
                    name,
                    kind: ItemKind::Module { module },
                })
            }
        } else {
            unimplemented!();
        }
    }
}

impl Module {
    fn parse_content(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut tokens = Vec::new();
        let mut balance = vec![Balance::Brace];

        enum Balance {
            Paren,
            Brace,
            Bracket,
        }

        while !input.is_empty() && !balance.is_empty() {
            if let Ok(_) = input.parse::<TLParen>() {
                balance.push(Balance::Paren);
            } else if input.peek::<TLBrace>() {
                balance.push(Balance::Brace);
            } else if input.peek::<TLBracket>() {
                balance.push(Balance::Bracket);
            } else if input.peek::<TRParen>() {
                balance.pop().unwrap(); // TODO: validate
            } else if input.peek::<TRBrace>() {
                balance.pop().unwrap(); // TODO: validate
            } else if input.peek::<TRBracket>() {
                balance.pop().unwrap(); // TODO: validate
            }

            tokens.push(input.parse::<parser::buffer::Entry>()?);
        }

        let tokens = parser::buffer::TokenBuffer::new(tokens);
        let buffer = parser::parse::ParseBuffer::new(tokens.begin(), input.reporter, (), start);

        Module::parse(&buffer)
    }
}
