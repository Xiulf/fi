use crate::buffer::Cursor;

pub trait Token {
    fn peek(cursor: Cursor) -> bool;
    fn display() -> &'static str;
}

#[macro_export]
macro_rules! token {
    (punct $token:literal pub $name:ident/$n:literal) => {
        #[derive(Clone)]
        pub struct $name {
            span: diagnostics::Span
        }
        
        $crate::token!(@i punct $token $name/$n);
    };
    
    (ident $token:literal pub $name:ident) => {
        #[derive(Clone)]
        pub struct $name {
            span: diagnostics::Span
        }
        
        $crate:token!(@i ident $token $name);
    };
    
    (punct $token:literal $name:ident/$n:literal) => {
        #[derive(Clone)]
        struct $name {
            span: diagnostics::Span
        }
        
        $crate::token!(@i punct $token $name/$n);
    };
    
    (ident $token:literal $name:ident) => {
        #[derive(Clone)]
        struct $name {
            span: diagnostics::Span
        }
        
        $crate::token!(@i ident $token $name);
    };
    
    (@i punct $token:literal $name:ident/$n:literal) => {
        $crate::token!(@i $token $name);
        
        impl<D> $crate::parse::Parse<D> for $name {
            fn parse(input: $crate::parse::ParseStream<D>) -> $crate::error::Result<$name> {
                use diagnostics::Spanned;
                
                let mut span = input.span();
                
                input.step(|cur| {
                    let mut cursor = *cur;
                    
                    for (i, ch) in $token.chars().enumerate() {
                        match cursor.punct() {
                            Some((punct, rest)) => {
                                span.end = punct.span().end;
                                
                                if punct.ch != ch {
                                    break;
                                } else if i == $n - 1 {
                                    return Ok(((), rest));
                                } else if punct.spacing != $crate::punct::Spacing::Joint {
                                    break;
                                }
                                
                                cursor = rest;
                            },
                            None => break
                        }
                    }
                    Err(cur.error(concat!("expected `", $token, "`")))
                })?;
                
                Ok($name::from(span))
            }
        }
        
        impl $crate::token::Token for $name {
            fn peek(mut cursor: $crate::buffer::Cursor) -> bool {
                for (i, ch) in $token.chars().enumerate() {
                    match cursor.punct() {
                        Some((punct, rest)) => {
                            if punct.ch != ch {
                                break;
                            } else if i == $n - 1 {
                                return true;
                            } else if punct.spacing != $crate::punct::Spacing::Joint {
                                break;
                            }
                            
                            cursor = rest;
                        },
                        None => break
                    }
                }
                
                false
            }
            
            fn display() -> &'static str {
                $token
            }
        }
        
        impl $crate::parse::ToTokens for $name {
            fn to_tokens(&self) -> $crate::buffer::TokenBuffer {
                $crate::buffer::TokenBuffer::new({
                    let chars = $token.chars().collect::<Vec<_>>();
                    
                    (0..chars.len()).map(|i| $crate::buffer::Entry::Punct($crate::punct::Punct {
                        span: diagnostics::Span {
                            start: diagnostics::Position {
                                offset: self.span.start.offset + i,
                                col: self.span.start.col + i,
                                line: self.span.start.line
                            },
                            end: diagnostics::Position {
                                offset: self.span.start.offset + i + 1,
                                col: self.span.start.col + i + 1,
                                line: self.span.start.line
                            },
                            file: self.span.file.clone()
                        },
                        ch: chars[i],
                        spacing: if i != chars.len() -1 { $crate::punct::Spacing::Joint } else { $crate::punct::Spacing::Alone }
                    })).collect()
                })
            }
        }
    };
    
    (@i ident $token:literal $name:ident) => {
        $crate::token!(@i $token $name);
        
        impl<D> $crate::parse::Parse<D> for $name {
            fn parse(input: $crate::parse::ParseStream<D>) -> $crate::error::Result<$name> {
                input.step(|cursor| {
                    if let Some((ident, rest)) = cursor.ident() {
                        if ident.name == $token {
                            use diagnostics::Spanned;
                            
                            return Ok(($name::from(ident.span()), rest))
                        }
                    }
                    
                    Err(cursor.error(concat!("expected `", $token, "`")))
                })
            }
        }
        
        impl $crate::token::Token for $name {
            fn peek(cursor: $crate::buffer::Cursor) -> bool {
                if let Some((ident, _rest)) = cursor.ident() {
                    ident.name == $token
                } else {
                    false
                }
            }
            
            fn display() -> &'static str {
                $token
            }
        }
        
        impl $crate::parse::ToTokens for $name {
            fn to_tokens(&self) -> $crate::buffer::TokenBuffer {
                $crate::buffer::TokenBuffer::new(vec![
                    $crate::buffer::Entry::Ident($crate::ident::Ident {
                        span: self.span,
                        name: $token.to_string()
                    })
                ])
            }
        }
    };
    
    (@i $token:literal $name:ident) => {
        impl diagnostics::Spanned for $name {
            fn span(&self) -> diagnostics::Span {
                self.span
            }
        }
        
        impl std::convert::From<diagnostics::Span> for $name {
            fn from(source: diagnostics::Span) -> $name {
                $name {
                    span: source
                }
            }
        }
        
        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, stringify!($name))
            }
        }
        
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                $token.fmt(f)
            }
        }
        
        impl std::default::Default for $name {
            fn default() -> $name {
                $name {
                    span: Default::default()
                }
            }
        }
    };
}
