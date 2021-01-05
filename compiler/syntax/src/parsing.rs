use crate::ast::*;
use parser::attr::Attr;
use parser::error::Result;
use parser::layout::{LytEnd, LytSep, LytStart};
use parser::literal::{IntLiteral, Literal, StringLiteral};
use parser::parse::{Parse, ParseStream};

parser::token![ident "module" TModule];
parser::token![ident "where" TWhere];
parser::token![ident "import" TImport];
parser::token![ident "hiding" THiding];
parser::token![ident "as" TAs];
parser::token![ident "foreign" TForeign];
parser::token![ident "fn" TFn];
parser::token![ident "const" TConst];
parser::token![ident "static" TStatic];
parser::token![ident "alias" TAlias];
parser::token![ident "data" TData];
parser::token![ident "trait" TTrait];
parser::token![ident "impl" TImpl];
parser::token![ident "forall" TForall];
parser::token![ident "if" TIf];
parser::token![ident "then" TThen];
parser::token![ident "else" TElse];
parser::token![ident "case" TCase];
parser::token![ident "of" TOf];
parser::token![ident "let" TLet];
parser::token![ident "in" TIn];
parser::token![ident "do" TDo];
parser::token![ident "while" TWhile];
parser::token![ident "loop" TLoop];
parser::token![ident "break" TBreak];
parser::token![ident "next" TNext];
parser::token![ident "return" TReturn];

parser::token![punct "(" TLParen/1];
parser::token![punct ")" TRParen/1];
parser::token![punct "{" TLBrace/1];
parser::token![punct "}" TRBrace/1];
parser::token![punct "[" TLBracket/1];
parser::token![punct "]" TRBracket/1];

parser::token![punct "," TComma/1];
parser::token![punct "." TDot/1];
parser::token![punct ":" TColon/1];
parser::token![punct ".." TDblDot/2];
parser::token![punct "::" TDblColon/2];
parser::token![punct "->" TArrow/2];
parser::token![punct "=>" TFatArrow/2];
parser::token![punct "<-" TLeftArrow/2];
parser::token![punct "=" TEquals/1];
parser::token![punct "|" TBar/1];
parser::token![punct "?" TQmark/1];
parser::token![punct "@" TAt/1];
parser::token![ident "_" TWildcard];

parser::token![punct "-" TNeg/1];
parser::token![punct "!" TNot/1];
parser::token![punct "~" TBitNot/1];

parser::token![punct "+" TAdd/1];
parser::token![punct "-" TSub/1];
parser::token![punct "*" TMul/1];
parser::token![punct "/" TDiv/1];
parser::token![punct "%" TRem/1];
parser::token![punct "==" TEq/2];
parser::token![punct "!=" TNe/2];
parser::token![punct "<" TLt/1];
parser::token![punct "<=" TLe/2];
parser::token![punct ">" TGt/1];
parser::token![punct ">=" TGe/2];
parser::token![punct "&" TBitAnd/1];
parser::token![punct "|" TBitOr/1];
parser::token![punct "^" TBitXor/1];
parser::token![punct "<<" TShl/2];
parser::token![punct ">>" TShr/2];
parser::token![ident "and" TAnd];
parser::token![ident "or" TOr];

impl Parse for Module {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut attrs = Vec::new();

        while !input.is_empty() && Attribute::peek(input) {
            attrs.push(input.parse()?);
        }

        let _ = input.parse::<TModule>()?;
        let name = Module::parse_name(input)?;
        let exports = input.parse()?;
        let _ = input.parse::<TWhere>()?;
        let _ = input.parse::<LytStart>()?;
        let mut imports = Vec::new();
        let mut decls = Vec::new();
        let mut parse_decls = true;

        while !input.is_empty() && input.peek::<TImport>() {
            imports.push(input.parse()?);

            if input.peek::<LytEnd>() {
                parse_decls = false;
                break;
            } else {
                input.parse::<LytSep>()?;
            }
        }

        while parse_decls && !input.is_empty() && Decl::peek(input) {
            decls.push(input.parse()?);

            if input.peek::<LytEnd>() {
                break;
            } else {
                input.parse::<LytSep>()?;
            }
        }

        input.parse::<LytEnd>()?;

        Ok(Module {
            span: start.merge(input.prev_span()),
            attrs,
            name,
            exports,
            imports,
            decls,
        })
    }
}

impl Module {
    fn parse_name(input: ParseStream) -> Result<Ident> {
        let mut parts = vec![input.parse::<Ident>()?];

        while !input.is_empty() && input.peek::<TDot>() {
            input.parse::<TDot>()?;
            parts.push(input.parse()?);
        }

        Ok(Ident {
            span: parts[0].span.merge(parts[parts.len() - 1].span),
            symbol: Symbol::new(
                parts
                    .into_iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join("."),
            ),
        })
    }
}

impl Parse for Attribute {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(attr) = input.parse::<Attr>() {
            Ok(Attribute {
                span: attr.span,
                name: Ident {
                    span: attr.span,
                    symbol: Symbol::new("doc"),
                },
                body: Some(AttrBody {
                    span: attr.span,
                    args: vec![AttrArg::Literal(Literal::String(StringLiteral {
                        span: Span::new(
                            attr.span.start() + codespan::ByteOffset::from_str_len("--|"),
                            attr.span.end(),
                        ),
                        text: attr.text,
                    }))],
                }),
            })
        } else {
            let start = input.span();
            let _ = input.parse::<TLBracket>()?;
            let name = input.parse()?;
            let body = if input.peek::<TLParen>() {
                Some(input.parse()?)
            } else {
                None
            };

            input.parse::<TRBracket>()?;

            Ok(Attribute {
                span: start.merge(input.prev_span()),
                name,
                body,
            })
        }
    }
}

impl Attribute {
    fn peek(input: ParseStream) -> bool {
        input.peek::<Attr>() || input.peek::<TLBracket>()
    }
}

impl Parse for AttrBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TLParen>()?;
        let mut args = Vec::new();

        while !input.is_empty() && !input.peek::<TRParen>() {
            args.push(input.parse()?);

            if !input.peek::<TRParen>() {
                input.parse::<TComma>()?;
            }
        }

        input.parse::<TRParen>()?;

        Ok(AttrBody {
            span: start.merge(input.prev_span()),
            args,
        })
    }
}

impl Parse for AttrArg {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(lit) = input.parse() {
            Ok(AttrArg::Literal(lit))
        } else if let Ok(name) = input.parse() {
            if let Ok(_) = input.parse::<TLParen>() {
                let mut args = Vec::new();

                while !input.is_empty() && !input.peek::<TRParen>() {
                    args.push(input.parse()?);

                    if !input.peek::<TRParen>() {
                        input.parse::<TComma>()?;
                    }
                }

                input.parse::<TRParen>()?;

                Ok(AttrArg::Call(name, args))
            } else {
                let _ = input.parse::<TEquals>()?;
                let val = input.parse()?;

                Ok(AttrArg::Field(name, val))
            }
        } else {
            input.error("expected a literal or an identifier", "E0006")
        }
    }
}

impl Parse for Exports {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TLParen>() {
            let mut exports = Vec::new();

            while !input.is_empty() && !input.peek::<TRParen>() {
                exports.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRParen>()?;

            Ok(Exports::Some(exports))
        } else {
            Ok(Exports::All)
        }
    }
}

impl Parse for Export {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut kind = if let Ok(_) = input.parse::<TModule>() {
            ExportKind::Module
        } else {
            ExportKind::Any
        };

        let name = input.parse::<Ident>()?;

        if let Ok(_) = input.parse::<TLParen>() {
            let ctors = if let Ok(_) = input.parse::<TDblDot>() {
                ExportGroup::All
            } else {
                let mut ctors = Vec::new();

                while !input.is_empty() && !input.peek::<TRParen>() {
                    ctors.push(input.parse()?);

                    if !input.peek::<TRParen>() {
                        input.parse::<TComma>()?;
                    }
                }

                ExportGroup::Some(ctors)
            };

            input.parse::<TRParen>()?;
            kind = ExportKind::Group(ctors);
        }

        Ok(Export {
            span: start.merge(input.prev_span()),
            name,
            kind,
        })
    }
}

impl Parse for ImportDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TImport>()?;
        let module = Module::parse_name(input)?;
        let names = if input.peek::<THiding>() || input.peek::<TLParen>() {
            let hiding = input.parse::<THiding>().is_ok();
            let _ = input.parse::<TLParen>()?;
            let mut imports = Vec::new();

            while !input.is_empty() && !input.peek::<TRParen>() {
                imports.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRParen>()?;

            Some((hiding, imports))
        } else {
            None
        };

        let qual = if let Ok(_) = input.parse::<TAs>() {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(ImportDecl {
            span: start.merge(input.prev_span()),
            module,
            names,
            qual,
        })
    }
}

impl Parse for Import {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse::<Ident>()?;
        let kind = if let Ok(_) = input.parse::<TLParen>() {
            let ctors = if let Ok(_) = input.parse::<TDblDot>() {
                ImportGroup::All
            } else {
                let mut ctors = Vec::new();

                while !input.is_empty() && !input.peek::<TRParen>() {
                    ctors.push(input.parse()?);

                    if !input.peek::<TRParen>() {
                        input.parse::<TComma>()?;
                    }
                }

                ImportGroup::Some(ctors)
            };

            input.parse::<TRParen>()?;

            ImportKind::Group(ctors)
        } else {
            ImportKind::Any
        };

        Ok(Import {
            span: start.merge(input.prev_span()),
            name,
            kind,
        })
    }
}

impl Parse for Decl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut attrs = Vec::new();

        while !input.is_empty() && Attribute::peek(input) {
            let _ = attrs.push(input.parse()?);
            let _ = input.parse::<LytSep>();
        }

        let (name, kind) = if let Ok(_) = input.parse::<TForeign>() {
            let kind = if let Ok(_) = input.parse::<TFn>() {
                ForeignKind::Func
            } else if let Ok(_) = input.parse::<TStatic>() {
                ForeignKind::Static
            } else {
                return input.error("expected 'fn' or 'static'", "E0006");
            };

            let name = input.parse()?;
            let _ = input.parse::<TDblColon>()?;
            let ty = input.parse()?;

            (name, DeclKind::Foreign { ty, kind })
        } else if let Ok(_) = input.parse::<TFn>() {
            let name = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TDblColon>() {
                let ty = input.parse()?;

                DeclKind::FuncTy { ty }
            } else {
                let mut pats = Vec::new();

                while !input.is_empty() && Pat::peek(input) {
                    pats.push(Pat::atom(input)?);
                }

                let val = input.parse()?;

                DeclKind::Func { pats, val }
            };

            (name, kind)
        } else if let Ok(_) = input.parse::<TConst>() {
            let name = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TDblColon>() {
                let ty = input.parse()?;

                DeclKind::ConstTy { ty }
            } else {
                let _ = input.parse::<TEquals>()?;
                let val = input.parse()?;

                DeclKind::Const { val }
            };

            (name, kind)
        } else if let Ok(_) = input.parse::<TStatic>() {
            let name = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TDblColon>() {
                let ty = input.parse()?;

                DeclKind::StaticTy { ty }
            } else {
                let _ = input.parse::<TEquals>()?;
                let val = input.parse()?;

                DeclKind::Static { val }
            };

            (name, kind)
        } else if let Ok(_) = input.parse::<TAlias>() {
            let name = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TDblColon>() {
                let kind = input.parse()?;

                DeclKind::AliasKind { kind }
            } else {
                let mut vars = Vec::new();

                while !input.is_empty() && TypeVar::peek(input) {
                    vars.push(input.parse()?);
                }

                let _ = input.parse::<TEquals>()?;
                let ty = input.parse()?;

                DeclKind::Alias { vars, ty }
            };

            (name, kind)
        } else if let Ok(_) = input.parse::<TData>() {
            let name = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TDblColon>() {
                let kind = input.parse()?;

                DeclKind::DataKind { kind }
            } else {
                let mut vars = Vec::new();

                while !input.is_empty() && TypeVar::peek(input) {
                    vars.push(input.parse()?);
                }

                let head = DataHead {
                    span: start.merge(input.prev_span()),
                    vars,
                };

                let body = if let Ok(_) = input.parse::<TEquals>() {
                    let mut ctors = vec![input.parse()?];

                    while !input.is_empty() && input.peek::<TBar>() {
                        input.parse::<TBar>()?;
                        ctors.push(input.parse()?);
                    }

                    Some(ctors)
                } else {
                    None
                };

                DeclKind::Data { head, body }
            };

            (name, kind)
        } else if let Ok(_) = input.parse::<TTrait>() {
            let mut parent = Vec::new();
            let mut fork = input.fork();

            while fork.parse::<Constraint>().is_ok() && fork.peek::<TFatArrow>() {
                let cs = input.parse()?;
                let _ = input.parse::<TFatArrow>()?;

                parent.push(cs);
                fork = input.fork();
            }

            let name = input.parse()?;
            let mut vars = Vec::new();

            while !input.is_empty() && (input.peek::<Ident>() || input.peek::<TLParen>()) {
                vars.push(input.parse()?);
            }

            let mut fundeps = Vec::new();

            if let Ok(_) = input.parse::<TBar>() {
                while !input.is_empty() && (input.peek::<TArrow>() || input.peek::<Ident>()) {
                    fundeps.push(input.parse()?);

                    if input.peek::<TComma>() {
                        input.bump();
                    } else {
                        break;
                    }
                }
            }

            let head = TraitHead {
                span: start.merge(input.prev_span()),
                parent: if parent.is_empty() {
                    None
                } else {
                    Some(parent)
                },
                vars,
                fundeps,
            };

            let body = if input.peek::<TWhere>() {
                Some(input.parse()?)
            } else {
                None
            };

            (name, DeclKind::Trait { head, body })
        } else if input.peek::<TImpl>() {
            let mut impls = vec![input.parse::<Impl>()?];

            while !input.is_empty() && input.peek::<TElse>() {
                let _ = input.parse::<TElse>()?;
                let impl_ = input.parse::<Impl>()?;

                if impl_.head.iface.symbol != impls[0].head.iface.symbol {
                    return input.error_at(
                        "impl chains must implement the same interface",
                        "E0007",
                        impl_.head.iface.span,
                    );
                }

                impls.push(impl_);
            }

            (impls[0].head.name, DeclKind::ImplChain { impls })
        } else {
            return input.error(
                "expected 'foreign', 'fn', 'alias', 'data', 'trait' or 'impl'",
                "E0006",
            );
        };

        Ok(Decl {
            span: start.merge(input.prev_span()),
            attrs,
            name,
            kind,
        })
    }
}

impl Decl {
    fn peek(input: ParseStream) -> bool {
        Attribute::peek(input)
            || input.peek::<TForeign>()
            || input.peek::<TFn>()
            || input.peek::<TConst>()
            || input.peek::<TStatic>()
            || input.peek::<TAlias>()
            || input.peek::<TData>()
            || input.peek::<TTrait>()
            || input.peek::<TImpl>()
    }
}

impl Parse for DataCtor {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let mut tys = Vec::new();

        while !input.is_empty() && Type::peek(input) {
            tys.push(Type::infix(input)?);
        }

        Ok(DataCtor {
            span: start.merge(input.prev_span()),
            name,
            tys,
        })
    }
}

impl Parse for FunDep {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TArrow>() {
            let mut names = vec![input.parse()?];

            while !input.is_empty() && input.peek::<Ident>() {
                names.push(input.parse()?);
            }

            Ok(FunDep::Determined(names))
        } else {
            let mut left = vec![input.parse()?];

            while !input.is_empty() && input.peek::<Ident>() {
                left.push(input.parse()?);
            }

            let _ = input.parse::<TArrow>()?;
            let mut right = vec![input.parse()?];

            while !input.is_empty() && input.peek::<Ident>() {
                right.push(input.parse()?);
            }

            Ok(FunDep::Determines(left, right))
        }
    }
}

impl Parse for TraitBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TWhere>()?;
        let _ = input.parse::<LytStart>()?;
        let mut decls = Vec::new();

        while !input.is_empty() && TraitDecl::peek(input) {
            decls.push(input.parse()?);

            if input.peek::<LytEnd>() {
                break;
            } else {
                input.parse::<LytSep>()?;
            }
        }

        input.parse::<LytEnd>()?;

        Ok(TraitBody {
            span: start.merge(input.prev_span()),
            decls,
        })
    }
}

impl Parse for TraitDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let (name, kind) = if let Ok(_) = input.parse::<TFn>() {
            let name = input.parse()?;
            let _ = input.parse::<TDblColon>()?;
            let ty = input.parse()?;

            (name, TraitDeclKind::FuncTy { ty })
        } else {
            return input.error("expected 'fn'", "E0006");
        };

        Ok(TraitDecl {
            span: start.merge(input.prev_span()),
            name,
            kind,
        })
    }
}

impl TraitDecl {
    fn peek(input: ParseStream) -> bool {
        input.peek::<TFn>()
    }
}

impl Parse for Impl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TImpl>()?;
        let name = input.parse()?;
        let _ = input.parse::<TDblColon>()?;
        let mut cs = Vec::new();
        let mut fork = input.fork();

        while !fork.is_empty() && fork.parse::<Constraint>().is_ok() && fork.peek::<TFatArrow>() {
            cs.push(input.parse()?);
            input.parse::<TFatArrow>()?;
            fork = input.fork();
        }

        let iface = input.parse()?;
        let mut tys = Vec::new();

        while !input.is_empty() && Type::peek(input) {
            tys.push(Type::atom(input)?);
        }

        let head = ImplHead {
            span: start.merge(input.prev_span()),
            name,
            cs: if cs.is_empty() { None } else { Some(cs) },
            iface,
            tys,
        };

        let body = if input.peek::<TWhere>() {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Impl {
            span: start.merge(input.prev_span()),
            head,
            body,
        })
    }
}

impl Parse for ImplBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TWhere>()?;
        let _ = input.parse::<LytStart>()?;
        let mut decls = Vec::new();

        while !input.is_empty() && ImplDecl::peek(input) {
            decls.push(input.parse()?);

            if input.peek::<LytEnd>() {
                break;
            } else {
                input.parse::<LytSep>()?;
            }
        }

        input.parse::<LytEnd>()?;

        Ok(ImplBody {
            span: start.merge(input.prev_span()),
            decls,
        })
    }
}

impl Parse for ImplDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let (name, kind) = if let Ok(_) = input.parse::<TFn>() {
            let name = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TDblColon>() {
                let ty = input.parse()?;

                ImplDeclKind::FuncTy { ty }
            } else {
                let mut pats = Vec::new();

                while !input.is_empty() && Pat::peek(input) {
                    pats.push(Pat::atom(input)?);
                }

                let val = input.parse()?;

                ImplDeclKind::Func { pats, val }
            };

            (name, kind)
        } else {
            return input.error("expected 'fn'", "E0006");
        };

        Ok(ImplDecl {
            span: start.merge(input.prev_span()),
            name,
            kind,
        })
    }
}

impl ImplDecl {
    fn peek(input: ParseStream) -> bool {
        input.peek::<TFn>()
    }
}

impl Parse for Pat {
    fn parse(input: ParseStream) -> Result<Self> {
        let pat = Pat::ctor(input)?;

        if let Ok(_) = input.parse::<TDblColon>() {
            let ty = input.parse::<Type>()?;

            Ok(Pat {
                span: pat.span.merge(ty.span),
                kind: PatKind::Typed {
                    pat: Box::new(pat),
                    ty,
                },
            })
        } else {
            Ok(pat)
        }
    }
}

impl Pat {
    fn ctor(input: ParseStream) -> Result<Self> {
        let base = Pat::named(input)?;
        let mut pats = Vec::new();

        if let PatKind::Ident { .. } = &base.kind {
            while Pat::peek(input) {
                pats.push(Pat::atom(input)?);
            }
        }

        if pats.is_empty() {
            Ok(base)
        } else if let PatKind::Ident { name } = base.kind {
            Ok(Pat {
                span: base.span.merge(input.prev_span()),
                kind: PatKind::Ctor { name, pats },
            })
        } else {
            Ok(base)
        }
    }

    fn named(input: ParseStream) -> Result<Self> {
        let pat = Pat::atom(input)?;

        if let PatKind::Ident { name } = pat.kind {
            if let Ok(_) = input.parse::<TAt>() {
                let sub = input.parse::<Pat>()?;

                return Ok(Pat {
                    span: pat.span.merge(sub.span),
                    kind: PatKind::Named {
                        name,
                        pat: Box::new(sub),
                    },
                });
            }
        }

        Ok(pat)
    }

    fn atom(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let kind = if let Ok(_) = input.parse::<TLParen>() {
            let mut pats = Vec::new();
            let mut tuple = false;

            while !input.is_empty() && !input.peek::<TRParen>() {
                pats.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                    tuple = true;
                }
            }

            input.parse::<TRParen>()?;

            if pats.len() == 1 && !tuple {
                PatKind::Parens {
                    inner: Box::new(pats.pop().unwrap()),
                }
            } else {
                PatKind::Tuple { pats }
            }
        } else if let Ok(_) = input.parse::<TLBrace>() {
            let mut fields = Vec::new();

            while !input.is_empty() && !input.peek::<TRBrace>() {
                fields.push(input.parse()?);

                if !input.peek::<TRBrace>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRBrace>()?;

            PatKind::Record { fields }
        } else if let Ok(_) = input.parse::<TLBracket>() {
            let mut pats = Vec::new();

            while !input.is_empty() && !input.peek::<TRBracket>() {
                pats.push(input.parse()?);

                if !input.peek::<TRBracket>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRBracket>()?;

            PatKind::Array { pats }
        } else if let Ok(lit) = input.parse::<Literal>() {
            match lit {
                Literal::Int(lit) => PatKind::Int { val: lit.int },
                Literal::Float(lit) => PatKind::Float { bits: lit.float },
                Literal::Char(lit) => PatKind::Char { val: lit.ch },
                Literal::String(lit) => PatKind::Str { val: lit.text },
            }
        } else if let Ok(_) = input.parse::<TWildcard>() {
            PatKind::Wildcard
        } else if let Ok(name) = input.parse::<Ident>() {
            PatKind::Ident { name }
        } else {
            return input.error("expected '(', '{', '_' or an identifier", "E0006");
        };

        Ok(Pat {
            span: start.merge(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>()
            || input.peek::<TLBrace>()
            || input.peek::<Literal>()
            || input.peek::<TWildcard>()
            || input.peek::<Ident>()
    }
}

impl<T: Parse> Parse for RecordField<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;

        if let Ok(_) = input.parse::<TColon>() {
            let val = input.parse()?;

            Ok(RecordField::Field { name, val })
        } else {
            Ok(RecordField::Pun { name })
        }
    }
}

impl Parse for Guarded {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TEquals>() {
            let expr = input.parse()?;

            Ok(Guarded::Unconditional(expr))
        } else {
            let mut guards = Vec::new();

            while input.peek::<TBar>() {
                guards.push(input.parse()?);
            }

            Ok(Guarded::Guarded(guards))
        }
    }
}

impl Guarded {
    fn case(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TArrow>() {
            let expr = input.parse()?;

            Ok(Guarded::Unconditional(expr))
        } else {
            let mut guards = Vec::new();

            while input.peek::<TBar>() {
                guards.push(GuardedExpr::case(input)?);
            }

            Ok(Guarded::Guarded(guards))
        }
    }
}

impl Parse for GuardedExpr {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TBar>()?;
        let guard = Expr::infix(input)?;
        let _ = input.parse::<TEquals>()?;
        let val = input.parse()?;

        Ok(GuardedExpr {
            span: start.merge(input.prev_span()),
            guard,
            val,
        })
    }
}

impl GuardedExpr {
    fn case(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TBar>()?;
        let guard = Expr::infix(input)?;
        let _ = input.parse::<TArrow>()?;
        let val = input.parse()?;

        Ok(GuardedExpr {
            span: start.merge(input.prev_span()),
            guard,
            val,
        })
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr = Expr::assign(input)?;

        if let Ok(_) = input.parse::<TDblColon>() {
            let ty = input.parse()?;

            Ok(Expr {
                span: expr.span.merge(input.prev_span()),
                kind: ExprKind::Typed {
                    expr: Box::new(expr),
                    ty,
                },
            })
        } else {
            Ok(expr)
        }
    }
}

impl Expr {
    fn assign(input: ParseStream) -> Result<Self> {
        let lhs = Self::infix(input)?;

        if let Ok(_) = input.parse::<TEquals>() {
            let rhs = Self::assign(input)?;

            Ok(Expr {
                span: lhs.span.merge(rhs.span),
                kind: ExprKind::Assign {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            })
        } else {
            Ok(lhs)
        }
    }

    fn infix(input: ParseStream) -> Result<Self> {
        return or(input);

        fn or(input: ParseStream) -> Result<Expr> {
            let mut lhs = and(input)?;

            loop {
                if let Ok(_) = input.parse::<TOr>() {
                    let rhs = and(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Or,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn and(input: ParseStream) -> Result<Expr> {
            let mut lhs = bit_or(input)?;

            loop {
                if let Ok(_) = input.parse::<TAnd>() {
                    let rhs = bit_or(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::And,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn bit_or(input: ParseStream) -> Result<Expr> {
            let mut lhs = bit_xor(input)?;

            loop {
                if let Ok(_) = input.parse::<TBitOr>() {
                    let rhs = bit_xor(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::BitOr,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn bit_xor(input: ParseStream) -> Result<Expr> {
            let mut lhs = bit_and(input)?;

            loop {
                if let Ok(_) = input.parse::<TBitXor>() {
                    let rhs = bit_and(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::BitXor,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn bit_and(input: ParseStream) -> Result<Expr> {
            let mut lhs = eq(input)?;

            loop {
                if let Ok(_) = input.parse::<TEq>() {
                    let rhs = eq(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::BitAnd,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn eq(input: ParseStream) -> Result<Expr> {
            let mut lhs = cmp(input)?;

            loop {
                if let Ok(_) = input.parse::<TEq>() {
                    let rhs = cmp(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Eq,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else if let Ok(_) = input.parse::<TNe>() {
                    let rhs = cmp(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Ne,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn cmp(input: ParseStream) -> Result<Expr> {
            let mut lhs = shift(input)?;

            loop {
                if let Ok(_) = input.parse::<TLe>() {
                    let rhs = shift(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Le,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else if let Ok(_) = input.parse::<TGe>() {
                    let rhs = shift(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Ge,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else if let Ok(_) = input.parse::<TLt>() {
                    let rhs = shift(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Lt,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else if let Ok(_) = input.parse::<TGt>() {
                    let rhs = shift(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Gt,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn shift(input: ParseStream) -> Result<Expr> {
            let mut lhs = add(input)?;

            loop {
                if let Ok(_) = input.parse::<TShl>() {
                    let rhs = add(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Shl,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else if let Ok(_) = input.parse::<TShr>() {
                    let rhs = add(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Shr,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn add(input: ParseStream) -> Result<Expr> {
            let mut lhs = mul(input)?;

            loop {
                if let Ok(_) = input.parse::<TAdd>() {
                    let rhs = mul(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Add,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else if input.peek::<TSub>() && !input.peek::<TArrow>() {
                    let _ = input.parse::<TSub>()?;
                    let rhs = mul(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Sub,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }

        fn mul(input: ParseStream) -> Result<Expr> {
            let mut lhs = Expr::app(input)?;

            loop {
                if let Ok(_) = input.parse::<TMul>() {
                    let rhs = Expr::app(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Mul,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else if let Ok(_) = input.parse::<TDiv>() {
                    let rhs = Expr::app(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Div,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else if let Ok(_) = input.parse::<TRem>() {
                    let rhs = Expr::app(input)?;

                    lhs = Expr {
                        span: lhs.span.merge(rhs.span),
                        kind: ExprKind::Infix {
                            op: InfixOp::Rem,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    };
                } else {
                    return Ok(lhs);
                }
            }
        }
    }

    fn app(input: ParseStream) -> Result<Self> {
        let base = Expr::prefix(input)?;
        let mut args = Vec::new();

        while !input.is_empty() && Expr::peek(input) {
            args.push(Expr::prefix(input)?);
        }

        if args.is_empty() {
            Ok(base)
        } else {
            Ok(Expr {
                span: base.span.merge(input.prev_span()),
                kind: ExprKind::App {
                    base: Box::new(base),
                    args,
                },
            })
        }
    }

    fn prefix(input: ParseStream) -> Result<Self> {
        // let start = input.span();
        //
        // if input.peek::<TNeg>() && !input.peek::<TArrow>() {
        //     let _ = input.parse::<TNeg>()?;
        //     let rhs = Expr::prefix(input)?;
        //
        //     Ok(Expr {
        //         span: start.merge(input.prev_span()),
        //         kind: ExprKind::Prefix {
        //             op: PrefixOp::Neg,
        //             rhs: Box::new(rhs),
        //         },
        //     })
        // } else if let Ok(_) = input.parse::<TNot>() {
        //     let rhs = Expr::prefix(input)?;
        //
        //     Ok(Expr {
        //         span: start.merge(input.prev_span()),
        //         kind: ExprKind::Prefix {
        //             op: PrefixOp::Not,
        //             rhs: Box::new(rhs),
        //         },
        //     })
        // } else if let Ok(_) = input.parse::<TBitNot>() {
        //     let rhs = Expr::prefix(input)?;
        //
        //     Ok(Expr {
        //         span: start.merge(input.prev_span()),
        //         kind: ExprKind::Prefix {
        //             op: PrefixOp::BitNot,
        //             rhs: Box::new(rhs),
        //         },
        //     })
        // } else {
        Expr::postfix(input)
        // }
    }

    fn postfix(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::atom(input)?;

        while !input.is_empty() {
            if let Ok(_) = input.parse::<TDot>() {
                let field = if let Ok(name) = input.parse::<Ident>() {
                    name
                } else {
                    let lit = input.parse::<IntLiteral>()?;

                    Ident {
                        span: lit.span,
                        symbol: Symbol::new(lit.int.to_string()),
                    }
                };

                expr = Expr {
                    span: expr.span.merge(input.prev_span()),
                    kind: ExprKind::Field {
                        base: Box::new(expr),
                        field,
                    },
                };
            } else if let Ok(_) = input.parse::<TLBracket>() {
                let index = input.parse()?;
                let _ = input.parse::<TRBracket>()?;

                expr = Expr {
                    span: expr.span.merge(input.prev_span()),
                    kind: ExprKind::Index {
                        base: Box::new(expr),
                        index,
                    },
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn atom(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let kind = if let Ok(_) = input.parse::<TLParen>() {
            let mut exprs = Vec::new();
            let mut tuple = false;

            while !input.is_empty() && !input.peek::<TRParen>() {
                exprs.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                    tuple = true;
                }
            }

            input.parse::<TRParen>()?;

            if exprs.len() == 1 && !tuple {
                ExprKind::Parens {
                    inner: Box::new(exprs.pop().unwrap()),
                }
            } else {
                ExprKind::Tuple { exprs }
            }
        } else if let Ok(_) = input.parse::<TLBrace>() {
            let mut fields = Vec::new();

            while !input.is_empty() && !input.peek::<TRBrace>() {
                fields.push(input.parse()?);

                if !input.peek::<TRBrace>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRBrace>()?;

            ExprKind::Record { fields }
        } else if let Ok(_) = input.parse::<TLBracket>() {
            let mut exprs = Vec::new();

            while !input.is_empty() && !input.peek::<TRBracket>() {
                exprs.push(input.parse()?);

                if !input.peek::<TRBracket>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRBracket>()?;

            ExprKind::Array { exprs }
        } else if let Ok(_) = input.parse::<TQmark>() {
            let name = input.parse()?;

            ExprKind::Hole { name }
        } else if let Ok(lit) = input.parse::<Literal>() {
            match lit {
                Literal::Int(lit) => ExprKind::Int { val: lit.int },
                Literal::Float(lit) => ExprKind::Float { bits: lit.float },
                Literal::Char(lit) => ExprKind::Char { val: lit.ch },
                Literal::String(lit) => ExprKind::Str { val: lit.text },
            }
        } else if let Ok(_) = input.parse::<TLet>() {
            let _ = input.parse::<LytStart>()?;
            let mut bindings = Vec::new();

            while !input.is_empty() && !input.peek::<LytEnd>() {
                bindings.push(input.parse()?);

                if !input.peek::<LytEnd>() {
                    input.parse::<LytSep>()?;
                }
            }

            let _ = input.parse::<LytEnd>()?;
            let _ = input.parse::<TIn>()?;
            let body = input.parse()?;

            ExprKind::Let { bindings, body }
        } else if let Ok(_) = input.parse::<TIf>() {
            let cond = input.parse()?;
            let _ = input.parse::<TThen>()?;
            let then = input.parse()?;
            let _ = input.parse::<TElse>()?;
            let else_ = input.parse()?;

            ExprKind::If { cond, then, else_ }
        } else if let Ok(_) = input.parse::<TCase>() {
            let mut pred = vec![input.parse()?];

            while !input.is_empty() && input.peek::<TComma>() {
                input.parse::<TComma>()?;
                pred.push(input.parse()?);
            }

            let _ = input.parse::<TOf>()?;
            let _ = input.parse::<LytStart>()?;
            let mut arms = Vec::new();

            while !input.is_empty() && !input.peek::<LytEnd>() {
                arms.push(input.parse()?);

                if !input.peek::<LytEnd>() {
                    input.parse::<LytSep>()?;
                }
            }

            input.parse::<LytEnd>()?;

            ExprKind::Case { pred, arms }
        } else if let Ok(_) = input.parse::<TLoop>() {
            let body = input.parse()?;

            ExprKind::Loop { body }
        } else if let Ok(_) = input.parse::<TWhile>() {
            let cond = input.parse()?;
            let _ = input.parse::<TDo>()?;
            let body = input.parse()?;

            ExprKind::While { cond, body }
        } else if let Ok(_) = input.parse::<TBreak>() {
            ExprKind::Break {}
        } else if let Ok(_) = input.parse::<TNext>() {
            ExprKind::Next {}
        } else if let Ok(_) = input.parse::<TDo>() {
            let block = input.parse()?;

            ExprKind::Do { block }
        } else if let Ok(_) = input.parse::<TReturn>() {
            let val = input.parse()?;

            ExprKind::Return { val }
        } else if input.peek::<Ident>() {
            ExprKind::Ident {
                name: input.parse()?,
            }
        } else {
            return input.error("expected '(', '{', '[', '?', 'let', 'if', 'case', 'loop', 'while', 'break', 'next', 'do', 'return', a literal or an identifier", "E0006");
        };

        Ok(Expr {
            span: start.merge(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>()
            || input.peek::<TLBrace>()
            // || (input.peek::<TNeg>() && !input.peek::<TArrow>())
            // || input.peek::<TNot>()
            // || input.peek::<TBitNot>()
            || input.peek::<TQmark>()
            || input.peek::<Ident>()
            || input.peek::<Literal>()
    }
}

impl Parse for Block {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<LytStart>()?;
        let mut stmts = Vec::new();

        while !input.is_empty() && !input.peek::<LytEnd>() {
            stmts.push(input.parse::<Stmt>()?);

            if !input.peek::<LytEnd>() {
                input.parse::<LytSep>()?;
            }
        }

        input.parse::<LytEnd>()?;

        Ok(Block {
            span: start.merge(input.prev_span()),
            stmts,
        })
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let fork = input.fork();
        let kind = if fork.parse::<Pat>().is_ok() && fork.peek::<TLeftArrow>() {
            let pat = input.parse()?;
            let _ = input.parse::<TLeftArrow>()?;
            let val = input.parse()?;

            StmtKind::Bind { pat, val }
        } else {
            let expr = input.parse()?;

            StmtKind::Discard { expr }
        };

        Ok(Stmt {
            span: start.merge(input.prev_span()),
            kind,
        })
    }
}

impl Parse for LetBinding {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let kind = if input.peek::<Ident>() && input.peek2::<TDblColon>() {
            let name = input.parse()?;
            let _ = input.parse::<TDblColon>()?;
            let ty = input.parse()?;

            LetBindingKind::Type { name, ty }
        } else {
            let pat = input.parse()?;
            let _ = input.parse::<TEquals>()?;
            let val = input.parse()?;

            LetBindingKind::Value { pat, val }
        };

        Ok(LetBinding {
            span: start.merge(input.prev_span()),
            kind,
        })
    }
}

impl Parse for CaseArm {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut pats = vec![input.parse()?];

        while !input.is_empty() && input.peek::<TComma>() {
            input.parse::<TComma>()?;
            pats.push(input.parse()?);
        }

        let val = Guarded::case(input)?;

        Ok(CaseArm {
            span: start.merge(input.prev_span()),
            pats,
            val,
        })
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let ty = Type::infix(input)?;

        if let Ok(_) = input.parse::<TDblColon>() {
            let kind = input.parse()?;

            Ok(Type {
                span: ty.span.merge(input.prev_span()),
                kind: TypeKind::Kinded {
                    ty: Box::new(ty),
                    kind,
                },
            })
        } else {
            Ok(ty)
        }
    }
}

impl Type {
    fn infix(input: ParseStream) -> Result<Self> {
        let save = input.cursor();
        let start = input.span();
        let ty = Type::app(input)?;

        if let Ok(_) = input.parse::<TArrow>() {
            let ret = input.parse()?;

            Ok(Type {
                span: start.merge(input.prev_span()),
                kind: TypeKind::Func {
                    params: match ty.kind {
                        TypeKind::Parens { inner } => vec![*inner],
                        TypeKind::Tuple { tys } => tys,
                        _ => vec![ty],
                    },
                    ret,
                },
            })
        } else if let Ok(_) = input.parse::<TFatArrow>() {
            input.restore(save);

            let cs = input.parse()?;
            let _ = input.parse::<TFatArrow>()?;
            let ty = input.parse()?;

            Ok(Type {
                span: start.merge(input.prev_span()),
                kind: TypeKind::Cons { cs, ty },
            })
        } else {
            Ok(ty)
        }
    }

    fn app(input: ParseStream) -> Result<Self> {
        let base = Type::atom(input)?;
        let mut args = Vec::new();

        while !input.is_empty() && Type::peek(input) {
            args.push(Type::atom(input)?);
        }

        if args.is_empty() {
            Ok(base)
        } else {
            Ok(Type {
                span: base.span.merge(input.prev_span()),
                kind: TypeKind::App {
                    base: Box::new(base),
                    args,
                },
            })
        }
    }

    fn atom(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let kind = if let Ok(_) = input.parse::<TLParen>() {
            let mut tys = Vec::new();
            let mut tuple = false;

            while !input.is_empty() && !input.peek::<TRParen>() {
                tys.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                    tuple = true;
                }
            }

            input.parse::<TRParen>()?;

            if tys.len() == 1 && !tuple {
                TypeKind::Parens {
                    inner: Box::new(tys.pop().unwrap()),
                }
            } else {
                TypeKind::Tuple { tys }
            }
        } else if let Ok(_) = input.parse::<TLBrace>() {
            let row = input.parse()?;
            let _ = input.parse::<TRBrace>()?;

            TypeKind::Record { row }
        } else if let Ok(_) = input.parse::<TQmark>() {
            let name = input.parse()?;

            TypeKind::Hole { name }
        } else if let Ok(_) = input.parse::<TForall>() {
            let mut vars = Vec::new();

            while !input.peek::<TDot>() {
                vars.push(input.parse()?);
            }

            let _ = input.parse::<TDot>()?;
            let ret = input.parse()?;

            TypeKind::Forall { vars, ret }
        } else if let Ok(name) = input.parse::<Ident>() {
            if let Ok(_) = input.parse::<TDot>() {
                let module = name;
                let name = input.parse()?;

                TypeKind::Qual { module, name }
            } else {
                TypeKind::Ident { name }
            }
        } else {
            return input.error("expected '(', '{', '?', 'forall' or an identifier", "E0006");
        };

        Ok(Type {
            span: start.merge(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>()
            || input.peek::<TLBrace>()
            || input.peek::<TQmark>()
            || input.peek::<Ident>()
    }
}

impl Parse for Row {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut fields = Vec::new();

        while !input.is_empty()
            && !input.peek::<TRBrace>()
            && !input.peek::<TRParen>()
            && !input.peek::<TBar>()
        {
            fields.push(input.parse()?);

            if !input.peek::<TRBrace>() && !input.peek::<TRParen>() && !input.peek::<TBar>() {
                input.parse::<TComma>()?;
            }
        }

        let tail = if let Ok(_) = input.parse::<TBar>() {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Row {
            span: start.merge(input.prev_span()),
            fields,
            tail,
        })
    }
}

impl Parse for RowField {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let _ = input.parse::<TDblColon>()?;
        let ty = input.parse()?;

        Ok(RowField {
            span: start.merge(input.prev_span()),
            name,
            ty,
        })
    }
}

impl Parse for TypeVar {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TLParen>() {
            let name = input.parse()?;
            let _ = input.parse::<TDblColon>()?;
            let kind = input.parse()?;
            let _ = input.parse::<TRParen>()?;

            Ok(TypeVar::Kind { name, kind })
        } else {
            let name = input.parse()?;

            Ok(TypeVar::Name { name })
        }
    }
}

impl TypeVar {
    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>() || input.peek::<Ident>()
    }
}

impl Parse for Constraint {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TLParen>() {
            let inner = input.parse()?;
            let _ = input.parse::<TRParen>()?;

            Ok(Constraint::Parens { inner })
        } else {
            let iface = input.parse()?;
            let mut tys = Vec::new();

            while Type::peek(input) {
                tys.push(Type::atom(input)?);
            }

            Ok(Constraint::CS { iface, tys })
        }
    }
}
