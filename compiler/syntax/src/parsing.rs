use crate::ast::*;
use parser::parse::{Parse, ParseError, ParseStream, Result};
use parser::token::*;

parser::keywords! {
    TModule, "module"
    TWhere, "where"
    TImport, "import"
    THiding, "hiding"
    TAs, "as"
    TForeign, "foreign"
    TFn, "fn"
    TConst, "const"
    TStatic, "static"
    TInfixl, "infixl"
    TInfixr, "infixr"
    TInfix, "infix"
    TAlias, "alias"
    TData, "data"
    TClass, "class"
    TInstance, "instance"
    TForall, "forall"
    TIf, "if"
    TThen, "then"
    TElse, "else"
    TCase, "case"
    TOf, "of"
    TLet, "let"
    TIn, "in"
    TDo, "do"
}

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
        let _ = input.parse::<TLytStart>()?;
        let mut imports = Vec::new();
        let mut decls = Vec::new();
        let mut parse_decls = true;

        while !input.is_empty() && input.peek::<TImport>() {
            imports.push(input.parse()?);

            if input.peek::<TLytEnd>() {
                parse_decls = false;
                break;
            } else {
                input.parse::<TLytSep>()?;
            }
        }

        while parse_decls && !input.is_empty() && Decl::peek(input) {
            decls.push(input.parse()?);

            if input.peek::<TLytEnd>() {
                break;
            } else {
                input.parse::<TLytSep>()?;
            }
        }

        input.parse::<TLytEnd>()?;

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
            symbol: Symbol::new(parts.into_iter().map(|t| t.to_string()).collect::<Vec<_>>().join(".")),
        })
    }
}

impl Parse for Attribute {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TLBracket>()?;
        let name = input.parse()?;
        let body = if input.peek::<TLParen>() { Some(input.parse()?) } else { None };

        input.parse::<TRBracket>()?;

        Ok(Attribute {
            span: start.merge(input.prev_span()),
            name,
            body,
        })
    }
}

impl Attribute {
    fn peek(input: ParseStream) -> bool {
        input.peek::<TLBracket>()
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
            Err(ParseError {
                span: input.span(),
                expected: "literal or an identifier".into(),
            })
        }
    }
}

impl Parse for Literal {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(TInt { span }) = input.parse() {
            let text = input.cursor().text(span);
            let val = text.parse::<u128>().unwrap();

            Ok(Literal::Int(span, val))
        } else if let Ok(TFloat { span }) = input.parse() {
            let text = input.cursor().text(span);
            let val = text.parse::<f64>().unwrap();

            Ok(Literal::Float(span, val.to_bits()))
        } else if let Ok(TChar { span }) = input.parse() {
            let text = input.cursor().text(span);
            let text = &text[1..text.len() - 1];
            let val = text.parse::<char>().unwrap();

            Ok(Literal::Char(span, val))
        } else if let Ok(TString { span }) = input.parse() {
            let text = input.cursor().text(span);
            let text = &text[1..text.len() - 1];
            // TODO: parse escape sequences

            Ok(Literal::String(span, text.into()))
        } else if let Ok(TRawString { span }) = input.parse() {
            let text = input.cursor().text(span);
            let text = &text[2..text.len() - 1];

            Ok(Literal::String(span, text.into()))
        } else {
            Err(ParseError {
                span: input.span(),
                expected: "integer literal, floating point literal, character literal or string literal".into(),
            })
        }
    }
}

impl Token for Literal {
    fn peek(cursor: parser::buffer::Cursor) -> bool {
        TInt::peek(cursor) || TFloat::peek(cursor) || TChar::peek(cursor) || TString::peek(cursor) || TRawString::peek(cursor)
    }

    fn display() -> &'static str {
        "literal"
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

        let name = if let Ok(TSymbol { span }) = input.parse() {
            let text = input.cursor().text(span);

            Ident {
                span,
                symbol: Symbol::new(&text[1..text.len() - 1]),
            }
        } else {
            Module::parse_name(input)?
        };

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

        let qual = if let Ok(_) = input.parse::<TAs>() { Some(input.parse()?) } else { None };

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
        let name = if let Ok(TSymbol { span }) = input.parse() {
            let text = input.cursor().text(span);

            Ident {
                span,
                symbol: Symbol::new(&text[1..text.len() - 1]),
            }
        } else {
            input.parse::<Ident>()?
        };

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
            let _ = input.parse::<TLytSep>();
        }

        let (name, kind) = if let Ok(_) = input.parse::<TForeign>() {
            let kind = if let Ok(_) = input.parse::<TFn>() {
                ForeignKind::Func
            } else if let Ok(_) = input.parse::<TStatic>() {
                ForeignKind::Static
            } else {
                return Err(ParseError {
                    span: input.span(),
                    expected: "fn or static".into(),
                });
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
        } else if input.peek::<TInfixl>() || input.peek::<TInfixr>() || input.peek::<TInfix>() {
            let assoc = if let Ok(_) = input.parse::<TInfixl>() {
                Assoc::Left
            } else if let Ok(_) = input.parse::<TInfixr>() {
                Assoc::Right
            } else {
                input.parse::<TInfix>()?;
                Assoc::None
            };

            let TInt { span } = input.parse()?;
            let prec = match input.cursor().text(span) {
                | "0" => Prec::Zero,
                | "1" => Prec::One,
                | "2" => Prec::Two,
                | "3" => Prec::Three,
                | "4" => Prec::Four,
                | "5" => Prec::Five,
                | "6" => Prec::Six,
                | "7" => Prec::Seven,
                | "8" => Prec::Eight,
                | "9" => Prec::Nine,
                | _ => {
                    return Err(ParseError {
                        span,
                        expected: "0, 1, 2, 3, 4, 5, 6, 7, 8 or 0".into(),
                    })
                },
            };

            let func = input.parse()?;
            let _ = input.parse::<TAs>()?;
            let op = input.parse::<TSymbol>()?;
            let op_text = input.cursor().text(op.span);
            let op = Ident {
                span: op.span,
                symbol: Symbol::new(&op_text[1..op_text.len() - 1]),
            };

            (op, DeclKind::Fixity { assoc, prec, func })
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

                    while !input.is_empty() && input.peek::<TPipe>() {
                        input.parse::<TPipe>()?;
                        ctors.push(input.parse()?);
                    }

                    Some(ctors)
                } else {
                    None
                };

                DeclKind::Data { head, body }
            };

            (name, kind)
        } else if let Ok(_) = input.parse::<TClass>() {
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

            if let Ok(_) = input.parse::<TPipe>() {
                while !input.is_empty() && (input.peek::<TRArrow>() || input.peek::<Ident>()) {
                    fundeps.push(input.parse()?);

                    if input.peek::<TComma>() {
                        input.bump();
                    } else {
                        break;
                    }
                }
            }

            let head = ClassHead {
                span: start.merge(input.prev_span()),
                parent: if parent.is_empty() { None } else { Some(parent) },
                vars,
                fundeps,
            };

            let body = if input.peek::<TWhere>() { Some(input.parse()?) } else { None };

            (name, DeclKind::Class { head, body })
        } else if input.peek::<TInstance>() {
            let mut impls = vec![input.parse::<Instance>()?];

            while !input.is_empty() && input.peek::<TElse>() {
                let _ = input.parse::<TElse>()?;
                let impl_ = input.parse::<Instance>()?;

                if impl_.head.iface.symbol != impls[0].head.iface.symbol {
                    return Err(ParseError {
                        span: impl_.head.iface.span,
                        expected: "instance chains must all implement the same class".into(),
                    });
                }

                impls.push(impl_);
            }

            (impls[0].head.name, DeclKind::InstanceChain { instances: impls })
        } else {
            return Err(ParseError {
                span: input.span(),
                expected: "foreign, fn, static, const, infixl, infixr, infix, alias, data, class or instance".into(),
            });
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
            || input.peek::<TInfixl>()
            || input.peek::<TInfixr>()
            || input.peek::<TInfix>()
            || input.peek::<TAlias>()
            || input.peek::<TData>()
            || input.peek::<TClass>()
            || input.peek::<TInstance>()
    }
}

impl Parse for DataCtor {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let mut tys = Vec::new();

        while !input.is_empty() && Type::peek(input) {
            tys.push(Type::atom(input)?);
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
        if let Ok(_) = input.parse::<TRArrow>() {
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

            let _ = input.parse::<TRArrow>()?;
            let mut right = vec![input.parse()?];

            while !input.is_empty() && input.peek::<Ident>() {
                right.push(input.parse()?);
            }

            Ok(FunDep::Determines(left, right))
        }
    }
}

impl Parse for ClassBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TWhere>()?;
        let _ = input.parse::<TLytStart>()?;
        let mut decls = Vec::new();

        while !input.is_empty() && ClassDecl::peek(input) {
            decls.push(input.parse()?);

            if input.peek::<TLytEnd>() {
                break;
            } else {
                input.parse::<TLytSep>()?;
            }
        }

        input.parse::<TLytEnd>()?;

        Ok(ClassBody {
            span: start.merge(input.prev_span()),
            decls,
        })
    }
}

impl Parse for ClassDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let (name, kind) = if let Ok(_) = input.parse::<TFn>() {
            let name = input.parse()?;
            let _ = input.parse::<TDblColon>()?;
            let ty = input.parse()?;

            (name, ClassDeclKind::FuncTy { ty })
        } else {
            return Err(ParseError {
                span: input.span(),
                expected: "fn".into(),
            });
        };

        Ok(ClassDecl {
            span: start.merge(input.prev_span()),
            name,
            kind,
        })
    }
}

impl ClassDecl {
    fn peek(input: ParseStream) -> bool {
        input.peek::<TFn>()
    }
}

impl Parse for Instance {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TInstance>()?;
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

        let head = InstanceHead {
            span: start.merge(input.prev_span()),
            name,
            cs: if cs.is_empty() { None } else { Some(cs) },
            iface,
            tys,
        };

        let body = if input.peek::<TWhere>() { Some(input.parse()?) } else { None };

        Ok(Instance {
            span: start.merge(input.prev_span()),
            head,
            body,
        })
    }
}

impl Parse for InstanceBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TWhere>()?;
        let _ = input.parse::<TLytStart>()?;
        let mut decls = Vec::new();

        while !input.is_empty() && InstanceDecl::peek(input) {
            decls.push(input.parse()?);

            if input.peek::<TLytEnd>() {
                break;
            } else {
                input.parse::<TLytSep>()?;
            }
        }

        input.parse::<TLytEnd>()?;

        Ok(InstanceBody {
            span: start.merge(input.prev_span()),
            decls,
        })
    }
}

impl Parse for InstanceDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let (name, kind) = if let Ok(_) = input.parse::<TFn>() {
            let name = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TDblColon>() {
                let ty = input.parse()?;

                InstanceDeclKind::FuncTy { ty }
            } else {
                let mut pats = Vec::new();

                while !input.is_empty() && Pat::peek(input) {
                    pats.push(Pat::atom(input)?);
                }

                let val = input.parse()?;

                InstanceDeclKind::Func { pats, val }
            };

            (name, kind)
        } else {
            return Err(ParseError {
                span: input.span(),
                expected: "fn".into(),
            });
        };

        Ok(InstanceDecl {
            span: start.merge(input.prev_span()),
            name,
            kind,
        })
    }
}

impl InstanceDecl {
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
                kind: PatKind::Typed { pat: Box::new(pat), ty },
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
                    kind: PatKind::Named { name, pat: Box::new(sub) },
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
                | Literal::Int(_, val) => PatKind::Int { val },
                | Literal::Float(_, bits) => PatKind::Float { bits },
                | Literal::Char(_, val) => PatKind::Char { val },
                | Literal::String(_, val) => PatKind::Str { val },
            }
        } else if let Ok(_) = input.parse::<TUnderscore>() {
            PatKind::Wildcard
        } else if let Ok(name) = input.parse::<Ident>() {
            PatKind::Ident { name }
        } else {
            return Err(ParseError {
                span: input.span(),
                expected: "(, {, _ or an identifier".into(),
            });
        };

        Ok(Pat {
            span: start.merge(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>() || input.peek::<TLBrace>() || input.peek::<Literal>() || input.peek::<TUnderscore>() || input.peek::<Ident>()
    }
}

impl<T: Parse> Parse for RecordField<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;

        if input.peek::<TOperator>() {
            if input.cursor().text(input.span()) == ":" {
                let _ = input.parse::<TOperator>()?;
                let val = input.parse()?;

                Ok(RecordField::Field { name, val })
            } else {
                Ok(RecordField::Pun { name })
            }
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

            while input.peek::<TPipe>() {
                guards.push(input.parse()?);
            }

            Ok(Guarded::Guarded(guards))
        }
    }
}

impl Guarded {
    fn case(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TRArrow>() {
            let expr = input.parse()?;

            Ok(Guarded::Unconditional(expr))
        } else {
            let mut guards = Vec::new();

            while input.peek::<TPipe>() {
                guards.push(GuardedExpr::case(input)?);
            }

            Ok(Guarded::Guarded(guards))
        }
    }
}

impl Parse for GuardedExpr {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TPipe>()?;
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
        let _ = input.parse::<TPipe>()?;
        let guard = Expr::infix(input)?;
        let _ = input.parse::<TRArrow>()?;
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
                kind: ExprKind::Typed { expr: Box::new(expr), ty },
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
        let lhs = Expr::app(input)?;

        if let Ok(TOperator { span }) = input.parse::<TOperator>() {
            let op = Ident {
                span,
                symbol: Symbol::new(input.cursor().text(span)),
            };

            let rhs = Expr::infix(input)?;

            Ok(Expr {
                span: lhs.span.merge(input.prev_span()),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            })
        } else {
            Ok(lhs)
        }
    }

    fn app(input: ParseStream) -> Result<Self> {
        let mut base = Expr::prefix(input)?;

        while !input.is_empty() && Expr::peek(input) {
            let arg = Expr::prefix(input)?;

            base = Expr {
                span: base.span.merge(input.prev_span()),
                kind: ExprKind::App {
                    base: Box::new(base),
                    arg: Box::new(arg),
                },
            };
        }

        Ok(base)
    }

    fn prefix(input: ParseStream) -> Result<Self> {
        Expr::postfix(input)
    }

    fn postfix(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::atom(input)?;

        while !input.is_empty() {
            if let Ok(_) = input.parse::<TDot>() {
                let field = if let Ok(name) = input.parse::<Ident>() {
                    name
                } else {
                    let TInt { span } = input.parse::<TInt>()?;
                    let text = input.cursor().text(span);

                    Ident {
                        span,
                        symbol: Symbol::new(text),
                    }
                };

                expr = Expr {
                    span: expr.span.merge(input.prev_span()),
                    kind: ExprKind::Field { base: Box::new(expr), field },
                };
            } else if let Ok(_) = input.parse::<TLBracket>() {
                let index = input.parse()?;
                let _ = input.parse::<TRBracket>()?;

                expr = Expr {
                    span: expr.span.merge(input.prev_span()),
                    kind: ExprKind::Index { base: Box::new(expr), index },
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
                | Literal::Int(_, val) => ExprKind::Int { val },
                | Literal::Float(_, bits) => ExprKind::Float { bits },
                | Literal::Char(_, val) => ExprKind::Char { val },
                | Literal::String(_, val) => ExprKind::Str { val },
            }
        } else if let Ok(_) = input.parse::<TLet>() {
            let _ = input.parse::<TLytStart>()?;
            let mut bindings = Vec::new();

            while !input.is_empty() && !input.peek::<TLytEnd>() {
                bindings.push(input.parse()?);

                if !input.peek::<TLytEnd>() {
                    input.parse::<TLytSep>()?;
                }
            }

            let _ = input.parse::<TLytEnd>()?;
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
            let _ = input.parse::<TLytStart>()?;
            let mut arms = Vec::new();

            while !input.is_empty() && !input.peek::<TLytEnd>() {
                arms.push(input.parse()?);

                if !input.peek::<TLytEnd>() {
                    input.parse::<TLytSep>()?;
                }
            }

            input.parse::<TLytEnd>()?;

            ExprKind::Case { pred, arms }
        } else if let Ok(_) = input.parse::<TDo>() {
            let block = input.parse()?;

            ExprKind::Do { block }
        } else if let Ok(TSymbol { span }) = input.parse() {
            let text = input.cursor().text(span);
            let name = Ident {
                span,
                symbol: Symbol::new(&text[1..text.len() - 1]),
            };

            ExprKind::Ident { name }
        } else if input.peek::<Ident>() {
            ExprKind::Ident { name: input.parse()? }
        } else {
            return Err(ParseError {
                span: input.span(),
                expected: "(, {, [, ?, let, if, case, do, a literal, a symbol or an identifier".into(),
            });
        };

        Ok(Expr {
            span: start.merge(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>()
            || input.peek::<TLBrace>()
            || input.peek::<TQmark>()
            || input.peek::<TSymbol>()
            || input.peek::<Ident>()
            || input.peek::<Literal>()
    }
}

impl Parse for Block {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TLytStart>()?;
        let mut stmts = Vec::new();

        while !input.is_empty() && !input.peek::<TLytEnd>() {
            stmts.push(input.parse::<Stmt>()?);

            if !input.peek::<TLytEnd>() {
                input.parse::<TLytSep>()?;
            }
        }

        input.parse::<TLytEnd>()?;

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
        let kind = if fork.parse::<Pat>().is_ok() && fork.peek::<TLArrow>() {
            let pat = input.parse()?;
            let _ = input.parse::<TLArrow>()?;
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
                kind: TypeKind::Kinded { ty: Box::new(ty), kind },
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

        if let Ok(_) = input.parse::<TRArrow>() {
            let ret = Type::infix(input)?;

            Ok(Type {
                span: start.merge(input.prev_span()),
                kind: TypeKind::Func {
                    param: Box::new(ty),
                    ret: Box::new(ret),
                },
            })
        } else if let Ok(_) = input.parse::<TFatArrow>() {
            input.restore(save);

            let cs = input.parse()?;
            let _ = input.parse::<TFatArrow>()?;
            let ty = Type::infix(input)?;

            Ok(Type {
                span: start.merge(input.prev_span()),
                kind: TypeKind::Cons { cs, ty: Box::new(ty) },
            })
        } else {
            Ok(ty)
        }
    }

    fn app(input: ParseStream) -> Result<Self> {
        let mut base = Type::atom(input)?;

        while !input.is_empty() && Type::peek(input) {
            let arg = Type::atom(input)?;

            base = Type {
                span: base.span.merge(input.prev_span()),
                kind: TypeKind::App {
                    base: Box::new(base),
                    arg: Box::new(arg),
                },
            };
        }

        Ok(base)
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
            return Err(ParseError {
                span: input.span(),
                expected: "(, {, ?, forall or an identifier".into(),
            });
        };

        Ok(Type {
            span: start.merge(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>() || input.peek::<TLBrace>() || input.peek::<TQmark>() || input.peek::<Ident>()
    }
}

impl Parse for Row {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut fields = Vec::new();

        while !input.is_empty() && !input.peek::<TRBrace>() && !input.peek::<TRParen>() && !input.peek::<TPipe>() {
            fields.push(input.parse()?);

            if !input.peek::<TRBrace>() && !input.peek::<TRParen>() && !input.peek::<TPipe>() {
                input.parse::<TComma>()?;
            }
        }

        let tail = if let Ok(_) = input.parse::<TPipe>() { Some(input.parse()?) } else { None };

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

            Ok(Constraint::CS { class: iface, tys })
        }
    }
}
