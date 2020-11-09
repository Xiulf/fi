use crate::ast::*;
use parser::attr::Attr;
use parser::error::Result;
use parser::layout::{LytEnd, LytSep, LytStart};
use parser::literal::{Literal, StringLiteral};
use parser::parse::{Parse, ParseStream};

parser::token![ident "module" TModule];
parser::token![ident "where" TWhere];
parser::token![ident "import" TImport];
parser::token![ident "hiding" THiding];
parser::token![ident "as" TAs];
parser::token![ident "fn" TFn];
parser::token![ident "alias" TAlias];
parser::token![ident "data" TData];
parser::token![ident "iface" TIface];
parser::token![ident "impl" TImpl];
parser::token![ident "forall" TForall];
parser::token![ident "else" TElse];

parser::token![punct "(" TLParen/1];
parser::token![punct ")" TRParen/1];
parser::token![punct "{" TLBrace/1];
parser::token![punct "}" TRBrace/1];
parser::token![punct "[" TLBracket/1];
parser::token![punct "]" TRBracket/1];

parser::token![punct "," TComma/1];
parser::token![punct "." TDot/1];
parser::token![punct "::" TDblColon/2];
parser::token![punct "->" TArrow/2];
parser::token![punct "=>" TFatArrow/2];
parser::token![punct "=" TEquals/1];
parser::token![punct "|" TBar/1];
parser::token![punct "?" TQmark/1];
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
parser::token![punct "<<" TShl/1];
parser::token![punct ">>" TShr/1];
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
        let name = input.parse()?;
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
            let _ = input.parse::<TEquals>()?;
            let val = input.parse()?;

            Ok(AttrArg::Field(name, val))
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
        let name = input.parse::<Ident>()?;
        let kind = if name
            .symbol
            .chars()
            .next()
            .map(|c| c.is_uppercase())
            .unwrap_or(false)
        {
            ExportKind::Type
        } else {
            ExportKind::Value
        };

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
        let module = input.parse()?;
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
        let alias = if let Ok(_) = input.parse::<TAs>() {
            Some(input.parse()?)
        } else {
            None
        };

        let kind = if name
            .symbol
            .chars()
            .next()
            .map(|c| c.is_uppercase())
            .unwrap_or(false)
        {
            ImportKind::Type
        } else {
            ImportKind::Value
        };

        Ok(Import {
            span: start.merge(input.prev_span()),
            name,
            alias,
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

        let (name, kind) = if let Ok(_) = input.parse::<TFn>() {
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
        } else if let Ok(_) = input.parse::<TIface>() {
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

            let head = IfaceHead {
                span: start.merge(input.prev_span()),
                parent: if parent.is_empty() {
                    None
                } else {
                    Some(parent)
                },
                vars,
            };

            let body = if input.peek::<TWhere>() {
                Some(input.parse()?)
            } else {
                None
            };

            (name, DeclKind::Iface { head, body })
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

            (impls[0].head.iface, DeclKind::ImplChain { impls })
        } else {
            return input.error("expected 'fn', 'alias', 'data', 'iface' or 'impl'", "E0006");
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
            || input.peek::<TFn>()
            || input.peek::<TAlias>()
            || input.peek::<TData>()
            || input.peek::<TIface>()
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

impl Parse for IfaceBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TWhere>()?;
        let _ = input.parse::<LytStart>()?;
        let mut decls = Vec::new();

        while !input.is_empty() && IfaceDecl::peek(input) {
            decls.push(input.parse()?);

            if input.peek::<LytEnd>() {
                break;
            } else {
                input.parse::<LytSep>()?;
            }
        }

        input.parse::<LytEnd>()?;

        Ok(IfaceBody {
            span: start.merge(input.prev_span()),
            decls,
        })
    }
}

impl Parse for IfaceDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let (name, kind) = if let Ok(_) = input.parse::<TFn>() {
            let name = input.parse()?;
            let _ = input.parse::<TDblColon>()?;
            let ty = input.parse()?;

            (name, IfaceDeclKind::FuncTy { ty })
        } else {
            return input.error("expected 'fn'", "E0006");
        };

        Ok(IfaceDecl {
            span: start.merge(input.prev_span()),
            name,
            kind,
        })
    }
}

impl IfaceDecl {
    fn peek(input: ParseStream) -> bool {
        input.peek::<TFn>()
    }
}

impl Parse for Impl {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TImpl>()?;
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
            tys.push(input.parse()?);
        }

        let head = ImplHead {
            span: start.merge(input.prev_span()),
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
                    pats.push(input.parse()?);
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
        let base = Pat::atom(input)?;
        let mut pats = Vec::new();

        if let PatKind::Ident { .. } = &base.kind {
            while Pat::peek(input) {
                pats.push(input.parse()?);
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
}

impl Pat {
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
        } else if let Ok(_) = input.parse::<TWildcard>() {
            PatKind::Wildcard
        } else if let Ok(name) = input.parse::<Ident>() {
            PatKind::Ident { name }
        } else {
            return input.error("expected '(', '_' or an identifier", "E0006");
        };

        Ok(Pat {
            span: start.merge(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>() || input.peek::<TWildcard>() || input.peek::<Ident>()
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

impl Parse for GuardedExpr {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TBar>()?;
        let guard = input.parse()?;
        let _ = input.parse::<TEquals>()?;
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
        let expr = Expr::infix(input)?;

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
    fn infix(input: ParseStream) -> Result<Self> {
        return assign(input);

        fn assign(input: ParseStream) -> Result<Expr> {
            let lhs = or(input)?;

            if let Ok(_) = input.parse::<TEquals>() {
                let rhs = assign(input)?;

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
                } else if let Ok(_) = input.parse::<TSub>() {
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
        let start = input.span();

        if let Ok(_) = input.parse::<TNeg>() {
            let rhs = Expr::prefix(input)?;

            Ok(Expr {
                span: start.merge(input.prev_span()),
                kind: ExprKind::Prefix {
                    op: PrefixOp::Neg,
                    rhs: Box::new(rhs),
                },
            })
        } else if let Ok(_) = input.parse::<TNot>() {
            let rhs = Expr::prefix(input)?;

            Ok(Expr {
                span: start.merge(input.prev_span()),
                kind: ExprKind::Prefix {
                    op: PrefixOp::Not,
                    rhs: Box::new(rhs),
                },
            })
        } else if let Ok(_) = input.parse::<TBitNot>() {
            let rhs = Expr::prefix(input)?;

            Ok(Expr {
                span: start.merge(input.prev_span()),
                kind: ExprKind::Prefix {
                    op: PrefixOp::BitNot,
                    rhs: Box::new(rhs),
                },
            })
        } else {
            Expr::postfix(input)
        }
    }

    fn postfix(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::atom(input)?;

        while !input.is_empty() {
            if let Ok(_) = input.parse::<TDot>() {
                let field = input.parse()?;

                expr = Expr {
                    span: expr.span.merge(input.prev_span()),
                    kind: ExprKind::Field {
                        base: Box::new(expr),
                        field,
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
        } else if let Ok(lit) = input.parse::<Literal>() {
            match lit {
                Literal::Int(lit) => ExprKind::Int { val: lit.int },
                Literal::Float(lit) => ExprKind::Float { bits: lit.float },
                Literal::Char(lit) => ExprKind::Char { val: lit.ch },
                Literal::String(lit) => ExprKind::Str { val: lit.text },
            }
        } else if let Ok(name) = input.parse::<Ident>() {
            ExprKind::Ident { name }
        } else {
            return input.error("expected '(', a literal or an identifier", "E0006");
        };

        Ok(Expr {
            span: start.merge(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<TLParen>()
            || input.peek::<TNeg>()
            || input.peek::<TNot>()
            || input.peek::<TBitNot>()
            || input.peek::<Ident>()
            || input.peek::<Literal>()
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
            if name
                .symbol
                .chars()
                .next()
                .map(|c| c.is_lowercase())
                .unwrap_or(false)
            {
                TypeKind::Var { name }
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
