use crate::ast::*;
use parser::error::Result;
use parser::layout::{LytEnd, LytSep, LytStart};
use parser::literal::Literal;
use parser::parse::{Parse, ParseStream};

parser::token![ident "module" TModule];
parser::token![ident "where" TWhere];
parser::token![ident "import" TImport];
parser::token![ident "hiding" THiding];
parser::token![ident "as" TAs];
parser::token![ident "fn" TFn];
parser::token![ident "alias" TAlias];
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

impl Parse for Module {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
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
            name,
            exports,
            imports,
            decls,
        })
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

                while !input.is_empty() && !input.peek::<TEquals>() {
                    vars.push(input.parse()?);
                }

                let _ = input.parse::<TEquals>()?;
                let ty = input.parse()?;

                DeclKind::Alias { vars, ty }
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
            return input.error("expected 'fn', 'alias', 'iface' or 'impl'", "E0006");
        };

        Ok(Decl {
            span: start.merge(input.prev_span()),
            name,
            kind,
        })
    }
}

impl Decl {
    fn peek(input: ParseStream) -> bool {
        input.peek::<TFn>()
            || input.peek::<TAlias>()
            || input.peek::<TIface>()
            || input.peek::<TImpl>()
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
        let mut args = Vec::new();

        while Pat::peek(input) {
            args.push(input.parse()?);
        }

        if args.is_empty() {
            Ok(base)
        } else {
            Ok(Pat {
                span: base.span.merge(input.prev_span()),
                kind: PatKind::App {
                    base: Box::new(base),
                    args,
                },
            })
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
        let base = Expr::infix(input)?;
        let mut args = Vec::new();

        while Expr::peek(input) {
            args.push(input.parse()?);
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
}

impl Expr {
    fn infix(input: ParseStream) -> Result<Self> {
        Expr::prefix(input)
    }

    fn prefix(input: ParseStream) -> Result<Self> {
        Expr::postfix(input)
    }

    fn postfix(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::atom(input)?;

        while !input.is_empty() {
            if let Ok(_) = input.parse::<TDblColon>() {
                let ty = input.parse()?;

                expr = Expr {
                    span: expr.span.merge(input.prev_span()),
                    kind: ExprKind::Typed {
                        expr: Box::new(expr),
                        ty,
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
        input.peek::<TLParen>() || input.peek::<Ident>() || input.peek::<Literal>()
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let base = Type::infix(input)?;
        let mut args = Vec::new();

        while Type::peek(input) {
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
}

impl Type {
    fn infix(input: ParseStream) -> Result<Self> {
        let save = input.cursor();
        let start = input.span();
        let ty = Type::atom(input)?;

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
            TypeKind::Hole
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
