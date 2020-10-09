use crate::ast::*;
use parser::attr::Attr;
use parser::error::Result;
use parser::parse::{Parse, ParseStream};

parser::token![ident "module" TModule];
parser::token![ident "as" TAs];
parser::token![ident "import" TImport];
parser::token![ident "hiding" THiding];
parser::token![ident "end" TEnd];
parser::token![ident "where" TWhere];
parser::token![ident "extern" TExtern];
parser::token![ident "fn" TFn];
parser::token![ident "var" TVar];
parser::token![ident "const" TConst];
parser::token![ident "struct" TStruct];
parser::token![ident "enum" TEnum];
parser::token![ident "do" TDo];
parser::token![ident "mut" TMut];
parser::token![ident "type" TType];
parser::token![ident "if" TIf];
parser::token![ident "else" TElse];
parser::token![ident "match" TMatch];
parser::token![ident "for" TFor];
parser::token![ident "while" TWhile];
parser::token![ident "loop" TLoop];
parser::token![ident "break" TBreak];
parser::token![ident "continue" TContinue];
parser::token![ident "return" TReturn];
parser::token![ident "defer" TDefer];
parser::token![ident "and" TAnd];
parser::token![ident "or" TOr];
parser::token![ident "box" TBox];
parser::token![ident "unbox" TUnbox];

parser::token![punct "(" TLParen/1];
parser::token![punct ")" TRParen/1];
parser::token![punct "[" TLBracket/1];
parser::token![punct "]" TRBracket/1];
parser::token![punct "{" TLBrace/1];
parser::token![punct "}" TRBrace/1];
parser::token![punct "<" TLeft/1];
parser::token![punct ">" TRight/1];

parser::token![punct ":" TColon/1];
parser::token![punct ";" TSemi/1];
parser::token![punct "," TComma/1];
parser::token![punct "." TDot/1];
parser::token![punct "=" TEquals/1];
parser::token![punct "`" TTick/1];
parser::token![punct "->" TArrow/2];
parser::token![punct ".." TDblDot/2];
parser::token![ident "_" TWildcard];
parser::token![punct "@" TAt/1];
parser::token![punct "&" TAmp/1];
parser::token![punct "*" TStar/1];

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

parser::token![punct "-" TNeg/1];
parser::token![punct "!" TNot/1];

impl Parse for Package {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut modules = Vec::new();

        while !input.is_empty() {
            match input.parse::<Module>() {
                Ok(m) => modules.push(m),
                Err(e) => {
                    input.reporter.add(e);

                    while !input.is_empty() && !input.peek::<TModule>() {
                        input.bump();
                    }
                }
            }
        }

        Ok(Package { modules })
    }
}

impl Parse for Module {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TModule>()?;
        let name = Module::parse_name(input)?;
        let exports = if let Ok(_) = input.parse::<TLParen>() {
            let mut exports = Vec::new();

            while !input.is_empty() && !input.peek::<TRParen>() {
                exports.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRParen>()?;

            Exports::Some(exports)
        } else {
            Exports::All
        };

        let mut imports = Vec::new();

        while !input.is_empty() && input.peek::<TImport>() {
            imports.push(input.parse()?);
        }

        let mut items = Vec::new();

        while !input.is_empty() && !input.peek::<TModule>() {
            match input.parse() {
                Ok(item) => items.push(item),
                Err(e) => {
                    input.reporter.add(e);
                    input.bump();

                    while !input.is_empty() && !input.peek::<TModule>() && !Item::peek(input) {
                        input.bump();
                    }
                }
            }
        }

        Ok(Module {
            span: start.to(input.prev_span()),
            name,
            exports,
            imports,
            items,
        })
    }
}

impl Module {
    fn parse_name(input: ParseStream) -> Result<Ident> {
        let mut name = input.parse::<Ident>()?;

        while input.peek::<TDot>()
            && input.peek2::<Ident>()
            && input.span().start.offset == name.span.end.offset
            && input.span().end.offset == input.cursor().bump().span().start.offset
        {
            input.parse::<TDot>()?;

            let part = input.parse::<Ident>()?;

            name.symbol = Symbol::new(format!("{}.{}", name.symbol, part.symbol));
            name.span.end = part.span.end;
        }

        Ok(name)
    }
}

impl Parse for Export {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;

        Ok(Export { name })
    }
}

impl Parse for Import {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TImport>()?;
        let module = Module::parse_name(input)?;
        let alias = if let Ok(_) = input.parse::<TAs>() {
            Some(input.parse()?)
        } else {
            None
        };

        let hiding = input.parse::<THiding>().is_ok();
        let imports = if let Ok(_) = input.parse::<TLParen>() {
            let mut imports = Vec::new();

            while !input.is_empty() && !input.peek::<TRParen>() {
                imports.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRParen>()?;

            Some(imports)
        } else {
            None
        };

        Ok(Import {
            span: start.to(input.prev_span()),
            module,
            alias,
            hiding,
            imports,
        })
    }
}

impl Parse for ImportItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let alias = if let Ok(_) = input.parse::<TAs>() {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(ImportItem {
            span: start.to(input.prev_span()),
            name,
            alias,
        })
    }
}

impl Parse for Attribute {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(attr) = input.parse::<Attr>() {
            Ok(Attribute {
                span: attr.span,
                kind: AttrKind::Doc(attr.text),
            })
        } else {
            let start = input.span();
            let _ = input.parse::<TAt>()?;
            let name = input.parse::<Ident>()?;
            let kind = match &**name.symbol {
                "no_mangle" => AttrKind::NoMangle,
                "lang" => AttrKind::Lang(input.parse()?),
                "intrinsic" => AttrKind::Intrinsic,
                "main" => AttrKind::Main,
                "poly" => AttrKind::Poly,
                "macro" => AttrKind::Macro,
                _ => return input.error_at("unknown attribute", name.span, 0001),
            };

            Ok(Attribute {
                span: start.to(input.prev_span()),
                kind,
            })
        }
    }
}

impl parser::token::Token for Attribute {
    fn peek(cursor: parser::buffer::Cursor) -> bool {
        Attr::peek(cursor) || TAt::peek(cursor)
    }

    fn display() -> &'static str {
        "attribute"
    }
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut attrs = Vec::new();

        while !input.is_empty() && input.peek::<Attribute>() {
            attrs.push(input.parse()?);
        }

        if let Ok(_) = input.parse::<TExtern>() {
            let abi = input.parse()?;
            let name = input.parse()?;
            let _ = input.parse::<TColon>()?;
            let ty = input.parse()?;
            let _ = input.parse::<TSemi>();

            Ok(Item {
                span: start.to(input.prev_span()),
                attrs,
                name,
                kind: ItemKind::Extern { abi, ty },
            })
        } else if let Ok(_) = input.parse::<TFn>() {
            let name = input.parse()?;
            let generics = input.parse()?;
            let _ = input.parse::<TLParen>()?;
            let mut params = Vec::new();

            while !input.is_empty() && !input.peek::<TRParen>() {
                params.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRParen>()?;

            let ret = if let Ok(_) = input.parse::<TArrow>() {
                input.parse()?
            } else {
                Type {
                    span: input.span(),
                    kind: TypeKind::Infer,
                }
            };

            let body = Block::parse(input, TEnd::parse)?;

            Ok(Item {
                span: start.to(input.prev_span()),
                attrs,
                name,
                kind: ItemKind::Func {
                    generics,
                    params,
                    ret,
                    body,
                },
            })
        } else if let Ok(_) = input.parse::<TVar>() {
            let name = input.parse()?;
            let ty = if let Ok(_) = input.parse::<TColon>() {
                input.parse::<Type>()?
            } else {
                Type {
                    span: input.span(),
                    kind: TypeKind::Infer,
                }
            };

            let val = if let Ok(_) = input.parse::<TEquals>() {
                Some(input.parse::<Expr>()?)
            } else {
                None
            };

            let _ = input.parse::<TSemi>();

            Ok(Item {
                span: start.to(input.prev_span()),
                attrs,
                name,
                kind: ItemKind::Var { ty, val },
            })
        } else if let Ok(_) = input.parse::<TConst>() {
            let name = input.parse()?;
            let ty = if let Ok(_) = input.parse::<TColon>() {
                input.parse()?
            } else {
                Type {
                    span: input.span(),
                    kind: TypeKind::Infer,
                }
            };

            let _ = input.parse::<TEquals>()?;
            let val = input.parse()?;
            let _ = input.parse::<TSemi>();

            Ok(Item {
                span: start.to(input.prev_span()),
                attrs,
                name,
                kind: ItemKind::Const { ty, val },
            })
        } else if let Ok(_) = input.parse::<TStruct>() {
            let name = input.parse()?;
            let generics = input.parse()?;
            let mut fields = Vec::new();
            let mut methods = Vec::new();

            while !input.is_empty() && !input.peek::<TEnd>() && !input.peek::<TFn>() {
                fields.push(input.parse()?);
            }

            while !input.is_empty() && input.peek::<TFn>() {
                methods.push(input.parse()?);
            }

            input.parse::<TEnd>()?;

            Ok(Item {
                span: start.to(input.prev_span()),
                attrs,
                name,
                kind: ItemKind::Struct {
                    generics,
                    fields,
                    methods,
                },
            })
        } else if let Ok(_) = input.parse::<TEnum>() {
            let name = input.parse()?;
            let generics = input.parse()?;
            let mut variants = Vec::new();
            let mut methods = Vec::new();

            while !input.is_empty() && !input.peek::<TEnd>() && !input.peek::<TFn>() {
                variants.push(input.parse()?);
            }

            while !input.is_empty() && input.peek::<TFn>() {
                methods.push(input.parse()?);
            }

            input.parse::<TEnd>()?;

            Ok(Item {
                span: start.to(input.prev_span()),
                attrs,
                name,
                kind: ItemKind::Enum {
                    generics,
                    variants,
                    methods,
                },
            })
        } else {
            input.error(
                "expected 'mod', 'extern', 'fn', 'var', 'const', 'struct' or 'enum'",
                0001,
            )
        }
    }
}

impl Item {
    fn peek(input: ParseStream) -> bool {
        input.peek::<Attribute>()
            || input.peek::<TExtern>()
            || input.peek::<TFn>()
            || input.peek::<TVar>()
            || input.peek::<TConst>()
            || input.peek::<TStruct>()
    }
}

impl Parse for Abi {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(lit) = input.parse::<StringLiteral>() {
            match lit.text.to_lowercase().as_str() {
                "c" => Ok(Abi::C),
                _ => input.error_at("invalid abi", lit.span, 0002),
            }
        } else {
            Ok(Abi::None)
        }
    }
}

impl Parse for Generics {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TLeft>() {
            let start = input.prev_span();
            let mut params = Vec::new();

            while !input.is_empty() && !input.peek::<TRight>() {
                params.push(input.parse()?);

                if !input.peek::<TRight>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRight>()?;

            Ok(Generics {
                span: start.to(input.prev_span()),
                params,
            })
        } else {
            Ok(Generics {
                span: input.span(),
                params: Vec::new(),
            })
        }
    }
}

impl Parse for Generic {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;

        Ok(Generic {
            span: start.to(input.prev_span()),
            name,
        })
    }
}

impl Parse for Param {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let ty = if let Ok(_) = input.parse::<TColon>() {
            input.parse()?
        } else {
            Type {
                span: input.span(),
                kind: TypeKind::Infer,
            }
        };

        Ok(Param {
            span: start.to(input.prev_span()),
            name,
            ty,
        })
    }
}

impl Parse for StructField {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let ty = if let Ok(_) = input.parse::<TColon>() {
            input.parse()?
        } else {
            Type {
                span: input.span(),
                kind: TypeKind::Infer,
            }
        };

        Ok(StructField {
            span: start.to(input.prev_span()),
            name,
            ty,
        })
    }
}

impl Parse for EnumVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let fields = if let Ok(_) = input.parse::<TLParen>() {
            let mut fields = Vec::new();

            while !input.is_empty() && !input.peek::<TRParen>() {
                fields.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRParen>()?;

            Some(fields)
        } else {
            None
        };

        Ok(EnumVariant {
            span: start.to(input.prev_span()),
            name,
            fields,
        })
    }
}

impl Parse for Method {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let _ = input.parse::<TFn>()?;
        let name = input.parse()?;
        let generics = input.parse()?;
        let _ = input.parse::<TLParen>()?;
        let mut params = Vec::new();

        while !input.is_empty() && !input.peek::<TRParen>() {
            params.push(input.parse()?);

            if !input.peek::<TRParen>() {
                input.parse::<TComma>()?;
            }
        }

        input.parse::<TRParen>()?;

        let ret = if let Ok(_) = input.parse::<TArrow>() {
            input.parse()?
        } else {
            Type {
                span: input.span(),
                kind: TypeKind::Infer,
            }
        };

        let body = Block::parse(input, TEnd::parse)?;

        Ok(Method {
            span: start.to(input.prev_span()),
            name,
            generics,
            params,
            ret,
            body,
        })
    }
}

impl Block {
    fn parse<T>(input: ParseStream, term: impl Fn(ParseStream) -> Result<T>) -> Result<Self> {
        let mut start = input.prev_span();
        let mut stmts = Vec::new();

        start.start = start.end;
        start.start.offset += 1;
        start.start.line += 1;
        start.start.col = 0;

        while !input.is_empty() && !term(&input.fork()).is_ok() {
            stmts.push(input.parse()?);
        }

        term(input)?;

        Ok(Block {
            span: start.to(input.prev_span()),
            stmts,
        })
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let kind = if Item::peek(input) {
            StmtKind::Item(input.parse()?)
        } else {
            let expr = input.parse()?;

            if let Ok(_) = input.parse::<TSemi>() {
                StmtKind::Semi(expr)
            } else {
                StmtKind::Expr(expr)
            }
        };

        Ok(Stmt {
            span: start.to(input.prev_span()),
            kind,
        })
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        Expr::assign(input)
    }
}

impl Expr {
    fn assign(input: ParseStream) -> Result<Self> {
        let start = input.span();

        if {
            let fork = input.fork();

            Expr::prefix(&fork).is_ok() && fork.parse::<BinOp>().is_ok() && fork.peek::<TEquals>()
        } {
            let lhs = Expr::prefix(input)?;
            let op = input.parse::<BinOp>()?;
            let _ = input.parse::<TEquals>()?;
            let rhs = Expr::infix(input, 1)?;

            Ok(Expr {
                span: start.to(input.prev_span()),
                kind: ExprKind::AssignOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            })
        } else {
            let expr = Expr::infix(input, 1)?;

            if let Ok(_) = input.parse::<TEquals>() {
                let rhs = Expr::assign(input)?;

                Ok(Expr {
                    span: start.to(input.prev_span()),
                    kind: ExprKind::Assign {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    },
                })
            } else {
                Ok(expr)
            }
        }
    }

    fn infix(input: ParseStream, prec_in: usize) -> Result<Self> {
        let start = input.span();
        let mut expr = Expr::prefix(input)?;
        let prec = input.fork().parse::<BinOp>().map(BinOp::prec).unwrap_or(0);

        for prec in (prec_in..=prec).rev() {
            loop {
                let op_prec = input.fork().parse::<BinOp>().map(BinOp::prec).unwrap_or(0);

                if op_prec != prec {
                    break;
                }

                let op = input.parse()?;
                let rhs = Expr::infix(input, prec + 1)?;

                expr = Expr {
                    span: start.to(input.prev_span()),
                    kind: ExprKind::BinOp {
                        op,
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    },
                };
            }
        }

        Ok(expr)
    }

    fn prefix(input: ParseStream) -> Result<Self> {
        let start = input.span();

        if let Ok(op) = input.parse::<UnOp>() {
            let rhs = Expr::prefix(input)?;

            Ok(Expr {
                span: start.to(input.prev_span()),
                kind: ExprKind::UnOp {
                    op,
                    rhs: Box::new(rhs),
                },
            })
        } else if let Ok(_) = input.parse::<TAmp>() {
            let rhs = Expr::prefix(input)?;

            Ok(Expr {
                span: start.to(input.prev_span()),
                kind: ExprKind::Ref {
                    expr: Box::new(rhs),
                },
            })
        } else if let Ok(_) = input.parse::<TBox>() {
            let rhs = Expr::prefix(input)?;

            Ok(Expr {
                span: start.to(input.prev_span()),
                kind: ExprKind::Box {
                    expr: Box::new(rhs),
                },
            })
        } else if let Ok(_) = input.parse::<TUnbox>() {
            let rhs = Expr::prefix(input)?;

            Ok(Expr {
                span: start.to(input.prev_span()),
                kind: ExprKind::Unbox {
                    expr: Box::new(rhs),
                },
            })
        } else {
            Expr::postfix(input)
        }
    }

    fn postfix(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut expr = Expr::atom(input)?;

        while !input.is_empty() {
            if let Ok(_) = input.parse::<TLParen>() {
                let mut args = Vec::new();

                while !input.is_empty() && !input.peek::<TRParen>() {
                    args.push(input.parse()?);

                    if !input.peek::<TRParen>() {
                        input.parse::<TComma>()?;
                    }
                }

                input.parse::<TRParen>()?;

                expr = Expr {
                    span: start.to(input.prev_span()),
                    kind: ExprKind::Call {
                        func: Box::new(expr),
                        args,
                    },
                };
            } else if let Ok(_) = input.parse::<TLBracket>() {
                if let Ok(_) = input.parse::<TDblDot>() {
                    let high = if !input.peek::<TRBracket>() {
                        Some(input.parse()?)
                    } else {
                        None
                    };

                    input.parse::<TRBracket>()?;

                    expr = Expr {
                        span: start.to(input.prev_span()),
                        kind: ExprKind::Slice {
                            list: Box::new(expr),
                            low: None,
                            high,
                        },
                    };
                } else {
                    let index = input.parse()?;

                    if let Ok(_) = input.parse::<TDblDot>() {
                        let high = if !input.peek::<TRBracket>() {
                            Some(input.parse()?)
                        } else {
                            None
                        };

                        input.parse::<TRBracket>()?;

                        expr = Expr {
                            span: start.to(input.prev_span()),
                            kind: ExprKind::Slice {
                                list: Box::new(expr),
                                low: Some(index),
                                high,
                            },
                        };
                    } else {
                        input.parse::<TRBracket>()?;

                        expr = Expr {
                            span: start.to(input.prev_span()),
                            kind: ExprKind::Index {
                                list: Box::new(expr),
                                index,
                            },
                        };
                    }
                }
            } else if let Ok(_) = input.parse::<TDblDot>() {
                let hi = input.parse()?;

                expr = Expr {
                    span: start.to(input.prev_span()),
                    kind: ExprKind::Range {
                        lo: Box::new(expr),
                        hi,
                    },
                };
            } else if let Ok(_) = input.parse::<TDot>() {
                if let Ok(_) = input.parse::<TLParen>() {
                    let ty = input.parse()?;
                    let _ = input.parse::<TRParen>()?;

                    expr = Expr {
                        span: start.to(input.prev_span()),
                        kind: ExprKind::Cast {
                            expr: Box::new(expr),
                            ty,
                        },
                    };
                } else if let Ok(_) = input.parse::<TLeft>() {
                    let mut args = Vec::new();

                    while !input.is_empty() && !input.peek::<TRight>() {
                        args.push(input.parse()?);

                        if !input.peek::<TRight>() {
                            input.parse::<TComma>()?;
                        }
                    }

                    input.parse::<TRight>()?;

                    expr = Expr {
                        span: start.to(input.prev_span()),
                        kind: ExprKind::Apply {
                            expr: Box::new(expr),
                            args,
                        },
                    };
                } else if let Ok(_) = input.parse::<TStar>() {
                    expr = Expr {
                        span: start.to(input.prev_span()),
                        kind: ExprKind::Deref {
                            expr: Box::new(expr),
                        },
                    };
                } else if let Ok(_) = input.parse::<TType>() {
                    expr = Expr {
                        span: start.to(input.prev_span()),
                        kind: ExprKind::TypeOf {
                            expr: Box::new(expr),
                        },
                    };
                } else if let Ok(lit) = input.parse::<IntLiteral>() {
                    expr = Expr {
                        span: start.to(input.prev_span()),
                        kind: ExprKind::Field {
                            obj: Box::new(expr),
                            field: Ident {
                                span: lit.span,
                                symbol: Symbol::new(lit.int.to_string()),
                            },
                        },
                    };
                } else if let Ok(name) = input.parse::<Ident>() {
                    if let Ok(_) = input.parse::<TLParen>() {
                        let mut args = Vec::new();

                        while !input.is_empty() && !input.peek::<TRParen>() {
                            args.push(input.parse()?);

                            if !input.peek::<TRParen>() {
                                input.parse::<TComma>()?;
                            }
                        }

                        input.parse::<TRParen>()?;

                        expr = Expr {
                            span: start.to(input.prev_span()),
                            kind: ExprKind::MethodCall {
                                obj: Box::new(expr),
                                method: name,
                                args,
                            },
                        };
                    } else {
                        expr = Expr {
                            span: start.to(input.prev_span()),
                            kind: ExprKind::Field {
                                obj: Box::new(expr),
                                field: name,
                            },
                        };
                    }
                } else {
                    return input.error(
                        "expected '(', '<', '*', 'type', a number or an identifier",
                        0001,
                    );
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn atom(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let kind = if let Ok(lit) = input.parse() {
            match lit {
                Literal::Int(lit) => ExprKind::Int { val: lit.int },
                Literal::Float(lit) => ExprKind::Float { bits: lit.float },
                Literal::Char(lit) => ExprKind::Char { val: lit.ch },
                Literal::String(lit) => ExprKind::String { val: lit.text },
            }
        } else if let Ok(_) = input.parse::<TTick>() {
            let ty = input.parse()?;
            let _ = input.parse::<TTick>()?;

            ExprKind::Type { ty }
        } else if let Ok(_) = input.parse::<TDo>() {
            ExprKind::Block {
                block: Block::parse(input, TEnd::parse)?,
            }
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
        } else if let Ok(_) = input.parse::<TLParen>() {
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
        } else if let Ok(_) = input.parse::<TIf>() {
            let cond = input.parse()?;
            let _ = input.parse::<TSemi>();
            let then = Block::parse(input, |i| {
                if i.peek::<TElse>() {
                    Ok(())
                } else {
                    i.parse::<TEnd>().map(|_| ())
                }
            })?;

            let else_ = if let Ok(_) = input.parse::<TElse>() {
                Some(Block::parse(input, TEnd::parse)?)
            } else {
                None
            };

            ExprKind::IfElse { cond, then, else_ }
        } else if let Ok(_) = input.parse::<TMatch>() {
            let pred = input.parse()?;
            let mut arms = Vec::new();

            while !input.is_empty() && !input.peek::<TEnd>() {
                arms.push(input.parse()?);
            }

            input.parse::<TEnd>()?;

            ExprKind::Match { pred, arms }
        } else if let Ok(_) = input.parse::<TWhile>() {
            let cond = input.parse()?;
            let _ = input.parse::<TSemi>();
            let body = Block::parse(input, TEnd::parse)?;

            ExprKind::While {
                label: None,
                cond,
                body,
            }
        } else if let Ok(_) = input.parse::<TLoop>() {
            let body = Block::parse(input, TEnd::parse)?;

            ExprKind::Loop { label: None, body }
        } else if let Ok(_) = input.parse::<TBreak>() {
            let label = if let Ok(_) = input.parse::<TColon>() {
                Some(input.parse()?)
            } else {
                None
            };

            let expr = if Expr::peek(input) {
                Some(input.parse()?)
            } else {
                None
            };

            ExprKind::Break { label, expr }
        } else if let Ok(_) = input.parse::<TContinue>() {
            let label = if let Ok(_) = input.parse::<TColon>() {
                Some(input.parse()?)
            } else {
                None
            };

            ExprKind::Continue { label }
        } else if let Ok(_) = input.parse::<TReturn>() {
            let expr = if Expr::peek(input) {
                Some(input.parse()?)
            } else {
                None
            };

            ExprKind::Return { expr }
        } else if let Ok(_) = input.parse::<TDefer>() {
            let expr = input.parse()?;

            ExprKind::Defer { expr }
        } else if let Ok(name) = input.parse() {
            ExprKind::Ident { name }
        } else {
            return input.error("expected '(', '{', '[', 'do', 'if', 'match', 'while', 'loop', 'break', 'continue', 'return', 'defer', a label, a literal or an identifier", 0001);
        };

        Ok(Expr {
            span: start.to(input.prev_span()),
            kind,
        })
    }

    fn peek(input: ParseStream) -> bool {
        input.peek::<Ident>()
            || input.peek::<TLParen>()
            || input.peek::<TLBrace>()
            || input.peek::<TLBracket>()
            || input.peek::<TColon>()
            || input.peek::<Literal>()
            || input.peek::<TNeg>()
            || input.peek::<TNot>()
            || input.peek::<TTick>()
    }
}

impl Parse for InitField {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let _ = input.parse::<TEquals>()?;
        let value = input.parse()?;

        Ok(InitField {
            span: start.to(input.prev_span()),
            name,
            value,
        })
    }
}

impl Parse for Arg {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = if input.peek::<Ident>() && input.peek2::<TEquals>() {
            let name = input.parse::<Ident>()?;
            let _ = input.parse::<TEquals>()?;

            Some(name)
        } else {
            None
        };

        let value = input.parse()?;

        Ok(Arg {
            span: start.to(input.prev_span()),
            name,
            value,
        })
    }
}

impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TShr>() {
            Ok(BinOp::Shr)
        } else if let Ok(_) = input.parse::<TShl>() {
            Ok(BinOp::Shl)
        } else if let Ok(_) = input.parse::<TEq>() {
            Ok(BinOp::Eq)
        } else if let Ok(_) = input.parse::<TNe>() {
            Ok(BinOp::Ne)
        } else if let Ok(_) = input.parse::<TLe>() {
            Ok(BinOp::Le)
        } else if let Ok(_) = input.parse::<TGe>() {
            Ok(BinOp::Ge)
        } else if let Ok(_) = input.parse::<TAdd>() {
            Ok(BinOp::Add)
        } else if let Ok(_) = input.parse::<TSub>() {
            Ok(BinOp::Sub)
        } else if let Ok(_) = input.parse::<TMul>() {
            Ok(BinOp::Mul)
        } else if let Ok(_) = input.parse::<TDiv>() {
            Ok(BinOp::Div)
        } else if let Ok(_) = input.parse::<TRem>() {
            Ok(BinOp::Rem)
        } else if let Ok(_) = input.parse::<TLt>() {
            Ok(BinOp::Lt)
        } else if let Ok(_) = input.parse::<TGt>() {
            Ok(BinOp::Gt)
        } else if let Ok(_) = input.parse::<TBitAnd>() {
            Ok(BinOp::BitAnd)
        } else if let Ok(_) = input.parse::<TBitOr>() {
            Ok(BinOp::BitOr)
        } else if let Ok(_) = input.parse::<TBitXOr>() {
            Ok(BinOp::BitXOr)
        } else if let Ok(_) = input.parse::<TAnd>() {
            Ok(BinOp::And)
        } else if let Ok(_) = input.parse::<TOr>() {
            Ok(BinOp::Or)
        } else {
            input.error("expected 'and', 'or', '+', '-', '*', '/', '%', '<', '<=', '>', '>=', '==', '!=', '&', '|', '^', '<<' or '>>'", 0001)
        }
    }
}

impl BinOp {
    fn prec(self) -> usize {
        match self {
            BinOp::Or => 1,
            BinOp::And => 2,
            BinOp::Eq | BinOp::Ne => 3,
            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => 4,
            BinOp::BitOr => 5,
            BinOp::BitXOr => 6,
            BinOp::BitAnd => 7,
            BinOp::Shl | BinOp::Shr => 8,
            BinOp::Add | BinOp::Sub => 9,
            BinOp::Mul | BinOp::Div | BinOp::Rem => 10,
        }
    }
}

impl Parse for UnOp {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<TNeg>() {
            Ok(UnOp::Neg)
        } else if let Ok(_) = input.parse::<TNot>() {
            Ok(UnOp::Not)
        } else {
            input.error("expected '-' or '!'", 0001)
        }
    }
}

impl Parse for MatchArm {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let pat = input.parse()?;
        let _ = input.parse::<TColon>()?;
        let value = input.parse()?;

        Ok(MatchArm {
            span: start.to(input.prev_span()),
            pat,
            value,
        })
    }
}

impl Parse for Pat {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let kind = if let Ok(_) = input.parse::<TWildcard>() {
            PatKind::Wildcard
        } else if let Ok(mut name) = input.parse::<Ident>() {
            if input.peek::<TLParen>() || input.peek::<TDot>() {
                let module = if let Ok(_) = input.parse::<TDot>() {
                    let m = name;

                    name = input.parse()?;

                    Some(m)
                } else {
                    None
                };

                let mut pats = Vec::new();

                if let Ok(_) = input.parse::<TLParen>() {
                    while !input.is_empty() && !input.peek::<TRParen>() {
                        pats.push(input.parse()?);

                        if !input.peek::<TRParen>() {
                            input.parse::<TComma>()?;
                        }
                    }

                    input.parse::<TRParen>()?;
                }

                PatKind::Ctor { module, name, pats }
            } else if let Ok(_) = input.parse::<TAt>() {
                let inner = input.parse()?;

                PatKind::Bind {
                    name,
                    inner: Some(inner),
                }
            } else {
                PatKind::Bind { name, inner: None }
            }
        } else {
            return input.error("expected '_' or an identifier", 0001);
        };

        Ok(Pat {
            span: start.to(input.prev_span()),
            kind,
        })
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let mut ty = Type::atom(input)?;

        while !input.is_empty() && input.peek::<TLeft>() {
            let _ = input.parse::<TLeft>()?;
            let mut args = Vec::new();

            while !input.is_empty() && !input.peek::<TRight>() {
                args.push(input.parse()?);

                if !input.peek::<TRight>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRight>()?;

            ty = Type {
                span: start.to(input.prev_span()),
                kind: TypeKind::Subst {
                    ty: Box::new(ty),
                    args,
                },
            };
        }

        Ok(ty)
    }
}

impl Type {
    fn atom(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let kind = if let Ok(_) = input.parse::<TFn>() {
            let _ = input.parse::<TLParen>()?;
            let mut params = Vec::new();

            while !input.is_empty() && !input.peek::<TRParen>() {
                params.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                }
            }

            let _ = input.parse::<TRParen>()?;
            let _ = input.parse::<TArrow>()?;
            let ret = input.parse()?;

            TypeKind::Func { params, ret }
        } else if let Ok(_) = input.parse::<TFor>() {
            let gen = input.parse()?;
            let ty = input.parse()?;

            TypeKind::Forall { gen, ty }
        } else if let Ok(_) = input.parse::<TStar>() {
            let ty = input.parse()?;

            TypeKind::Ptr {
                kind: PtrKind::Single,
                ty,
            }
        } else if let Ok(_) = input.parse::<TLBracket>() {
            if let Ok(_) = input.parse::<TStar>() {
                let null = if let Ok(_) = input.parse::<TColon>() {
                    let term = input.parse::<IntLiteral>()?;

                    term.int == 0
                } else {
                    false
                };

                let _ = input.parse::<TRBracket>()?;
                let to = input.parse()?;

                TypeKind::Ptr {
                    kind: PtrKind::Multiple(null),
                    ty: to,
                }
            } else if let Ok(len) = input.parse::<IntLiteral>() {
                let _ = input.parse::<TRBracket>()?;
                let of = input.parse()?;

                TypeKind::Array {
                    len: len.int as usize,
                    of,
                }
            } else {
                let _ = input.parse::<TRBracket>()?;
                let of = input.parse()?;

                TypeKind::Slice { of }
            }
        } else if let Ok(_) = input.parse::<TLParen>() {
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

            if tuple || tys.is_empty() {
                TypeKind::Tuple { tys }
            } else {
                TypeKind::Parens {
                    inner: Box::new(tys.pop().unwrap()),
                }
            }
        } else if let Ok(_) = input.parse::<TWildcard>() {
            TypeKind::Infer
        } else if let Ok(name) = input.parse::<Ident>() {
            if let Ok(_) = input.parse::<TDot>() {
                let field = input.parse::<Ident>()?;

                TypeKind::Path {
                    module: name,
                    name: field,
                }
            } else {
                TypeKind::Ident { name }
            }
        } else {
            return input.error("expected 'fn', '*', '[', '_', '#' or an identifier", 0001);
        };

        Ok(Type {
            span: start.to(input.prev_span()),
            kind,
        })
    }
}

impl Parse for TypeParam {
    fn parse(input: ParseStream) -> Result<Self> {
        let start = input.span();
        let name = input.parse()?;
        let _ = input.parse::<TColon>()?;
        let ty = input.parse()?;

        Ok(TypeParam {
            span: start.to(input.prev_span()),
            name,
            ty,
        })
    }
}
