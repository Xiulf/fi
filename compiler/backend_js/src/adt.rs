use hir::ty::{Ty, TyKind};
use hir::{Ctor, Literal, TypeCtor};

use crate::expr::JsExpr;
use crate::Ctx;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Repr {
    Any,
    Scalar { scalar: Scalar },
    Tuple { items: Vec<Repr> },
    Struct { fields: Vec<(String, Repr)> },
    Enum { variants: Variants },
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Scalar {
    Undefined,
    Bool,
    Number,
    String,
    Func,
}

#[derive(Debug, Clone)]
pub enum Variants {
    Single { index: usize, repr: Box<Repr> },
    Multiple { reprs: Vec<Repr> },
}

impl<'a> Ctx<'a> {
    pub fn ty_ctor_repr(&mut self, ty_ctor: TypeCtor) -> Repr {
        if let Some(repr) = self.ty_ctor_reprs.get(&ty_ctor) {
            return repr.clone();
        }

        let mut ctors = ty_ctor.ctors(self.db);

        if ctors.len() == 1 {
            let ctor = ctors.remove(0);
            let types = ctor.types(self.db);

            self.tuple_repr(types)
        } else {
            let reprs = ctors
                .into_iter()
                .map(|ctor| {
                    let types = ctor.types(self.db);

                    self.tuple_repr(types)
                })
                .collect::<Vec<_>>();

            Repr::Enum {
                variants: Variants::Multiple { reprs },
            }
        }
    }

    fn tuple_repr(&mut self, mut types: Vec<Ty>) -> Repr {
        match types.len() {
            | 0 => Repr::Scalar {
                scalar: Scalar::Undefined,
            },
            | 1 => self.ty_repr(types.remove(0)),
            | _ => {
                let items = types.into_iter().map(|t| self.ty_repr(t)).collect();

                Repr::Tuple { items }
            },
        }
    }

    pub fn ctor_repr(&mut self, ctor: Ctor) -> Repr {
        let ty_ctor = self.ty_ctor_repr(ctor.type_ctor());

        match ty_ctor {
            | Repr::Enum {
                variants: Variants::Multiple { mut reprs },
            } => {
                let ctors = ctor.type_ctor().ctors(self.db);
                let index = ctors.iter().position(|&c| c == ctor).unwrap();

                Repr::Enum {
                    variants: Variants::Single {
                        index,
                        repr: Box::new(reprs.swap_remove(index)),
                    },
                }
            },
            | _ => ty_ctor,
        }
    }

    pub fn ty_repr(&mut self, ty: Ty) -> Repr {
        match ty.lookup(self.db) {
            | TyKind::Figure(_) => Repr::Scalar { scalar: Scalar::Number },
            | TyKind::Symbol(_) => Repr::Scalar { scalar: Scalar::String },
            | TyKind::TypeVar(_) => Repr::Any,
            | TyKind::Where(_, inner) => self.ty_repr(inner),
            | TyKind::ForAll(_, inner, _) => self.ty_repr(inner),
            | TyKind::Ctor(id) => self.ty_ctor_repr(id.into()),
            | TyKind::App(base, _args) => {
                log::warn!(target: "ty_repr", "todo: apply types");
                self.ty_repr(base)
            },
            | _ => unreachable!(),
        }
    }

    pub fn construct(&mut self, ctor: Ctor, args: Vec<JsExpr>) -> JsExpr {
        let repr = self.ctor_repr(ctor);

        self.construct_repr(repr, args)
    }

    pub fn construct_repr(&mut self, repr: Repr, mut args: Vec<JsExpr>) -> JsExpr {
        match repr {
            | Repr::Any => args.remove(0),
            | Repr::Scalar { scalar } => match scalar {
                | Scalar::Undefined => JsExpr::Undefined,
                | _ => todo!(),
            },
            | Repr::Tuple { items: _ } => JsExpr::Array { exprs: args },
            | Repr::Struct { fields: _ } => todo!(),
            | Repr::Enum { variants } => match variants {
                | Variants::Single { index, repr } => {
                    let inner = self.construct_repr(*repr, args);
                    let index = JsExpr::Literal {
                        lit: Literal::Int(index as i128),
                    };

                    JsExpr::Array {
                        exprs: vec![index, inner],
                    }
                },
                | Variants::Multiple { reprs: _ } => todo!(),
            },
        }
    }

    pub fn deconstruct(&mut self, ctor: Ctor, place: JsExpr) -> (Vec<JsExpr>, Option<JsExpr>) {
        let repr = self.ctor_repr(ctor);

        self.deconstruct_repr(repr, place)
    }

    pub fn deconstruct_repr(&mut self, repr: Repr, place: JsExpr) -> (Vec<JsExpr>, Option<JsExpr>) {
        match repr {
            | Repr::Any => (vec![place], None),
            | Repr::Scalar { .. } => (Vec::new(), None),
            | Repr::Tuple { items } => (
                (0..items.len())
                    .map(|i| JsExpr::Index {
                        base: Box::new(place.clone()),
                        idx: Box::new(JsExpr::Literal {
                            lit: Literal::Int(i as i128),
                        }),
                    })
                    .collect(),
                None,
            ),
            | Repr::Enum { variants } => match variants {
                | Variants::Single { index, repr } => {
                    let inner = JsExpr::Index {
                        base: Box::new(place.clone()),
                        idx: Box::new(JsExpr::Literal { lit: Literal::Int(1) }),
                    };

                    let (places, _) = self.deconstruct_repr(*repr, inner);

                    (
                        places,
                        Some(JsExpr::BinOp {
                            op: "==",
                            lhs: Box::new(JsExpr::Index {
                                base: Box::new(place),
                                idx: Box::new(JsExpr::Literal { lit: Literal::Int(0) }),
                            }),
                            rhs: Box::new(JsExpr::Literal {
                                lit: Literal::Int(index as i128),
                            }),
                        }),
                    )
                },
                | Variants::Multiple { reprs: _ } => todo!(),
            },
            | _ => todo!("{:?}", repr),
        }
    }
}
