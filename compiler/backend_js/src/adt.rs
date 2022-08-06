use hir::ty::{Ty, TyKind};
use hir::{Ctor, Literal, TypeCtor};

use crate::expr::JsExpr;
use crate::Ctx;

#[derive(Debug, Clone)]
pub struct Layout {
    repr: Repr,
    fields: Fields,
    variants: Variants,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Repr {
    Void,
    Scalar(Scalar),
    Aggregate,
}

#[derive(Debug, Clone)]
pub enum Scalar {
    Undefined,
    Bool,
    Number,
}

#[derive(Debug, Clone)]
pub enum Fields {
    Primitive,
    Arbitrary { fields: Vec<Layout> },
}

#[derive(Debug, Clone)]
pub enum Variants {
    None,
    Single { index: usize, tag: Scalar },
    Multiple { tag: Scalar, variants: Vec<Layout> },
}

impl<'a> Ctx<'a> {
    pub fn ty_ctor_repr(&mut self, ty_ctor: TypeCtor) -> Layout {
        if let Some(repr) = self.ty_ctor_reprs.get(&ty_ctor) {
            return repr.clone();
        }

        self.ty_ctor_reprs.insert(ty_ctor, Layout {
            repr: Repr::Aggregate,
            fields: Fields::Primitive,
            variants: Variants::None,
        });

        let mut ctors = ty_ctor.ctors(self.db);

        let repr = if ctors.len() == 1 {
            let ctor = ctors.remove(0);
            let types = ctor.types(self.db);

            self.tuple_repr(types)
        } else {
            let variants = ctors
                .into_iter()
                .map(|ctor| {
                    let types = ctor.types(self.db);

                    self.tuple_repr(types)
                })
                .collect::<Vec<_>>();

            let tag = match variants.len() {
                | 0 => Scalar::Undefined,
                | 1 | 2 => Scalar::Bool,
                | _ => Scalar::Number,
            };

            Layout {
                repr: Repr::Aggregate,
                fields: Fields::Arbitrary { fields: Vec::new() },
                variants: Variants::Multiple { tag, variants },
            }
        };

        self.ty_ctor_reprs.insert(ty_ctor, repr.clone());
        repr
    }

    fn tuple_repr(&mut self, mut types: Vec<Ty>) -> Layout {
        match types.len() {
            | 0 => Layout {
                repr: Repr::Void,
                fields: Fields::Primitive,
                variants: Variants::None,
            },
            | 1 => self.ty_repr(types.remove(0)),
            | _ => {
                let fields = types.into_iter().map(|t| self.ty_repr(t)).collect();

                Layout {
                    repr: Repr::Aggregate,
                    fields: Fields::Arbitrary { fields },
                    variants: Variants::None,
                }
            },
        }
    }

    pub fn ctor_repr(&mut self, ctor: Ctor) -> Layout {
        let ty_ctor = self.ty_ctor_repr(ctor.type_ctor());

        match ty_ctor.variants {
            | Variants::Multiple { tag, mut variants } => {
                let ctors = ctor.type_ctor().ctors(self.db);
                let index = ctors.iter().position(|&c| c == ctor).unwrap();
                let variant = variants.remove(index);

                Layout {
                    variants: Variants::Single { index, tag },
                    ..variant
                }
            },
            | _ => ty_ctor,
        }
    }

    pub fn ty_repr(&mut self, ty: Ty) -> Layout {
        match ty.lookup(self.db) {
            // | TyKind::Figure(_) => Repr::Scalar { scalar: Scalar::Number },
            // | TyKind::Symbol(_) => Repr::Scalar { scalar: Scalar::String },
            | TyKind::TypeVar(_) => Layout {
                repr: Repr::Aggregate,
                fields: Fields::Arbitrary {
                    fields: vec![Layout {
                        repr: Repr::Void,
                        fields: Fields::Primitive,
                        variants: Variants::None,
                    }],
                },
                variants: Variants::None,
            },
            | TyKind::Where(_, inner) => self.ty_repr(inner),
            | TyKind::ForAll(_, inner, _) => self.ty_repr(inner),
            | TyKind::Ctor(id) => self.ty_ctor_repr(id.into()),
            | TyKind::App(base, _args) => {
                // log::warn!(target: "ty_repr", "todo: apply types");
                self.ty_repr(base)
            },
            | _ => unreachable!(),
        }
    }

    pub fn construct(&mut self, ctor: Ctor, args: Vec<JsExpr>) -> JsExpr {
        let repr = self.ctor_repr(ctor);

        self.construct_repr(repr, args)
    }

    pub fn construct_repr(&mut self, layout: Layout, mut args: Vec<JsExpr>) -> JsExpr {
        match layout.variants {
            | Variants::None => match layout.repr {
                | Repr::Void => JsExpr::Undefined,
                | Repr::Scalar(_) => todo!(),
                | Repr::Aggregate => match layout.fields {
                    | Fields::Primitive => args.remove(0),
                    | Fields::Arbitrary { fields } if fields.len() == 1 => args.remove(0),
                    | Fields::Arbitrary { fields } => JsExpr::Array {
                        exprs: args.drain(..fields.len()).collect(),
                    },
                },
            },
            | Variants::Single { index, tag } => match layout.repr {
                | Repr::Void => self.construct_scalar(index as i128, tag),
                | _ => {
                    let tag = self.construct_scalar(index as i128, tag);
                    let inner = self.construct_repr(
                        Layout {
                            variants: Variants::None,
                            ..layout
                        },
                        args,
                    );

                    JsExpr::Array {
                        exprs: vec![tag, inner],
                    }
                },
            },
            | Variants::Multiple { .. } => todo!(),
        }
    }

    pub fn construct_scalar(&mut self, index: i128, tag: Scalar) -> JsExpr {
        match tag {
            | Scalar::Undefined => JsExpr::Undefined,
            | Scalar::Bool => match index {
                | 0 => JsExpr::Ident {
                    name: String::from("false"),
                },
                | _ => JsExpr::Ident {
                    name: String::from("true"),
                },
            },
            | Scalar::Number => JsExpr::Literal {
                lit: Literal::Int(index),
            },
        }
    }

    pub fn deconstruct(&mut self, ctor: Ctor, place: JsExpr) -> (Vec<JsExpr>, Option<JsExpr>) {
        let repr = self.ctor_repr(ctor);

        self.deconstruct_repr(repr, place)
    }

    pub fn deconstruct_repr(&mut self, layout: Layout, place: JsExpr) -> (Vec<JsExpr>, Option<JsExpr>) {
        match layout.variants {
            | Variants::None => match layout.repr {
                | Repr::Void | Repr::Scalar(_) => (Vec::new(), None),
                | Repr::Aggregate => match layout.fields {
                    | Fields::Primitive => (Vec::new(), None),
                    | Fields::Arbitrary { fields } if fields.len() == 1 => (vec![place], None),
                    | Fields::Arbitrary { fields } => (
                        (0..fields.len())
                            .map(|i| JsExpr::Index {
                                base: Box::new(place.clone()),
                                idx: Box::new(JsExpr::Literal {
                                    lit: Literal::Int(i as i128),
                                }),
                            })
                            .collect(),
                        None,
                    ),
                },
            },
            | Variants::Single { index, tag } => match layout.repr {
                | Repr::Void => (Vec::new(), self.check_tag(index, tag, place)),
                | _ => {
                    let tag_place = JsExpr::Index {
                        base: Box::new(place.clone()),
                        idx: Box::new(JsExpr::Literal { lit: Literal::Int(0) }),
                    };

                    let inner = JsExpr::Index {
                        base: Box::new(place),
                        idx: Box::new(JsExpr::Literal { lit: Literal::Int(1) }),
                    };

                    let check = self.check_tag(index, tag, tag_place);
                    let (places, inner) = self.deconstruct_repr(
                        Layout {
                            variants: Variants::None,
                            ..layout
                        },
                        inner,
                    );

                    let check = match (check, inner) {
                        | (Some(a), Some(b)) => Some(JsExpr::BinOp {
                            op: "&&",
                            lhs: Box::new(a),
                            rhs: Box::new(b),
                        }),
                        | (a, b) => a.or(b),
                    };

                    (places, check)
                },
            },
            | Variants::Multiple { .. } => todo!(),
        }
        // match repr {
        //     | Repr::Any => (vec![place], None),
        //     | Repr::Scalar { .. } => (Vec::new(), None),
        //     | Repr::Tuple { items } => (
        //         (0..items.len())
        //             .map(|i| JsExpr::Index {
        //                 base: Box::new(place.clone()),
        //                 idx: Box::new(JsExpr::Literal {
        //                     lit: Literal::Int(i as i128),
        //                 }),
        //             })
        //             .collect(),
        //         None,
        //     ),
        //     | Repr::Enum { variants } => match variants {
        //         | Variants::Single { index, repr } => {
        //             let inner = JsExpr::Index {
        //                 base: Box::new(place.clone()),
        //                 idx: Box::new(JsExpr::Literal { lit: Literal::Int(1) }),
        //             };

        //             let (places, _) = self.deconstruct_repr(*repr, inner);

        //             (
        //                 places,
        //                 Some(JsExpr::BinOp {
        //                     op: "==",
        //                     lhs: Box::new(JsExpr::Index {
        //                         base: Box::new(place),
        //                         idx: Box::new(JsExpr::Literal { lit: Literal::Int(0) }),
        //                     }),
        //                     rhs: Box::new(JsExpr::Literal {
        //                         lit: Literal::Int(index as i128),
        //                     }),
        //                 }),
        //             )
        //         },
        //         | Variants::Multiple { reprs: _ } => todo!(),
        //     },
        //     | _ => todo!("{:?}", repr),
        // }
    }

    pub fn check_tag(&mut self, index: usize, tag: Scalar, place: JsExpr) -> Option<JsExpr> {
        match tag {
            | Scalar::Undefined => None,
            | Scalar::Bool if index == 0 => Some(JsExpr::UnOp {
                op: "!",
                rhs: Box::new(place),
            }),
            | Scalar::Bool => Some(place),
            | Scalar::Number => Some(JsExpr::BinOp {
                op: "==",
                lhs: Box::new(place),
                rhs: Box::new(JsExpr::Literal {
                    lit: Literal::Int(index as i128),
                }),
            }),
        }
    }
}
