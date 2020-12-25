use crate::ctx::*;
use crate::error::*;
use crate::ty::*;

pub enum Mode {
    Elaborate,
    NoElaborate,
}

trait ModeT {
    type Coercion;
    fn default_coercion() -> Self::Coercion;
}

struct ElaborateT;
struct NoElaborateT;

impl ModeT for ElaborateT {
    type Coercion = fn(()) -> ();

    fn default_coercion() -> Self::Coercion {
        std::convert::identity
    }
}

impl ModeT for NoElaborateT {
    type Coercion = ();

    fn default_coercion() -> Self::Coercion {}
}

impl<'db> Ctx<'db> {
    crate fn subsumes(&mut self, t1: Ty, t2: Ty) -> Result<fn(()) -> ()> {
        self.subsumes_::<ElaborateT>(t1, t2, Mode::Elaborate)
    }

    fn subsumes_<M: ModeT>(&mut self, t1: Ty, t2: Ty, mode: Mode) -> Result<M::Coercion> {
        match (mode, &*t1, &*t2) {
            (mode, Type::ForAll(vars, r1, _), _) => {
                let subst = vars
                    .into_iter()
                    .map(|v| (v, self.fresh_type(t1.span())))
                    .collect();

                let replaced = r1.clone().replace_vars(subst);

                self.subsumes_::<M>(replaced, t2, mode)
            }
            (mode, _, Type::ForAll(vars, r1, sc)) => match sc {
                Some(sc) => {
                    let skolems = (0..vars.len())
                        .map(|_| self.new_skolem_constant())
                        .collect();

                    let sk = self.skolemize(t2.span(), vars, skolems, r1.clone(), *sc);

                    self.subsumes_::<M>(t1, sk, mode)
                }
                None => Err(TypeError::Internal(
                    "subsumes: unspecified skolem scope".into(),
                )),
            },
            (_, _, _) => {
                self.unify_types(t1, t2)?;

                Ok(M::default_coercion())
            }
        }
    }
}
