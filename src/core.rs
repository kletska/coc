use std::rc::Rc;

use crate::list::List;

pub enum TermUp {
    Ann(Rc<TermDown>, Rc<Type>),
    Bound(usize),
    Free(Name),
    App(Rc<TermUp>, Rc<TermDown>),
}

pub enum TermDown {
    Inf(Rc<TermUp>),
    Lam(Rc<TermDown>),
}

#[derive(PartialEq, Eq)]
pub enum Type {
    TFree(Name),
    Fun(Rc<Type>, Rc<Type>),
}

pub enum Value {
    VLam(Rc<dyn Fn(Rc<Value>) -> Rc<Value>>),
    VNeutral(Rc<Neutral>),
}

pub enum Neutral {
    NFree(Name),
    NApp(Rc<Neutral>, Rc<Value>)
}

#[derive(Clone, PartialEq, Eq)]
pub enum Name {
    Global(String),
    Local(usize),
    Quote(usize),
}

pub fn vfree(n: Name) -> Rc<Value> {
    Rc::new(Value::VNeutral(Rc::new(Neutral::NFree(n))))
}

type Env = List<Rc<Value>>;


pub fn eval_up(t: Rc<TermUp>, env: Rc<Env>) -> Rc<Value> {
    match &*t {
        TermUp::Ann( e, _) => {
            eval_down(e.clone(), env)
        }
        TermUp::Free(x) => {
            vfree(x.clone())
        }
        TermUp::Bound(i) => env[*i].clone(),
        TermUp::App(e1, e2) => {
            vapp(eval_up(e1.clone(), env.clone()), eval_down(e2.clone(), env))
        }
    }
}

fn vapp(v1: Rc<Value>, v2: Rc<Value>) -> Rc<Value> {
    match &*v1 {
        Value::VLam(f) => f(v2),
        Value::VNeutral(n) => Rc::new(Value::VNeutral(Rc::new(Neutral::NApp(n.clone(), v2))))
    }
}

pub fn eval_down(t: Rc<TermDown>, env: Rc<Env>) -> Rc<Value> {
    match &*t {
        TermDown::Inf(i) => eval_up(i.clone(), env),
        TermDown::Lam(e)  => {
            let e = e.clone();

            let f: Rc<dyn Fn(Rc<Value>) -> Rc<Value>> = Rc::new(move |x| {
                eval_down(e.clone(), List::new(x, env.clone()))
            });

            Rc::new(Value::VLam(f))
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Star,
}

#[derive(Clone)]
pub enum Info {
    HasKind(Kind),
    HasType(Rc<Type>),
}

type Context = List<(Name, Info)>;

type Res<T> = Result<T, String>;

pub fn find(G: Rc<Context>, name: &Name) -> Option<Info> {
    match &*G {
        List::Nil => None,

        List::Cons(head, _tail) if name == &head.0 => {
            Some(head.1.clone())
        }

        List::Cons(_head, tail) => {
            find(tail.clone(), name)
        }
    }
}

pub fn kind_down(G: Rc<Context>, typ: Rc<Type>, kind: Kind) -> Res<()> {
    match &*typ {
        Type::TFree(x) => {
            match find(G, x) {
                Some(Info::HasKind(founded_kind)) if founded_kind == kind => Ok(()),
                _ => Err(String::from("unknown identifier")),
            }
        }

        Type::Fun(k1, k2) => {
            kind_down(G.clone(), k1.clone(), kind)?;
            kind_down(G, k2.clone(), kind)
        }
    }
}

pub fn type_up(i: usize, G: Rc<Context>, term: Rc<TermUp>) -> Res<Rc<Type>> {
    match &*term {
        TermUp::Ann(e, typ) => {
            kind_down(G.clone(), typ.clone(), Kind::Star)?;
            type_down(i, G, e.clone(), typ.clone())?;
            Ok(typ.clone())
        }

        TermUp::Free(x) => {
            match find(G, x) {
                Some(Info::HasType(typ)) => Ok(typ),
                _ => Err(String::from("unknown identifier")), 
            }
        }

        TermUp::App(e1, e2) => {
            let sigma = type_up(i, G.clone(), e1.clone())?;
            match &*sigma {
                Type::Fun(typ1, typ2) => {
                    type_down(i, G, e2.clone(), typ1.clone())?;
                    Ok(typ2.clone())
                }
                _ => Err(String::from("illegal application")),
            }
        }

        TermUp::Bound(_) => {
            Err(String::from("this shouldn't happen"))
        }
    }
}

pub fn type_down(i: usize, G: Rc<Context>, term: Rc<TermDown>, typ: Rc<Type>) -> Res<()> {
    match (&*term, &*typ) {
        (TermDown::Inf(e), _) => {
            let typ2 = type_up(i, G, e.clone())?;

            if typ == typ2 {
                Ok(())
            } else {
                Err(String::from("type mismatch"))
            }
        },

        (TermDown::Lam(e), Type::Fun(typ1, typ2)) => {
            let G = List::new((Name::Local(i), Info::HasType(typ1.clone())), G);

            let term1 = Rc::new(TermUp::Free(Name::Local(i)));

            type_down(i + 1, G, subst_down(0, term1, e.clone()), typ2.clone())
        },

        _ => Err(String::from("type mismatch")),
    }
}

pub fn subst_up(i: usize, r: Rc<TermUp>, term: Rc<TermUp>) -> Rc<TermUp> {
    match &*term {
        TermUp::Ann(e, t) => {
            Rc::new(TermUp::Ann(subst_down(i, r, e.clone()), t.clone()))
        },

        TermUp::Bound(j) => {
            if i == *j {
                r
            } else {
                term.clone()
            }
        },

        TermUp::Free(_) => {
            term.clone()
        },

        TermUp::App(e1, e2) => {
            Rc::new(TermUp::App(subst_up(i, r.clone(), e1.clone()), subst_down(i, r, e2.clone())))
        },
    }    
}

pub fn subst_down(i: usize, r: Rc<TermUp>, term: Rc<TermDown>) -> Rc<TermDown> {
    match &*term {
        TermDown::Inf(e) => {
            Rc::new(TermDown::Inf(subst_up(i, r, e.clone())))
        },

        TermDown::Lam(e) => {
            Rc::new(TermDown::Lam(subst_down(i + 1, r, e.clone())))
        }
    }
}

pub fn value_quote(i: usize, v: Rc<Value>) -> Rc<TermDown> {
    match &*v {
        Value::VLam(f) => {
            let val = vfree(Name::Quote(i));
            Rc::new(TermDown::Lam(value_quote(i + 1, f(val))))
        },
        
        Value::VNeutral(n) => {
            Rc::new(TermDown::Inf(neutral_quote(i, n.clone())))
        },
    }
}

pub fn neutral_quote(i: usize, n: Rc<Neutral>) -> Rc<TermUp> {
    match &*n {
        Neutral::NFree(x) => {
            bound_free(i, x.clone())
        },

        Neutral::NApp(n, v) => {
            Rc::new(TermUp::App(neutral_quote(i, n.clone()), value_quote(i, v.clone())))
        },
    }
}

pub fn bound_free(i: usize, x: Name) -> Rc<TermUp> {
    match x {
        Name::Quote(k) => {
            Rc::new(TermUp::Bound(i - k - 1))
        },
        x => {
            Rc::new(TermUp::Free(x))
        }
    }
}
















