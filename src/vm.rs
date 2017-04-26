
use data::*;

use std::rc::Rc;
use std::collections::HashMap;
use std::error::Error;
use std::mem;

type VMResult = Result<(), Box<Error>>;

impl SECD {
    pub fn new(c: Code) -> SECD {
        return SECD {
                   stack: (vec![]),
                   env: HashMap::new(),
                   code: c,
                   dump: vec![],
               };
    }

    fn error(&self, info: &Info, msg: &str) -> VMResult {
        return Err(From::from(format!("{}:{}:vm error: {}", info[0], info[1], msg)));
    }

    pub fn run(&mut self) -> Result<Rc<Lisp>, Box<Error>> {
        try!(self.run_());
        return Ok(self.stack.last().unwrap().clone());
    }

    fn run_(&mut self) -> VMResult {
        while self.code.len() > 0 {
            let c = self.code.remove(0);
            let info = c.info;
            match c.op {
                CodeOP::LET(id) => {
                    try!(self.run_let(info, id));
                }

                CodeOP::LD(id) => {
                    try!(self.run_ld(info, id));
                }

                CodeOP::LDC(lisp) => {
                    try!(self.run_ldc(info, lisp));
                }

                CodeOP::LDF(names, code) => {
                    try!(self.run_ldf(info, names, code));
                }

                CodeOP::RET => {
                    try!(self.run_ret(info));
                }

                CodeOP::AP => {
                    try!(self.run_ap(info));
                }

                CodeOP::RAP => {
                    try!(self.run_rap(info));
                }

                CodeOP::ARGS(n) => {
                    try!(self.run_args(info, n));
                }

                CodeOP::PUTS => {
                    try!(self.run_puts(info));
                }

                CodeOP::SEL(t, f) => {
                    try!(self.run_sel(info, t, f));
                }

                CodeOP::JOIN => {
                    try!(self.run_join(info));
                }

                CodeOP::EQ => {
                    try!(self.run_eq(info));
                }

                CodeOP::ADD => {
                    try!(self.run_add(info));
                }

                CodeOP::SUB => {
                    try!(self.run_sub(info));
                }

                CodeOP::CONS => {
                    try!(self.run_cons(info));
                }

                CodeOP::CAR => {
                    try!(self.run_car(info));
                }

                CodeOP::CDR => {
                    try!(self.run_cdr(info));
                }
            }
        }

        return Ok(());
    }


    fn run_let(&mut self, info: Info, id: String) -> VMResult {
        if let Some(expr) = self.stack.pop() {
            self.env.insert(id, expr);
            return Ok(());
        } else {
            return self.error(&info, "LET: stack is empty");
        }
    }

    fn run_ld(&mut self, info: Info, id: String) -> VMResult {
        if let Some(expr) = self.env.get(&id) {
            self.stack.push(expr.clone());
            return Ok(());
        } else {
            return self.error(&info, format!("LD: found {}", id).as_str());
        }
    }

    fn run_ldc(&mut self, _: Info, lisp: Rc<Lisp>) -> VMResult {
        self.stack.push(lisp);
        return Ok(());
    }

    fn run_ldf(&mut self, _: Info, names: Vec<String>, code: Code) -> VMResult {
        self.stack
            .push(Rc::new(Lisp::Closure(names, code, self.env.clone())));
        return Ok(());
    }

    fn run_ap(&mut self, info: Info) -> VMResult {
        let s = self.stack.pop();
        if let Some(closure) = s {
            if let Lisp::Closure(ref names, ref code, ref env) = *closure {
                let s = self.stack.pop();
                if let Some(list) = s {
                    if let Lisp::List(ref vals) = *list {
                        let mut env = env.clone();
                        for i in 0..names.len() {
                            env.insert(names[i].clone(), vals[i].clone());
                        }

                        let stack = mem::replace(&mut self.stack, vec![]);
                        let env = mem::replace(&mut self.env, env);
                        let code = mem::replace(&mut self.code, code.clone());
                        self.dump.push(DumpOP::DumpAP(stack, env, code));

                        return Ok(());
                    } else {
                        return self.error(&info, "AP: expected List");
                    }
                } else {
                    return self.error(&info, "AP: stack is empty");
                }
            } else {
                return self.error(&info, "AP: expected Closure");
            }
        } else {
            return self.error(&info, "AP: stack is empty");
        }
    }

    fn run_rap(&mut self, info: Info) -> VMResult {
        let s = self.stack.pop();
        if let Some(closure) = s {
            if let Lisp::Closure(ref names, ref code, ref env) = *closure {
                let ss = self.stack.pop();
                if let Some(list) = ss {
                    if let Lisp::List(ref vals) = *list {
                        let mut env = env.clone();
                        for i in 0..names.len() {
                            env.insert(names[i].clone(), vals[i].clone());
                        }

                        let stack = mem::replace(&mut self.stack, vec![]);
                        let code = mem::replace(&mut self.code, code.clone());

                        self.dump
                            .push(DumpOP::DumpAP(stack, self.env.clone(), code));

                        self.env.extend(env);

                        return Ok(());
                    } else {
                        return self.error(&info, "RAP: expected List");
                    }
                } else {
                    return self.error(&info, "RAP: stack is empty");
                }
            } else {
                return self.error(&info, "RAP: expected Closure");
            }
        } else {
            return self.error(&info, "RAP: stack is empty");
        }
    }

    fn run_ret(&mut self, info: Info) -> VMResult {
        let s = self.stack.pop();
        if let Some(val) = s {
            if let Some(DumpOP::DumpAP(stack, env, code)) = self.dump.pop() {

                self.stack = stack;
                self.env = env;
                self.code = code;

                self.stack.push(val);

                return Ok(());
            } else {
                return self.error(&info, "RET: dump is empty");
            }
        } else {
            return self.error(&info, "RET: stack is empty");
        }
    }

    fn run_args(&mut self, info: Info, n: usize) -> VMResult {
        let mut ls = vec![];
        for _ in 0..n {
            match self.stack.pop() {
                None => {
                    return self.error(&info, &format!("ARGS: {}", n));
                }

                Some(a) => {
                    ls.insert(0, a);
                }
            }
        }

        self.stack.push(Rc::new(Lisp::List(Rc::new(ls))));
        return Ok(());
    }

    fn run_puts(&mut self, info: Info) -> VMResult {
        match self.stack.last() {
            None => {
                return self.error(&info, "PUTS: expected args");
            }

            Some(a) => {
                println!("{}", **a);
                return Ok(());
            }
        }
    }

    fn run_sel(&mut self, info: Info, t: Code, f: Code) -> VMResult {
        let s = self.stack.pop();
        if let Some(b) = s {
            let code = match *b {
                Lisp::True => t,
                Lisp::False => f,
                _ => return self.error(&info, "SEL: expected bool"),
            };

            let code = mem::replace(&mut self.code, code);

            self.dump.push(DumpOP::DumpSEL(code));

            return Ok(());
        } else {
            return self.error(&info, "SEL: stack is empty");
        }
    }

    fn run_join(&mut self, info: Info) -> VMResult {
        let d = self.dump.pop();
        if let Some(dump) = d {
            if let DumpOP::DumpSEL(code) = dump {
                self.code = code;

                return Ok(());
            } else {
                return self.error(&info, "JOIN: expected DumpSEL");
            }
        } else {
            return self.error(&info, "JOIN: dump is empty");
        }
    }

    fn run_eq(&mut self, info: Info) -> VMResult {
        let s = self.stack.pop();
        if let Some(a) = s {
            let ss = self.stack.pop();
            if let Some(b) = ss {
                self.stack
                    .push(Rc::new(if a == b { Lisp::True } else { Lisp::False }));

                return Ok(());
            } else {
                return self.error(&info, "EQ: stack is empty");
            }
        } else {
            return self.error(&info, "EQ: stack is empty");
        }
    }

    fn run_add(&mut self, info: Info) -> VMResult {
        let s = self.stack.pop();
        if let Some(a) = s {
            if let Lisp::Int(n) = *a {
                let ss = self.stack.pop();
                if let Some(b) = ss {
                    if let Lisp::Int(m) = *b {
                        self.stack.push(Rc::new(Lisp::Int(m + n)));

                        return Ok(());
                    } else {
                        return self.error(&info, "ADD: expected int");
                    }
                } else {
                    return self.error(&info, "ADD: stack is empty");
                }
            } else {
                return self.error(&info, "ADD: expected int");
            }
        } else {
            return self.error(&info, "ADD: stack is empty");
        }
    }

    fn run_sub(&mut self, info: Info) -> VMResult {
        let s = self.stack.pop();
        if let Some(a) = s {
            if let Lisp::Int(n) = *a {
                let ss = self.stack.pop();
                if let Some(b) = ss {
                    if let Lisp::Int(o) = *b {
                        self.stack.push(Rc::new(Lisp::Int(o - n)));

                        return Ok(());
                    } else {
                        return self.error(&info, "SUB: expected int");
                    }
                } else {
                    return self.error(&info, "SUB: stack is empty");
                }
            } else {
                return self.error(&info, "SUB: expected int");
            }
        } else {
            return self.error(&info, "SUB: stack is empty");
        }
    }

    fn run_cons(&mut self, info: Info) -> VMResult {
        let a = self.stack.pop();
        if let Some(a) = a {
            let b = self.stack.pop();
            if let Some(b) = b {
                self.stack.push(Rc::new(Lisp::Cons(b, a)));

                return Ok(());
            } else {
                return self.error(&info, "CONS: stack is empty");
            }
        } else {
            return self.error(&info, "CONS: stack is empty");
        }
    }

    fn run_car(&mut self, info: Info) -> VMResult {
        let a = self.stack.pop();
        if let Some(a) = a {
            if let Lisp::Cons(ref car, _) = *a {
                self.stack.push(car.clone());

                return Ok(());
            } else {
                return self.error(&info, "CAR: expected Cons");
            }
        } else {
            return self.error(&info, "CAR: stack is empty");
        }
    }

    fn run_cdr(&mut self, info: Info) -> VMResult {
        let a = self.stack.pop();
        if let Some(a) = a {
            if let Lisp::Cons(_, ref cdr) = *a {
                self.stack.push(cdr.clone());

                return Ok(());
            } else {
                return self.error(&info, "CDR: expected Cons");
            }
        } else {
            return self.error(&info, "CDR: stack is empty");
        }
    }
}
