
use data::*;

use std::rc::Rc;
use std::collections::HashMap;
use std::error::Error;

type VMResult = Result<(), Box<Error>>;

impl SECD {
    pub fn new(c: Code) -> SECD {
        return SECD {
                   stack: vec![],
                   env: HashMap::new(),
                   code: c,
                   dump: vec![],
               };
    }

    fn error(&self, c: &CodeOPInfo, msg: &str) -> VMResult {
        return Err(From::from(format!("{}:{}:vm error: {}", c.info[0], c.info[1], msg)));
    }

    pub fn run(&mut self) -> Result<Rc<Lisp>, Box<Error>> {
        try!(self.run_());
        return Ok(self.stack.last().unwrap().clone());
    }

    fn run_(&mut self) -> VMResult {
        while self.code.len() > 0 {
            let c = self.code.remove(0);
            match c.op { 
                CodeOP::LET(ref id) => {
                    try!(self.run_let(&c, id));
                }

                CodeOP::LD(ref id) => {
                    try!(self.run_ld(&c, id));
                }

                CodeOP::LDC(ref lisp) => {
                    try!(self.run_ldc(&c, lisp));
                }

                CodeOP::LDF(ref names, ref code) => {
                    try!(self.run_ldf(&c, names, code));
                }

                CodeOP::RET => {
                    try!(self.run_ret(&c));
                }

                CodeOP::AP => {
                    try!(self.run_ap(&c));
                }

                CodeOP::RAP => {
                    try!(self.run_rap(&c));
                }

                CodeOP::ARGS(n) => {
                    try!(self.run_args(&c, n));
                }

                CodeOP::PUTS => {
                    try!(self.run_puts(&c));
                }

                CodeOP::SEL(ref t, ref f) => {
                    try!(self.run_sel(&c, t, f));
                }

                CodeOP::JOIN => {
                    try!(self.run_join(&c));
                }

                CodeOP::EQ => {
                    try!(self.run_eq(&c));
                }

                CodeOP::ADD => {
                    try!(self.run_add(&c));
                }

                CodeOP::SUB => {
                    try!(self.run_sub(&c));
                }

                CodeOP::CONS => {
                    try!(self.run_cons(&c));
                }

                CodeOP::CAR => {
                    try!(self.run_car(&c));
                }

                CodeOP::CDR => {
                    try!(self.run_cdr(&c));
                }
            }
        }

        return Ok(());
    }


    fn run_let(&mut self, _: &CodeOPInfo, id: &String) -> VMResult {
        let expr = self.stack.pop().unwrap();
        self.env.insert(id.clone(), expr);
        return Ok(());
    }

    fn run_ld(&mut self, _: &CodeOPInfo, id: &String) -> VMResult {
        let expr = self.env.get(id).unwrap();
        self.stack.push(expr.clone());
        return Ok(());
    }

    fn run_ldc(&mut self, _: &CodeOPInfo, lisp: &Rc<Lisp>) -> VMResult {
        self.stack.push(lisp.clone());
        return Ok(());
    }

    fn run_ldf(&mut self, _: &CodeOPInfo, names: &Vec<String>, code: &Code) -> VMResult {
        self.stack
            .push(Rc::new(Lisp::Closure(names.clone(), code.clone(), self.env.clone())));
        return Ok(());
    }

    fn run_ap(&mut self, c: &CodeOPInfo) -> VMResult {
        match *self.stack.pop().unwrap() {
            Lisp::Closure(ref names, ref code, ref env) => {
                match *self.stack.pop().unwrap() {
                    Lisp::List(ref vals) => {
                        let mut env = env.clone();
                        for i in 0..names.len() {
                            env.insert(names[i].clone(), vals[i].clone());
                        }

                        self.dump
                            .push(DumpOP::DumpAP(self.stack.clone(),
                                                 self.env.clone(),
                                                 self.code.clone()));

                        self.stack = vec![];
                        self.env = env;
                        self.code = code.clone();

                        return Ok(());
                    }
                    _ => return self.error(c, "AP: expected List"),
                }
            }

            _ => return self.error(c, "AP: expected Closure"),
        }
    }

    fn run_rap(&mut self, c: &CodeOPInfo) -> VMResult {
        match *self.stack.pop().unwrap() {
            Lisp::Closure(ref names, ref code, ref env) => {
                match *self.stack.pop().unwrap() {
                    Lisp::List(ref vals) => {
                        let mut env = env.clone();
                        for i in 0..names.len() {
                            env.insert(names[i].clone(), vals[i].clone());
                        }

                        self.dump
                            .push(DumpOP::DumpAP(self.stack.clone(),
                                                 self.env.clone(),
                                                 self.code.clone()));

                        self.stack = vec![];
                        self.env.extend(env);
                        self.code = code.clone();

                        return Ok(());
                    }

                    _ => return self.error(c, "RAP: expected List"),
                }
            }

            _ => return self.error(c, "RAP: expected Closure"),
        }
    }

    fn run_ret(&mut self, c: &CodeOPInfo) -> VMResult {
        let a = self.stack.pop().unwrap();
        match self.dump.pop().unwrap() {
            DumpOP::DumpAP(stack, env, code) => {
                self.stack = stack;
                self.env = env;
                self.code = code.clone();

                self.stack.push(a.clone());

                return Ok(());
            }

            _ => return self.error(c, "RET: expected DumpAP"),
        }
    }

    fn run_args(&mut self, _: &CodeOPInfo, n: usize) -> VMResult {
        let mut ls = vec![];
        for _ in 0..n {
            ls.insert(0, self.stack.pop().unwrap());
        }

        self.stack.push(Rc::new(Lisp::List(ls)));
        return Ok(());
    }

    fn run_puts(&mut self, _: &CodeOPInfo) -> VMResult {
        println!("{}", *self.stack.last().unwrap());
        return Ok(());
    }

    fn run_sel(&mut self, c: &CodeOPInfo, t: &Code, f: &Code) -> VMResult {
        let b = self.stack.pop().unwrap();
        let code = match *b {
            Lisp::True => t,
            Lisp::False => f,
            _ => return self.error(c, "SEL: expected bool"),
        };

        self.dump.push(DumpOP::DumpSEL(self.code.clone()));

        self.code = code.clone();

        return Ok(());
    }

    fn run_join(&mut self, c: &CodeOPInfo) -> VMResult {
        if let DumpOP::DumpSEL(ref code) = self.dump.pop().unwrap() {
            self.code = code.clone();

            return Ok(());
        } else {
            return self.error(c, "JOIN: expected DumpSEL");
        }
    }

    fn run_eq(&mut self, _: &CodeOPInfo) -> VMResult {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();
        self.stack
            .push(Rc::new(if a == b { Lisp::True } else { Lisp::False }));

        return Ok(());
    }

    fn run_add(&mut self, c: &CodeOPInfo) -> VMResult {
        let a = self.stack.pop().unwrap();
        if let Lisp::Int(n) = *a {
            let b = self.stack.pop().unwrap();
            if let Lisp::Int(m) = *b {
                self.stack.push(Rc::new(Lisp::Int(m + n)));

                return Ok(());
            } else {
                return self.error(c, "ADD: expected int");
            }
        } else {
            return self.error(c, "ADD: expected int");
        }
    }

    fn run_sub(&mut self, c: &CodeOPInfo) -> VMResult {
        let a = self.stack.pop().unwrap();
        if let Lisp::Int(n) = *a {
            let b = self.stack.pop().unwrap();
            if let Lisp::Int(o) = *b {
                self.stack.push(Rc::new(Lisp::Int(o - n)));

                return Ok(());
            } else {
                return self.error(c, "SUB: expected int");
            }
        } else {
            return self.error(c, "SUB: expected int");
        }
    }

    fn run_cons(&mut self, _: &CodeOPInfo) -> VMResult {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();
        self.stack.push(Rc::new(Lisp::Cons(b, a)));

        return Ok(());
    }

    fn run_car(&mut self, c: &CodeOPInfo) -> VMResult {
        let a = self.stack.pop().unwrap();
        if let Lisp::Cons(ref car, _) = *a {
            self.stack.push(car.clone());

            return Ok(());
        } else {
            return self.error(c, "CAR: expected Cons");
        }
    }

    fn run_cdr(&mut self, c: &CodeOPInfo) -> VMResult {
        let a = self.stack.pop().unwrap();
        if let Lisp::Cons(_, ref cdr) = *a {
            self.stack.push(cdr.clone());

            return Ok(());
        } else {
            return self.error(c, "CDR: expected Cons");
        }
    }
}
