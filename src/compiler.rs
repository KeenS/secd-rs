use data::{AST, SExpr, Lisp, Code, CodeOPInfo, CodeOP, Info};

use std::rc::Rc;
use std::error::Error;


pub struct Compiler {
    pub code: Code,
    letrec_id_list: Vec<String>,
}

type CompilerResult = Result<(), Box<Error>>;

macro_rules! destruct_ {
    ($e: expr, ()) => (
        assert!($e.next().is_none())
    );
    ($e: expr, ($arg: ident, $($args: ident, )*)) => (
        let $arg = $e.next().unwrap();
        destruct_!($e, ($($args, )*))
    );
    ($e: expr, ($arg: ident $(, $args: ident)*)) => (
        let $arg = $e.next().unwrap();
        destruct_!($e, ($($args, )*))
    );
}

macro_rules! destruct {
    ($e: expr, $($rest: tt)*) => (
        let mut iter = $e.into_iter();
        destruct_!(iter, $($rest)*)
    )
}

impl Compiler {
    pub fn new() -> Self {
        return Compiler {
                   code: vec![],
                   letrec_id_list: vec![],
               };
    }

    fn error(&self, info: &Info, msg: &str) -> CompilerResult {
        return Err(From::from(format!("{}:{}:compile error: {}", info[0], info[1], msg)));
    }

    pub fn compile(&mut self, ast: AST) -> Result<Code, Box<Error>> {
        try!(self.compile_(ast));
        return Ok(self.code.clone());
    }

    pub fn compile_(&mut self, ast: AST) -> CompilerResult {
        let info = ast.info;
        match ast.sexpr {
            SExpr::Int(n) => {
                return self.compile_int(info, n);
            }

            SExpr::Atom(id) => {
                return self.compile_atom(info, id);
            }

            SExpr::List(mut ls) => {
                if ls.len() == 0 {
                    return self.compile_nil(info);
                } else {
                    let fun = ls.drain(0..1).next().unwrap();
                    let args = ls;
                    let info = fun.info;
                    match fun.sexpr {
                        SExpr::Int(_) => {
                            return self.error(&info, "apply unexpect int");
                        }

                        SExpr::Atom(id) => {
                            match id.as_str() {
                                "lambda" => {
                                    return self.compile_lambda(info, args);
                                }

                                "let" => {
                                    return self.compile_let(info, args);
                                }

                                "letrec" => {
                                    return self.compile_letrec(info, args);
                                }

                                "puts" => {
                                    return self.compile_puts(info, args);
                                }

                                "if" => {
                                    return self.compile_if(info, args);
                                }

                                "eq" => {
                                    return self.compile_eq(info, args);
                                }

                                "+" => {
                                    return self.compile_add(info, args);
                                }

                                "-" => {
                                    return self.compile_sub(info, args);
                                }

                                "cons" => {
                                    return self.compile_cons(info, args);
                                }

                                "car" => {
                                    return self.compile_car(info, args);
                                }

                                "cdr" => {
                                    return self.compile_cdr(info, args);
                                }

                                _ => {
                                    return self.compile_apply(info,
                                                              AST {
                                                                  sexpr: SExpr::Atom(id),
                                                                  info: info,
                                                              },
                                                              args);
                                }
                            }
                        }

                        ex @ SExpr::List(_) => {
                            return self.compile_apply(info,
                                                      AST {
                                                          sexpr: ex,
                                                          info: info,
                                                      },
                                                      args);
                        }
                    }
                }
            }
        }
    }

    fn compile_int(&mut self, info: Info, n: i32) -> CompilerResult {
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::LDC(Rc::new(Lisp::Int(n))),
                  });
        return Ok(());
    }

    fn compile_atom(&mut self, info: Info, id: String) -> CompilerResult {
        match id.as_str() {
            "nil" => {
                self.code
                    .push(CodeOPInfo {
                              info: info,
                              op: CodeOP::LDC(Rc::new(Lisp::Nil)),
                          });
            }

            "true" => {
                self.code
                    .push(CodeOPInfo {
                              info: info,
                              op: CodeOP::LDC(Rc::new(Lisp::True)),
                          });
            }

            "false" => {
                self.code
                    .push(CodeOPInfo {
                              info: info,
                              op: CodeOP::LDC(Rc::new(Lisp::False)),
                          });
            }

            _ => {
                self.code
                    .push(CodeOPInfo {
                              info: info,
                              op: CodeOP::LD(id.clone()),
                          });
            }
        }

        return Ok(());
    }

    fn compile_nil(&mut self, info: Info) -> CompilerResult {
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::LDC(Rc::new(Lisp::Nil)),
                  });
        return Ok(());
    }

    fn compile_lambda(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 2 {
            return self.error(&info, "lambda syntax");
        }

        destruct!(ls, (arg, body));

        let mut args: Vec<String> = vec![];
        match arg.sexpr {
            SExpr::Atom(a) => {
                args.push(a);
            }

            SExpr::List(aa) => {
                for ast in aa {
                    match ast.sexpr {
                        SExpr::Atom(a) => {
                            args.push(a);
                        }

                        _ => {
                            return self.error(&info, "lambda args");
                        }
                    }
                }
            }

            _ => {
                return self.error(&arg.info, "lambda args");
            }
        }

        let mut body_compiler = Compiler::new();
        body_compiler.letrec_id_list = self.letrec_id_list.clone();
        try!(body_compiler.compile_(body));
        body_compiler
            .code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::RET,
                  });

        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::LDF(args, body_compiler.code),
                  });

        return Ok(());
    }

    fn compile_let(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 3 {
            return self.error(&info, "let syntax");
        }

        destruct!(ls, (var, expr, body));

        let id = match var.sexpr {
            SExpr::Atom(id) => id,
            _ => return self.error(&info, "let bind id sytax"),
        };

        self.letrec_id_list.retain(|a| *a != id);

        try!(self.compile_(expr));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::LET(id),
                  });

        try!(self.compile_(body));

        return Ok(());
    }

    fn compile_letrec(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 3 {
            return self.error(&info, "let syntax");
        }

        destruct!(ls, (var, expr, body));

        let id = match var.sexpr {
            SExpr::Atom(id) => id,
            _ => return self.error(&info, "let bind id sytax"),
        };

        self.letrec_id_list.push(id.clone());

        try!(self.compile_(expr));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::LET(id),
                  });
        try!(self.compile_(body));

        return Ok(());
    }

    fn compile_puts(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 1 {
            return self.error(&info, "puts syntax");
        }

        destruct!(ls, (expr));

        try!(self.compile_(expr));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::PUTS,
                  });
        return Ok(());
    }


    fn compile_apply(&mut self, info: Info, lambda: AST, ls: Vec<AST>) -> CompilerResult {
        let args = ls;
        let nargs = args.len();
        for arg in args.into_iter() {
            try!(self.compile_(arg));
        }
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::ARGS(nargs),
                  });
        let (is_atom, id) = match lambda.sexpr {
            SExpr::Atom(ref id) => (true, Some(id.clone())),
            _ => (false, None),
        };

        try!(self.compile_(lambda));

        match (is_atom, id) {
            (true, Some(id)) => {
                if self.letrec_id_list.iter().any(|a| a == &id) {
                    self.code
                        .push(CodeOPInfo {
                                  info: info,
                                  op: CodeOP::RAP,
                              });
                } else {
                    self.code
                        .push(CodeOPInfo {
                                  info: info,
                                  op: CodeOP::AP,
                              });
                }
            }

            _ => {
                self.code
                    .push(CodeOPInfo {
                              info: info,
                              op: CodeOP::AP,
                          });
            }
        }

        return Ok(());
    }

    fn compile_if(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 3 {
            return self.error(&info, "if syntax");
        }

        destruct!(ls, (cond, then, else_));

        try!(self.compile_(cond));

        let mut tc = Compiler::new();
        tc.letrec_id_list = self.letrec_id_list.clone();

        let then_info = then.info.clone();
        try!(tc.compile_(then));
        tc.code
            .push(CodeOPInfo {
                      info: then_info,
                      op: CodeOP::JOIN,
                  });

        let mut fc = Compiler::new();

        let else_info = else_.info.clone();
        fc.letrec_id_list = self.letrec_id_list.clone();
        try!(fc.compile_(else_));
        fc.code
            .push(CodeOPInfo {
                      info: else_info,
                      op: CodeOP::JOIN,
                  });

        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::SEL(tc.code, fc.code),
                  });


        return Ok(());
    }


    fn compile_eq(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 2 {
            return self.error(&info, "eq syntax");
        }

        destruct!(ls, (l, r));

        try!(self.compile_(l));
        try!(self.compile_(r));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::EQ,
                  });

        return Ok(());
    }

    fn compile_add(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 2 {
            return self.error(&info, "add syntax");
        }

        destruct!(ls, (l, r));

        try!(self.compile_(l));
        try!(self.compile_(r));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::ADD,
                  });

        return Ok(());
    }

    fn compile_sub(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 2 {
            return self.error(&info, "sub syntax");
        }

        destruct!(ls, (l, r));

        try!(self.compile_(l));
        try!(self.compile_(r));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::SUB,
                  });

        return Ok(());
    }

    fn compile_cons(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 2 {
            return self.error(&info, "cons syntax");
        }

        destruct!(ls, (l, r));

        try!(self.compile_(l));
        try!(self.compile_(r));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::CONS,
                  });

        return Ok(());
    }

    fn compile_car(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 1 {
            return self.error(&info, "car syntax");
        }

        destruct!(ls, (expr));

        try!(self.compile_(expr));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::CAR,
                  });

        return Ok(());
    }

    fn compile_cdr(&mut self, info: Info, ls: Vec<AST>) -> CompilerResult {
        if ls.len() != 1 {
            return self.error(&info, "cdr syntax");
        }

        destruct!(ls, (expr));

        try!(self.compile_(expr));
        self.code
            .push(CodeOPInfo {
                      info: info,
                      op: CodeOP::CDR,
                  });

        return Ok(());
    }
}
