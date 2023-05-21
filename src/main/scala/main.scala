package com.draminghuang

import Value.Closure


import scala.annotation.tailrec

@main
def main(): Unit = {
  println("Hello world!")
  // λx.λy.x
  val t = Exp.Lam(Exp.Sym("x"),Exp.Lam(Exp.Sym("y"),Exp.Sym("x")))
  // λx.λy.y
  val y = Exp.Lam(Exp.Sym("x"), Exp.Lam(Exp.Sym("y"), Exp.Sym("y")))
}

enum Box[+T](contents: T):
  case IntBox(n: Int) extends Box[Int](n)
  case BoolBox(b: Boolean) extends Box[Boolean](b)
end Box

enum Exp():
  case Sym(s: String) extends Exp
  case Lam(symbol: Exp.Sym,exp: Exp) extends Exp
  case App(argument: Exp,target: Exp) extends Exp
end Exp

type Env = List[(Exp.Sym,Value)]

enum Value():
  case Closure(lambda: Exp.Lam,env: Env)
end Value

def interp(exp: Exp,env: Env): Value = {
  exp match
    case s @ Exp.Sym(_) => lookup(s,env)
    case lam @ Exp.Lam(_,_) => Closure(lam,env)
    case Exp.App(argument, func) =>
      val argumentValue = interp(argument,env)
      val funcValue = interp(func,env)
      funcValue match
        case Closure(Exp.Lam(para,body),envi) =>
          interp(body,(para,argumentValue)::envi)
        case _ => throw Exception("Not a function!")
}

@tailrec
def lookup(symbol: Exp.Sym, env: Env) : Value = {
  env match
    case (k,v)::t => if (k == symbol) {
      v
    } else {
      lookup(symbol,t)
    }
    case Nil => throw Exception("Unbound variable"+symbol.s)
}