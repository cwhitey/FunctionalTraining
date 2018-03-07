package com.rea.adt

sealed trait Expr
case class Const(value: Boolean) extends Expr
case class And(a: Expr, b: Expr) extends Expr
case class Or(a: Expr, b: Expr) extends Expr
case class Not(expr: Expr) extends Expr

/**
 * Use pattern matching and recursion.  No vars, no loops, no overriding.
 */

object Expr {

  /**
   * Evaluate the expression.
   */

  def eval(expr: Expr): Boolean = expr match {
    case Const(b) => b
    case And(x, y) => eval(x) && eval(y)
    case Or(x, y) => eval(x) || eval(y)
    case Not(e) => !eval(e)
  }

  /**
   * Normalise the expression, such that:
   *
   * !!a     ==> a
   * !a & !b ==> !(a | b)
   * !a | !b ==> !(a & b)
   *
   * Normalize until you get the simplest case
   *
   * Make sure the pattern match is exhaustive (has a default clause)
   *
   * (Hint: You can and should normalize recursively)
   */

  def normalise(expr: Expr): Expr = expr match {
    case Not(Not(e)) => normalise(e)
    case And(Not(x), Not(y)) => Not(Or(normalise(x), normalise(y)))
    case Or(Not(x), Not(y)) => Not(And(normalise(x), normalise(y)))
    case _ => expr
  }

  /**
   * Show, using English lower-case words "and", "or", "not", "true", "false"
   */

  def show(expr: Expr): String = expr match {
    case Not(e) => s"not(${show(e)})"
    case And(x, y) => s"and(${show(x)},${show(y)})"
    case Or(x, y) => s"or(${show(x)},${show(y)})"
    case Const(b) => b.toString()
  }
}
