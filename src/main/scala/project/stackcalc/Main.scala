package org.laplacetec.study
package project.stackcalc

object Main {
  /**
   * Parse a list of tokens to an arithmetic expression.
   * Ex) parser(List(TkInt(1), TkBop(OpAdd), TkInt(2))) => EOp(OpAdd, EInt(1), EInt(2))
   * @return
   */
  def parser: List[Token] => Exp = ???

  /**
   * Evaluate an expression to an EInt if possible, otherwise return EError.
   * Ex) eval(EOp(OpAdd, EInt(1), EInt(2))) => EInt(3)
   * @return
   */
  def eval: Exp => Either[EInt, EError] = ???

  def run(tokens: List[Token]): Either[EInt, EError] = eval(parser(tokens))
}
