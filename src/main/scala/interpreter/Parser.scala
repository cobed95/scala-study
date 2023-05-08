package org.laplacetec.study
package interpreter

import org.laplacetec.study.interpreter.DataBundle._
import org.laplacetec.study.interpreter._


class ParserException(val msg: String) extends Exception

object Parser {
  def apply(tokens:List[Token]): Expr =
    throw new ParserException("Not implemented yet.")
}
