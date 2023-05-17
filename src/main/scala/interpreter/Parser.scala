package org.laplacetec.study
package interpreter

import org.laplacetec.study.interpreter.DataBundle._
import org.laplacetec.study.interpreter._

class ParserException(val msg: String) extends Exception

object Parser {
  def apply(tokens: List[Token]): Expr = {
    def applyHelper(tokens: List[Token]): (Expr, List[Token]) = tokens match {
      case TPLUS() :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        (EPlus(e1, e2), tail2)
      }
      case TMINUS() :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        (EMinus(e1, e2), tail2)
      }
      case TMULT() :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        (EMult(e1, e2), tail2)
      }
      case TEQ() :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        (EEq(e1, e2), tail2)
      }
      case TLT() :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        (ELt(e1, e2), tail2)
      }
      case TGT() :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        (EGt(e1, e2), tail2)
      }
      case TCONS() :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        (ECons(e1, e2), tail2)
      }
      case TIF() :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        val (e3, tail3) = applyHelper(tail2)
        (EIf(e1, e2, e3), tail3)
      }
      case TINT(n) :: tail  => (EInt(n), tail)
      case TTRUE() :: tail  => (ETrue(), tail)
      case TFALSE() :: tail => (EFalse(), tail)
      case TNIL() :: tail   => (ENil(), tail)
      // case TNNAME(s) :: tail => (AName(s), tail)
      // case TVNAME(s) :: tail => (EVName(s), tail)
      case TFST() :: tail => {
        val (e, tail1) = applyHelper(tail)
        (EFst(e), tail1)
      }
      case TSND() :: tail => {
        val (e, tail1) = applyHelper(tail)
        (ESnd(e), tail1)
      }
      case Nil => throw new ParserException("Syntax error.")
      case _   => throw new ParserException("Not implemented yet.")
      case TNNAME(s) :: tail => {
        val (e, tail1) = applyHelper(tail)
        (ANname(s), tail1)
      }
      case TDEF() :: TVNAME(s) :: tail => {
        val (e1, tail1) = applyHelper(tail)
        val (e2, tail2) = applyHelper(tail1)
        (BDef("a", List(), e1), tail2)
      }
    }

    applyHelper(tokens) match {
      case (expr, Nil) => expr
      case _           => throw new ParserException("Syntax error.")
    }
  }
}

def hello(a,b,c): a+b
