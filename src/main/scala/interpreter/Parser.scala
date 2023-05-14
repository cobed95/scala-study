package org.laplacetec.study
package interpreter

import org.laplacetec.study.interpreter.DataBundle._
import org.laplacetec.study.interpreter._

class ParserException(val msg: String) extends Exception

object Parser {
  def apply(tokens: List[Token]): Expr = tokens match {
    case TVNAME(x: String) => AVname(x: String)

    case TLPAREN() :: TNNAME(x: String) :: TRPAREN() => ANname(x: String)

    case TLPAREN() :: TDEF() :: f :: ls1 ::: ls2 ::: TRPAREN() =>
      BDef(f, apply(ls1), apply(ls2))

    case TLPAREN() :: TVAL() :: x :: ls ::: TRPAREN() =>
      BVal(x, apply(ls))

    case TLPAREN() :: TLAZYVAL() :: x :: ls ::: TRPAREN() =>
      BLval(x, apply(ls))

    case TINT(n: Int) => EInt(n: Int)

    case TFALSE() => EFalse()

    case TTRUE() => ETrue()

    case TLPAREN() :: TIF() :: ls1 ::: ls2 ::: ls3 ::: TRPAREN() =>
      EIf(apply(ls1), apply(ls2), apply(ls3))

    case TNIL() => ENil()

    case TLPAREN() :: TCONS() :: ls1 ::: ls2 ::: TRPAREN() =>
      ECons(apply(ls1), apply(ls2))

    case TLPAREN() :: TFST() :: ls ::: TRPAREN() =>
      EFst(apply(ls))

    case TLPAREN() :: TSND() :: ls ::: TRPAREN() =>
      ESnd(apply(ls))

    case TLPAREN() :: TMATCH() :: ls1 ::: ls2 ::: TRPAREN() =>
      EMatch(apply(ls1), apply(ls2))

    case TLPAREN() :: TLET() :: ls1 ::: ls2 ::: TRPAREN() =>
      ELet(apply(ls1), apply(ls2))

    case TLPAREN() :: TAPP() :: ls1 ::: ls2 ::: TRPAREN() =>
      EApp(apply(ls1), ls2.map(apply))

    case TLPAREN() :: TRMK() :: ls ::: TRPAREN() =>
      ERmk(ls)

    case TLPAREN() :: TRFD() :: ls1 ::: x :: TRPAREN() =>
      ERfd(apply(ls1), x)

    case TLPAREN() :: TPLUS() :: ls1 ::: ls2 ::: TRPAREN() =>
      EPlus(apply(ls1), apply(ls2))

    case TLPAREN() :: TMINUS() :: ls1 ::: ls2 ::: TRPAREN() =>
      EMinus(apply(ls1), apply(ls2))

    case TLPAREN() :: TMULT() :: ls1 ::: ls2 ::: TRPAREN() =>
      EMult(apply(ls1), apply(ls2))

    case TLPAREN() :: TEQ() :: ls1 ::: ls2 ::: TRPAREN() =>
      EEq(apply(ls1), apply(ls2))

    case TLPAREN() :: TLT() :: ls1 ::: ls2 ::: TRPAREN() =>
      ELt(apply(ls1), apply(ls2))

    case TLPAREN() :: TGT() :: ls1 ::: ls2 ::: TRPAREN() =>
      EGt(apply(ls1), apply(ls2))
  }
}
