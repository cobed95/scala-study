package org.laplacetec.study
package interpreter

object DataBundle {

  sealed abstract class Arg
  case class AVname(x:String) extends Arg
  case class ANname(x:String) extends Arg

  sealed abstract class Bind
  case class BDef(f:String, params:List[Arg], e:Expr) extends Bind
  case class BVal(x:String, e:Expr) extends Bind
  case class BLval(x:String, e:Expr) extends Bind

  sealed abstract class Expr
  case class EInt(n:Int) extends Expr
  case class ETrue() extends Expr
  case class EFalse() extends Expr
  case class ENil() extends Expr
  case class EName(s:String) extends Expr
  case class EIf(econd:Expr, et:Expr, ef:Expr) extends Expr
  case class ECons(eh:Expr, et:Expr) extends Expr
  case class EFst(el:Expr) extends Expr
  case class ESnd(el:Expr) extends Expr
  case class EApp(ef:Expr, eargs:List[Expr]) extends Expr
  case class ELet(bs:List[Bind], eb:Expr) extends Expr
  case class EMatch(e1:Expr, e2:Expr, hd:String, tl:String, e3:Expr) extends Expr
  case class ERmk(bs:List[Bind]) extends Expr
  case class ERfd(rec:Expr, fd:String) extends Expr
  case class EPlus(e1:Expr, e2:Expr) extends Expr
  case class EMinus(e1:Expr, e2:Expr) extends Expr
  case class EMult(e1:Expr, e2:Expr) extends Expr
  case class EEq(e1:Expr, e2:Expr) extends Expr
  case class ELt(e1:Expr, e2:Expr) extends Expr
  case class EGt(e1:Expr, e2:Expr) extends Expr

  // Before implement problem 3(at Problem 1 2),
  // any kinds of outputs about these two cases(EThrow, ETrycatch) will be accepted by tests.
  case class EThrow (e:Expr) extends Expr
  case class ETrycatch (e1:Expr, x:String, e2:Expr) extends Expr

  object ExprToString {
    def arg_toString (a:Arg) : String =
      a match {
        case AVname(s) => s.toString
        case ANname(s) => inparen("by-name" + s.toString)
      }

    def arg_toString_list (al:List[Arg]) : String = {
      concat(al.map(arg_toString))
    }

    def concat(sl:List[String]) : String = {
      def cc_intl(l:List[String]):String =
        l match {
          case Nil => ""
          case h::t => " " + h + cc_intl(t)
        }
      sl match {
        case Nil => ""
        case h::t => h + cc_intl(t)
      }
    }

    def inparen(s:String):String = "(" + s + ")"

    def expr_toString (e:Expr) : String =
      e match {
        case EInt(n) => n.toString
        case ETrue() => "true"
        case EFalse() => "false"
        case ENil() => "nil"
        case EName(s) => s
        case EIf(e1, e2, e3) => inparen(concat(List("if", expr_toString(e1), expr_toString(e2), expr_toString(e3))))
        case ECons(e1, e2) => inparen(concat(List("cons", expr_toString(e1), expr_toString(e2))))
        case EFst(e1) => inparen(concat(List("fst", expr_toString(e1))))
        case ESnd(e1) => inparen(concat(List("snd", expr_toString(e1))))
        case EApp(e1, el) => {
          inparen(concat(List("app", expr_toString_list(e1::el))))
        }
        case ELet(b, e1) => {
          val sl = inparen(binds_toString(b))
          inparen(concat(List("let", sl, expr_toString(e1))))
        }
        case EMatch(e1, e2, hd, tl, e3) => {
          val hdtl = inparen(concat(List(hd, tl)))
          inparen(concat(List("match-list", expr_toString(e1), expr_toString(e2), hdtl, expr_toString(e3))))
        }
        case ERmk(bs) =>
          inparen(concat(List("rmk", binds_toString(bs))))
        case ERfd(e, x) => {
          val access = inparen(concat(List(x, expr_toString(e))))
          inparen(concat(List("rfd", access)))
        }
        case EPlus(e1, e2) =>
          inparen(concat(List("+", expr_toString(e1), expr_toString(e2))))
        case EMinus(e1, e2) =>
          inparen(concat(List("-", expr_toString(e1), expr_toString(e2))))
        case EMult(e1, e2) =>
          inparen(concat(List("*", expr_toString(e1), expr_toString(e2))))
        case EEq(e1, e2) =>
          inparen(concat(List("=", expr_toString(e1), expr_toString(e2))))
        case ELt(e1, e2) =>
          inparen(concat(List("<", expr_toString(e1), expr_toString(e2))))
        case EGt(e1, e2) =>
          inparen(concat(List(">", expr_toString(e1), expr_toString(e2))))
        case EThrow(e) =>
          inparen(concat(List("throw", expr_toString(e))))
        case ETrycatch(e1, x, e2) => {
          val catcher = inparen(concat(List(x, expr_toString(e2))))
          inparen(concat(List("try-catch", expr_toString(e1), catcher)))
        }
      }

    def expr_toString_list(el:List[Expr]):String =
      concat(el.map(expr_toString))

    def binds_toString(bl:List[Bind]) : String = {
      def bind_toString(b:Bind):String =
        b match {
          case BDef(f, params, e) => inparen(concat(List("def", f, arg_toString_list(params), expr_toString(e))))
          case BVal(x, e) => inparen(concat(List("val", x, expr_toString(e))))
          case BLval(x, e) => inparen(concat(List("lazy-val", x, expr_toString(e))))
        }
      concat(bl.map(bind_toString))
    }
  }

}
