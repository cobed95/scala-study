package org.laplacetec.study
package project.stackcalc

sealed abstract class Bop
case class OpAdd() extends Bop
case object OpAdd extends Bop
case class OpSub() extends Bop
case object OpSub extends Bop
case class OpMul() extends Bop
case object OpMul extends Bop

sealed abstract class Token
case class TkBop(o: Bop) extends Token
case class TkInt(n: Int) extends Token
case class TkLPar() extends Token
case object TkLPar extends Token
case class TkRPar() extends Token
case object TkRPar extends Token

sealed abstract class Exp
case class EInt(i: Int) extends Exp
case class EOp(o: Bop, lhs: Exp, rhs: Exp) extends Exp
case class EError() extends Exp
case object EError extends Exp
