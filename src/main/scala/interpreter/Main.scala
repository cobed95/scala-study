package org.laplacetec.study
package interpreter

import scala.annotation.tailrec
import org.laplacetec.study.interpreter.DataBundle._
import scala.collection.immutable._

object Value {

  // Environment

  sealed abstract class Val

  abstract class ConvertToScala[A] {
    def toInt(a:A) : Option[Int]
    def toBool(a:A) : Option[Boolean]
    def toPair(a:A) : Option[(A, A)]
    def isNil(a:A) : Boolean
    def isDef(a:A) : Boolean
    def isRec(a:A) : Boolean
  }

  // implicit val valConv : ConvertToScala[Val]
}

object Main {
  import Value._

  class EvalException(val msg: String) extends Exception

  def myeval(e:Expr) : Val = throw new EvalException("Not implemented yet")

}