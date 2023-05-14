package org.laplacetec.study
package interpreter
import org.laplacetec.study.interpreter.Main._
import org.laplacetec.study.interpreter.Value._
import org.laplacetec.study.interpreter.DataBundle._
import org.laplacetec.study.interpreter.DataBundle.ExprToString._
import org.laplacetec.study.interpreter._

object MainSpec extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  def run_eval(eval: Expr => Val)(code:String) : Val = {
    val tokens = ProjLexer(code)
    val e:Expr = Parser(tokens)
    eval(e)
  }

  def run_myeval = run_eval(myeval) _

  def run_test(implicit conv:ConvertToScala[Val]) = {
    try {
      println("=================")
      println("1. Basic Test")

      { // 1
        val code = "(fst (cons 1 2))"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(1) => true
          case _ => false
        }
        print_result(res)
      }

      { // 2
        val code = "(let ((val p (cons 1 (cons true nil)))) (cons 0 p))"
        val res = conv.toPair(run_myeval(code)) match {
          case Some(_) => true // this only checks whether the result is a pair.
          case _ => false
        }
        print_result(res)
      }

      { // 3
        val code = "(if true 10 20)"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(10) => true
          case _ => false
        }
        print_result(res)
      }

      { // 4
        val code = "(let ((def f (x (by-name y)) (+ x y))) (app f 2 3))"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(5) => true
          case _ => false
        }
        print_result(res)
      }

      { // 5
        val code = "(let ((def g () (+ 1 2))) (let ((val f g)) f))"
        val res = conv.isDef(run_myeval(code)) match {
          case true => true
          case false => false
        }
        print_result(res)
      }

      { // 6
        val code = "(let ((val a 10) (val b (+ a 1))) (* b 3))"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(33) => true
          case _ => false
        }
        print_result(res)
      }

      { // 7
        val code = "(let (def f (x) (if (= x 0) 0 (+ x (f (- x 1))))) (let (val g f) (g 5)))"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(15) => true
          case _ => false
        }
        print_result(res)
      }

      { // 8
        val code = "(match-list (cons 1 2) (+ 4 5) (hdx tly) (+ hdx tly))"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(3) => true
          case _ => false
        }
        print_result(res)
      }

      { // 9
        val code = "(let ((def x () b) (lazy val a (app x)) (val b 5)) a)"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(5) => true
          case _ => false
        }
        print_result(res)
      }

      { // 10
        val code = "(rfd (rmk (lazy-val abc (+ 3 4))) abc)"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(7) => true
          case _ => false
        }
        print_result(res)
      }

      { // 11
        val code = "(let ((def x () (cons a b)) (val a 5) (val b 3)) (let ((val y x) (val a 4)) (app y)))"
        val res = conv.toPair(run_myeval(code)) match {
          case Some(_) => true
          case _ => false
        }
        print_result(res)
      }
    } catch {
      case e : LexerException =>
        println("Lexer failed: " + e.msg)
      case e : ParserException =>
        println("Parser failed: " + e.msg)
      case e : EvalException =>
        println("myeval failed: " + e.msg)
    }

    try {
      println("=================")
      println("2. Infinite Lazy List Test")

      { // 1

        val code = "(let ((lazy-val inflst (let ((def x () (rmk (val hd 1) (lazy-val tl (app x))))) (app x)))) (rfd inflist hd))"
        val res = conv.toInt(run_myeval(code)) match {
          case Some(1) => true
          case _ => false
        }
        print_result(res)
      }

      { // 2
        val code = "(let ((lazy-val inflst (let ((def x () (rmk (val hd 1) (lazy-val tl (app x))))) (app x)))) (rfd inflist tl))"
        val res = conv.isRec(run_myeval(code)) match {
          case true => true
          case _ => false
        }
        print_result(res)
      }
    } catch {
      case e : LexerException =>
        println("Lexer failed: " + e.msg)
      case e : ParserException =>
        println("Parser failed: " + e.msg)
      case e : EvalException =>
        println("myeval failed: " + e.msg)
    }

    try {
      println("=================")
      println("3. Tailrec Test (should be finished)")
      val code = "(let ((def f (x n) (if (= x 0) n (app f (- x 1) (+ n x))))) (let ((val g f)) (app g 9999 0)))"
      val res = conv.toInt(run_myeval(code)) match {
        case Some(49995000) => true
        case _ => false
      }
      print_result(res)
    } catch {
      case e : LexerException =>
        println("Lexer failed: " + e.msg)
      case e : ParserException =>
        println("Parser failed: " + e.msg)
      case e : EvalException =>
        println("myeval failed: " + e.msg)
    }

    try {
      println("=================")
      println("4. Exception Handling Test")
      val code = "(try-catch (throw (if (> 3 4) 3 4)) (x) (+ x 1))"
      val res = conv.toInt(run_myeval(code)) match {
        case Some(5) => true
        case _ => false
      }
      print_result(res)
    } catch {
      case e : LexerException =>
        println("Lexer failed: " + e.msg)
      case e : ParserException =>
        println("Parser failed: " + e.msg)
      case e : EvalException =>
        println("myeval failed: " + e.msg)
    }

  }
  run_test
}
