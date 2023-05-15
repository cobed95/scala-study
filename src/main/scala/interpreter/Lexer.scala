package org.laplacetec.study
package interpreter

import scala.util.parsing._
import scala.util.parsing.combinator._
import org.laplacetec.study.interpreter.DataBundle._

class LexerException(val msg: String) extends Exception

sealed trait Token

// Arg
case class TVNAME(str:String) extends Token
case class TNNAME(str:String) extends Token

// Bind
case class TDEF() extends Token
case class TVAL() extends Token
case class TLAZYVAL() extends Token

// Expr
case class TINT(n:Int) extends Token
case class TTRUE() extends Token
case class TFALSE() extends Token
case class TNIL() extends Token

case class TIF() extends Token
case class TCONS() extends Token
case class TFST() extends Token
case class TSND() extends Token
case class TAPP() extends Token
case class TLET() extends Token
case class TMATCH() extends Token
case class TRMK() extends Token
case class TRFD() extends Token

case class TPLUS() extends Token
case class TMINUS() extends Token
case class TMULT() extends Token
case class TEQ() extends Token
case class TLT() extends Token
case class TGT() extends Token
case class TTHROW() extends Token
case class TTRYCATCH() extends Token

case class TLPAREN() extends Token
case class TRPAREN() extends Token

object ProjLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f\n]+".r

  def tint: Parser[TINT] = {
    """(0|[1-9]\d*)""".r ^^ { n => TINT(n.toInt) }
  }
  def ttrue: Parser[TTRUE] = {
    "true" ^^ { _ => TTRUE() }
  }
  def tfalse: Parser[TFALSE] = {
    "false" ^^ { _ => TFALSE() }
  }
  def tnil: Parser[TNIL] = {
    "nil" ^^ { _ => TNIL() }
  }

  def tvname: Parser[TVNAME] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => TVNAME(str) }
  }
  def tnname: Parser[TNNAME] = {
    "by-name" ~ "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { case bynm ~ str => TNNAME(str) }
  }
  def tif: Parser[TIF] = {
    "if" ^^ { _ => TIF() }
  }
  def tcons: Parser[TCONS] = {
    "cons" ^^ { _ => TCONS() }
  }
  def tfst: Parser[TFST] = {
    "fst" ^^ { _ => TFST() }
  }
  def tsnd: Parser[TSND] = {
    "snd" ^^ { _ => TSND() }
  }
  def tapp: Parser[TAPP] = {
    "app" ^^ { _ => TAPP() }
  }
  def tlet: Parser[TLET] = {
    "let" ^^ { _ => TLET() }
  }
  def tmatch: Parser[TMATCH] = {
    "match-list" ^^ { _ => TMATCH() }
  }
  def trmk: Parser[TRMK] = {
    "rmk" ^^ { _ => TRMK() }
  }
  def trfd: Parser[TRFD] = {
    "rfd" ^^ { _ => TRFD() }
  }

  def tdef: Parser[TDEF] = {
    "def" ^^ { _ => TDEF() }
  }
  def tval: Parser[TVAL] = {
    "val" ^^ { _ => TVAL() }
  }
  def tlazyval: Parser[TLAZYVAL] = {
    "lazy-val" ^^ { _ => TLAZYVAL() }
  }

  def tplus: Parser[TPLUS] = {
    "+" ^^ { _ => TPLUS() }
  }
  def tminus: Parser[TMINUS] = {
    "-" ^^ { _ => TMINUS() }
  }
  def tmult: Parser[TMULT] = {
    "*" ^^ { _ => TMULT() }
  }
  def teq: Parser[TEQ] = {
    "=" ^^ { _ => TEQ() }
  }
  def tlt: Parser[TLT] = {
    "<" ^^ { _ => TLT() }
  }
  def tgt: Parser[TGT] = {
    ">" ^^ { _ => TGT() }
  }
  def tthrow: Parser[TTHROW] = {
    "throw" ^^ { _ => TTHROW() }
  }
  def ttrycatch: Parser[TTRYCATCH] = {
    "try-catch" ^^ { _ => TTRYCATCH() }
  }

  def tlparen: Parser[TLPAREN] = {
    "(" ^^ { _ => TLPAREN() }
  }
  def trparen: Parser[TRPAREN] = {
    ")" ^^ { _ => TRPAREN() }
  }

  def tokens: Parser[List[Token]] = {
    phrase(rep1(tint|ttrue|tfalse|tnil|
      tif|tcons|tfst|tsnd|tapp|tlet|tmatch|trmk|trfd|tdef|tval|tlazyval|
      tplus|tminus|tmult|teq|tlt|tgt|tthrow|ttrycatch|tlparen|trparen|
      tnname|tvname)) ^^ { t => t }
  }

  def apply(code: String): List[Token] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => throw new LexerException(msg)
      case Success(result, next) => result
    }
  }

}
