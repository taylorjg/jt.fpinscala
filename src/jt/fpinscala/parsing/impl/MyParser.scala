package jt.fpinscala.parsing.impl
import jt.fpinscala.parsing._
import MyParserTypes._
import scala.util.matching.Regex

/**
 * Created by Jonathan Taylor on 14/01/2014.
 */
object MyParserTypes {
  type Parser[+A] = Location => Result[A]
  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

object MyParser extends Parsers[Parser] {

  def run[A](p: Parser[A])(s: String): Either[ParseError, A] =
    p(Location(s)) match {
      case Success(a, _) => Right(a)
      case Failure(e) => Left(e)
    }

  implicit def string(s: String): Parser[String] =
    l => if (l.current.startsWith(s))
      Success(s, s.length)
    else
      Failure(l.toError(s"Expected input matching string '$s'"))

  implicit def regex(r: Regex): Parser[String] =
    l => r findPrefixOf l.current match {
      case Some(a) => Success(a, a.length)
      case _ => Failure(l.toError(s"Expected input matching regex '$r'"))
    }

  def slice[A](p: Parser[A]): Parser[String] =
    l => p(l) match {
      case Success(_, n) => Success(l.current.take(n), n)
      case f @ Failure(_) => f
    }

  def succeed[A](a: A): Parser[A] =
    l => Success(a, l.lastCharsConsumed)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    l => p(l) match {
      case Success(a, n) => f(a)(l.advanceBy(n))
      case f @ Failure(pe) => f
    }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    l => p1(l) match {
      case s1 @ Success(_, _) => s1
      case f1 @ Failure(_) => p2(l) match {
        case s2 @ Success(_, _) => s2
        case f2 => f1 // or f2 ???
      }
    }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???
}
