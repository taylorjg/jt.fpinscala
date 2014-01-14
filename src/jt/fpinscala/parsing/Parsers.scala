package jt.fpinscala.parsing
import scala.util.matching.Regex

/**
 * Created by Jonathan Taylor on 14/01/2014.
 */

trait Parsers[Parser[+_]] { self =>

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  // Primitives
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
  def succeed[A](a: A): Parser[A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def attempt[A](p: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a => map(p2)(b => (a, b)))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p1)(a => map(p2)(b => f(a, b)))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def errorLocation(e: ParseError): Location =
    e.stack.lastOption.getOrElse((Location("???"), "<no error>"))._1

  def errorMessage(e: ParseError): String =
    e.stack.lastOption.getOrElse((Location("???"), "<no error>"))._2

  case class ParserOps[A](p: Parser[A]) {
    def  |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
    def many: Parser[List[A]] = self.many(p)
    def many1(): Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def succeed(a: A) = self.succeed(a)
    def slice: Parser[String] = self.slice(p)
    def      **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
    def attempt: Parser[A] = self.attempt(p)
  }
}
