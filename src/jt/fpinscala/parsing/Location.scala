package jt.fpinscala.parsing

/**
 * Created by Jonathan Taylor on 14/01/2014.
 */
case class Location(input: String, offset: Int = 0, lastCharsConsumed: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n, lastCharsConsumed = n)

  def current = input.substring(offset)
}
