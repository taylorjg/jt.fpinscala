package jt.fpinscala.parsing

/**
 * Created by Jonathan Taylor on 14/01/2014.
 */
case class ParseError(stack: List[(Location, String)])
