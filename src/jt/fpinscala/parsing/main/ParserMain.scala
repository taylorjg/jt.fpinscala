package jt.fpinscala.parsing.main
import jt.fpinscala.parsing.impl.MyParser
import jt.fpinscala.parsing.impl.MyParser._

/**
 * Created by Jonathan Taylor on 14/01/2014.
 */
object ParserMain {
  def main(args: Array[String]): Unit = {

    // char
    println("char")
    println(MyParser.run(char('a'))("a"))
    println(MyParser.run(char('a'))("b"))

    // string
    println("string")
    println(MyParser.run("abc")("abc"))
    println(MyParser.run("abc")("def"))

    // regex
    println("regex")
    println(MyParser.run("""(\d)+""".r)("123"))
    println(MyParser.run("""(\d)+""".r)("a123"))

    // slice
    println("slice")
    println(MyParser.run(slice("black"))("blackbird"))
    println(MyParser.run(slice("black"))("bluebird"))

    // succeed
    println("succeed")
    println(MyParser.run(succeed("this"))("that"))

    // or
    println("or")
    println(MyParser.run(or("jig", "saw"))("jig"))
    println(MyParser.run(or("jig", "saw"))("saw"))
    println(MyParser.run(or("jig", "saw"))("band"))

    // flatMap
    val flatMapParser = flatMap( """\d""".r) {
      case "1" => "blue"
      case "2" => "red"
      case _ => "?"
    }
    println("flatMap")
    println(MyParser.run(flatMapParser)("1blue"))
    println(MyParser.run(flatMapParser)("2red"))
    println(MyParser.run(flatMapParser)("3?"))
    println(MyParser.run(flatMapParser)("2pink"))

    // label
    println("label")

    // scope
    println("scope")

    // attempt
    println("attempt")

    // listOfN
    println("listOfN")
    println(MyParser.run(listOfN(5, """\d""".r))("0123456789"))
    println(MyParser.run(listOfN(5, """\d""".r))("abcdefghij"))
    println(MyParser.run(listOfN(5, "a"))("aabbb"))
    println(MyParser.run(listOfN(5, char('a')))("aabbb"))

    // many
    println("many")
    println(MyParser.run(many("ha!"))("ha!ha!ha!"))
    println(MyParser.run(many("ha!"))("boo!boo!"))

    // many1
    println("many1")
    println(MyParser.run(many1("ha!"))("ha!ha!"))
    println(MyParser.run(many1("ha!"))("ha!"))
    println(MyParser.run(many1("ha!"))(""))

    // product
    println("product")
    println(MyParser.run("jig" ** "saw")("jigsaw"))
    println(MyParser.run("jig" ** "saw")("jiggle"))

    // exercise 6
    val ex6Parser = flatMap("""(\d)+""".r)(s => listOfN(s.toInt, char('a')))
    println("exercise 6")
    println(MyParser.run(ex6Parser)("1a"))
    println(MyParser.run(ex6Parser)("2aa"))
    println(MyParser.run(ex6Parser)("3aaa"))
    println(MyParser.run(ex6Parser)("12aaaaaaaaaaaa"))
    println(MyParser.run(ex6Parser)("4aab"))
  }
}
