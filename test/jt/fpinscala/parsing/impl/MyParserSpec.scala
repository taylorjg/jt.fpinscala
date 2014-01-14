package jt.fpinscala.parsing.impl
import org.scalatest.FunSpec
import jt.fpinscala.parsing.impl.MyParser._

/**
 * Created by Jonathan Taylor on 14/01/2014.
 */
class MyParserSpec extends FunSpec {

  describe("Primitives") {

    it("string succeeding") {
      val actual = MyParser.run("abc")("abc")
      assert(actual == Right("abc"))
    }

    it("string succeeding reading partial input") {
      val actual = MyParser.run("abc")("abcdef")
      assert(actual == Right("abc"))
    }

    it("string failing") {
      val actual = MyParser.run("abc")("abx")
      assert(actual.isLeft)
      assert(MyParser.errorLocation(actual.left.get).offset == 0)
    }

    it("regex succeeding") {
      val actual = MyParser.run("""(\d)+""".r)("123")
      assert(actual == Right("123"))
    }

    it("regex succeeding reading partial input") {
      val actual = MyParser.run("""(\d)+""".r)("123abc")
      assert(actual == Right("123"))
    }

    it("regex failing") {
      val actual = MyParser.run("""(\d)+""".r)("abc")
      assert(actual.isLeft)
      assert(MyParser.errorLocation(actual.left.get).offset == 0)
    }
  }

  describe("Combinators") {

    it("char succeeding") {
      val actual = MyParser.run(char('a'))("a")
      assert(actual == Right('a'))
    }

    it("char failing") {
      val actual = MyParser.run(char('a'))("b")
      assert(actual.isLeft)
      assert(MyParser.errorLocation(actual.left.get).offset == 0)
    }
  }
}
