package org.orbeon.msv.datatype.regexp

import org.orbeon.msv.datatype.xsd.regex.RegExpFactory
import utest.*

object RegexTest extends TestSuite {

  val tests = Tests {
    test("basic regular expressions") {
      val re = new RegularExpression("abc")
      assert(re.matches("abc"))
      assert(re.matches("abcd")) // non-XML mode matches if contains pattern
      assert(!re.matches("ab"))

      val reAnchored = new RegularExpression("^abc$")
      assert(reAnchored.matches("abc"))
      assert(!reAnchored.matches("abcd"))
    }

    test("xml schema mode options") {
      val re = new RegularExpression("[a-z]+", "X")
      assert(re.matches("hello"))
      assert(!re.matches("Hello"))
      assert(!re.matches("123"))
    }

    test("xml schema character class subtraction") {
      // lower-case consonants only: [a-z-[aeiou]]
      val re = new RegularExpression("[a-z-[aeiou]]+", "X")
      assert(re.matches("bcdfghjklmnpqrstvwxyz"))
      assert(!re.matches("a"))
      assert(!re.matches("e"))
      assert(!re.matches("hello"))
    }

    test("xml schema unicode categories and blocks") {
      val reDigit = new RegularExpression("\\p{Nd}+", "X")
      assert(reDigit.matches("1234567890"))
      assert(!reDigit.matches("abc"))

      val reLatin = new RegularExpression("\\p{IsBasicLatin}+", "X")
      assert(reLatin.matches("Hello World!"))
    }

    test("xml schema XML name chars") {
      // \i for initial name char, \c for name char
      val reName = new RegularExpression("\\i\\c*", "X")
      assert(reName.matches("foo_bar-123"))
      assert(reName.matches(":foo"))
      assert(!reName.matches("123foo"))
      assert(!reName.matches("-foo"))
    }

    test("RegExpFactory compilation") {
      val factory = RegExpFactory.createFactory()
      val regex = factory.compile("[A-Z]{3}-[0-9]{4}")
      assert(regex.matches("XYZ-1234"))
      assert(!regex.matches("XY-1234"))
      assert(!regex.matches("XYZ-123"))
      assert(!regex.matches("xyz-1234"))
    }
    test("xml schema literal dollar and caret with quantifiers") {
      // In XML Schema regex, $ and ^ are normal characters and can have quantifiers (?, +, *, {min,max})
      val reDollar = new RegularExpression("[$]?([A-Z]+)$?[0-9]+", "X")
      assert(reDollar.matches("A1"))
      assert(reDollar.matches("$A1"))
      assert(reDollar.matches("A$1"))
      assert(reDollar.matches("$A$1"))

      val reCaret = new RegularExpression("^[A-Z]+^?[0-9]+", "X")
      assert(reCaret.matches("^A1"))
      assert(reCaret.matches("^A^1"))
    }
  }
}
