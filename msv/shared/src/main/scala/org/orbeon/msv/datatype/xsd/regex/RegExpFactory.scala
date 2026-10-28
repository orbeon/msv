package org.orbeon.msv.datatype.xsd.regex

import org.orbeon.msv.datatype.regexp.InternalImpl
import java.text.ParseException

// Just return the internal implementation, as that's the one that would be found anyway
abstract class RegExpFactory {
  @throws[ParseException]
  def compile(exp: String): RegExp
}

object RegExpFactory {
  def createFactory(): RegExpFactory = new InternalImpl()
}
