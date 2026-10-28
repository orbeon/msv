package org.orbeon.msv.datatype.regexp

import org.orbeon.msv.datatype.xsd.regex.{RegExp, RegExpFactory}
import java.text.ParseException

/**
 * {@link RegExpFactory} by the internal copy of Xerces.
 *
 * @author Kohsuke Kawaguchi
 */
final class InternalImpl extends RegExpFactory {

  @throws[ParseException]
  def compile(exp: String): RegExp = {
    val re = try {
      new RegularExpression(exp, "X")
    } catch {
      case e: org.orbeon.msv.datatype.regexp.ParseException =>
        throw new ParseException(e.getMessage, e.getLocation)
    }

    new RegExp {
      def matches(text: String): Boolean = re.matches(text)
    }
  }
}
