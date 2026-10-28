package org.orbeon.msv.datatype.regexp

import org.orbeon.msv.datatype.xsd.regex.{RegExp, RegExpFactory}

/**
 * {@link RegExpFactory} by the internal copy of Xerces.
 *
 * @author Kohsuke Kawaguchi
 */
final class InternalImpl extends RegExpFactory {

  @throws[ParseException]
  def compile(exp: String): RegExp = {
    val re = new RegularExpression(exp, "X")
    new RegExp {
      def matches(text: String): Boolean = re.matches(text)
    }
  }
}
