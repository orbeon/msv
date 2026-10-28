package org.orbeon.msv.datatype.xsd.regex

/**
 * @author Kohsuke Kawaguchi
 */
trait RegExp {
  def matches(text: String): Boolean
}
