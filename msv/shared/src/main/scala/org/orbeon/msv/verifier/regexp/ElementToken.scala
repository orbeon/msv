package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.ElementExp

/**
 * a token that represents an XML element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementToken(val acceptedPatterns: Array[ElementExp]) extends Token {
  override def `match`(exp: ElementExp): Boolean = {
    var i = 0
    while (i < acceptedPatterns.length) {
      if (acceptedPatterns(i) eq exp) return true
      i += 1
    }
    false
  }

  override def toString: String = {
    var s = "ElementToken"
    var i = 0
    while (i < acceptedPatterns.length) {
      s += "/" + acceptedPatterns(i).getNameClass.toString
      i += 1
    }
    s
  }
}
