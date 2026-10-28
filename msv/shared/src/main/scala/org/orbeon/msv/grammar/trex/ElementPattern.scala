package org.orbeon.msv.grammar.trex

import org.orbeon.msv.grammar.{ElementExp, Expression, NameClass}

/**
 * &lt;element&gt; pattern of TREX.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementPattern(val nameClass: NameClass, contentModel: Expression)
  extends ElementExp(contentModel, false) {

  def getNameClass: NameClass = nameClass
}
