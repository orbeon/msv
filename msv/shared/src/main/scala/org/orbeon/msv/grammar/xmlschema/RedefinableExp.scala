package org.orbeon.msv.grammar.xmlschema

import org.orbeon.msv.grammar.ReferenceExp

/**
 * declaration that can be redefined by using &lt;redefine&gt; element.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class RedefinableExp(typeLocalName: String) extends ReferenceExp(typeLocalName):

  /** clones this object. */
  def getClone(): RedefinableExp

  /**
   * assigns contents of rhs to this object.
   */
  def redefine(rhs: RedefinableExp): Unit =
    if (this.getClass ne rhs.getClass) || this.name != rhs.name then
      throw new IllegalArgumentException()
    this.exp = rhs.exp
