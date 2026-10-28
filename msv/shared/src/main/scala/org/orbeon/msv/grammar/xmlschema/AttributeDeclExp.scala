package org.orbeon.msv.grammar.xmlschema

import org.orbeon.msv.grammar.{AttributeExp, ReferenceExp}

/**
 * global attribute declaration.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeDeclExp(typeLocalName: String) extends ReferenceExp(typeLocalName):

  var self: AttributeExp = scala.compiletime.uninitialized

  def set(exp: AttributeExp): Unit =
    self = exp
    this.exp = self
