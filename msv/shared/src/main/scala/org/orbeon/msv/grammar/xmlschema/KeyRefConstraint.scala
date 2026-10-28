package org.orbeon.msv.grammar.xmlschema

/**
 * keyref constraint.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class KeyRefConstraint(
  namespaceURI: String,
  localName: String,
  selectors: Array[XPath],
  fields: Array[Field]
) extends IdentityConstraint(namespaceURI, localName, selectors, fields):

  var key: KeyConstraint = scala.compiletime.uninitialized
