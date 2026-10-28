package org.orbeon.msv.grammar.xmlschema

/**
 * represents an identity constraint.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IdentityConstraint(
  val namespaceURI: String,
  val localName: String,
  val selectors: Array[XPath],
  val fields: Array[Field]
) extends Serializable
