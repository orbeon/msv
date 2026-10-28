package org.orbeon.msv.grammar.xmlschema

/**
 * key constraint.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class KeyConstraint(
  namespaceURI: String,
  localName: String,
  selectors: Array[XPath],
  fields: Array[Field]
) extends IdentityConstraint(namespaceURI, localName, selectors, fields)
