package org.orbeon.msv.grammar.xmlschema

/**
 * represents one field of an identity constraint.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class Field extends Serializable:
  var paths: Array[XPath] = scala.compiletime.uninitialized
