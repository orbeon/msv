package org.orbeon.msv.grammar.xmlschema

import org.orbeon.msv.grammar.NameClass

/**
 * internal representation of XPath ('aaa/bbb/ccc/ ... /eee').
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class XPath extends Serializable:
  var isAnyDescendant: Boolean = false
  var steps: Array[NameClass] = scala.compiletime.uninitialized
  var attributeStep: NameClass = scala.compiletime.uninitialized
