package org.orbeon.msv.reader.datatype

import org.orbeon.msv.grammar.Expression

/**
 * State can implement this method to be notified by DataType vocabulary
 * about the result of parsing.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait TypeOwner:
  def onEndChildType(datatype: Expression, typeName: String): Unit
