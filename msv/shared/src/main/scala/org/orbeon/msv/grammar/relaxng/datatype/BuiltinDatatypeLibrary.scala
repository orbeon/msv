package org.orbeon.msv.grammar.relaxng.datatype

import org.orbeon.msv.datatype.xsd.{StringType, TokenType}
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeBuilder, DatatypeException, DatatypeLibrary}

/**
 * RELAX NG built-in datatypes.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class BuiltinDatatypeLibrary protected () extends DatatypeLibrary {
  @throws(classOf[DatatypeException])
  def createDatatype(name: String): Datatype = {
    if (name == "string") StringType.theInstance
    else if (name == "token") TokenType.theInstance
    else throw new DatatypeException("undefined built-in type:" + name)
  }

  @throws(classOf[DatatypeException])
  def createDatatypeBuilder(name: String): DatatypeBuilder =
    new DatatypeBuilderImpl(createDatatype(name))
}

object BuiltinDatatypeLibrary {
  val theInstance: BuiltinDatatypeLibrary = new BuiltinDatatypeLibrary()
}
