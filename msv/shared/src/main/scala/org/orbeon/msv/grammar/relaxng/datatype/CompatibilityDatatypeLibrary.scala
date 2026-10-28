package org.orbeon.msv.grammar.relaxng.datatype

import org.orbeon.msv.datatype.xsd.{DatatypeFactory, IDREFType, IDType}
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeBuilder, DatatypeException, DatatypeLibrary}

/**
 * RELAX NG DTD compatibility datatype library.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class CompatibilityDatatypeLibrary extends DatatypeLibrary {
  @throws(classOf[DatatypeException])
  def createDatatype(name: String): Datatype = {
    if (name == "ID") IDType.theInstance
    else if (name == "IDREF") IDREFType.theInstance
    else if (name == "IDREFS") DatatypeFactory.getTypeByName("IDREFS")
    else throw new DatatypeException("undefined built-in type:" + name)
  }

  @throws(classOf[DatatypeException])
  def createDatatypeBuilder(name: String): DatatypeBuilder =
    new DatatypeBuilderImpl(createDatatype(name))
}

object CompatibilityDatatypeLibrary {
  val namespaceURI: String = "http://relaxng.org/ns/compatibility/datatypes/1.0"
}
