package org.orbeon.msv.grammar.relaxng.datatype

import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeBuilder, DatatypeException, ValidationContext}

/**
 * DataTypeBuilder implementation.
 *
 * There is no parameter for any built-in and compatibility types.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DatatypeBuilderImpl(private val baseType: Datatype) extends DatatypeBuilder {
  def createDatatype(): Datatype = baseType

  @throws(classOf[DatatypeException])
  def addParameter(name: String, value: String, context: ValidationContext): Unit =
    throw new DatatypeException("Parameters are not supported")
}
