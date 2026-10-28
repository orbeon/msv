package org.orbeon.msv.relaxng.datatype.helpers

import org.orbeon.msv.relaxng.datatype.*

/**
 * Dummy implementation of {@link DatatypeBuilder}.
 * 
 * This implementation can be used for Datatypes which have no parameters.
 * Any attempt to add parameters will be rejected.
 * 
 * <p>
 * Typical usage would be:
 * <PRE><XMP>
 * class MyDatatypeLibrary implements DatatypeLibrary {
 *     ....
 *     DatatypeBuilder createDatatypeBuilder( String typeName ) {
 *         return new ParameterleessDatatypeBuilder(createDatatype(typeName));
 *     }
 *     ....
 * }
 * </XMP></PRE>
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
final class ParameterlessDatatypeBuilder(
  /** This type object is returned for the derive method. */
  private val baseType: Datatype
) extends DatatypeBuilder {

  @throws(classOf[DatatypeException])
  def addParameter(name: String, strValue: String, context: ValidationContext): Unit =
    throw new DatatypeException()

  @throws(classOf[DatatypeException])
  def createDatatype(): Datatype =
    baseType
}
