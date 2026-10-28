package org.orbeon.msv.relaxng.datatype

/**
 * A Datatype library
 * 
 * @author <a href="mailto:jjc@jclark.com">James Clark</a>
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
trait DatatypeLibrary {

  /**
   * Creates a new instance of DatatypeBuilder.
   * 
   * The callee should throw a DatatypeException in case of an error.
   * 
   * @param baseTypeLocalName
   *   The local name of the base type.
   * 
   * @return
   *   A non-null valid datatype object.
   */
  @throws(classOf[DatatypeException])
  def createDatatypeBuilder(baseTypeLocalName: String): DatatypeBuilder

  /**
   * Gets or creates a pre-defined type.
   * 
   * This is just a short-cut of
   * <code>createDatatypeBuilder(typeLocalName).createDatatype();</code>
   * 
   * The callee should throw a DatatypeException in case of an error.
   * 
   * @return
   *   A non-null valid datatype object.
   */
  @throws(classOf[DatatypeException])
  def createDatatype(typeLocalName: String): Datatype
}
