package org.orbeon.msv.reader.datatype.xsd

import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

/**
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait XSTypeIncubator:
  @throws(classOf[DatatypeException])
  def addFacet(name: String, value: String, fixed: Boolean, context: ValidationContext): Unit

  @throws(classOf[DatatypeException])
  def derive(newTypeUri: String, newTypeName: String): XSDatatypeExp
