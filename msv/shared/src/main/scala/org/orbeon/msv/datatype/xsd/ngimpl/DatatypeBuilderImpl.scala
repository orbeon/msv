/*
 * @(#)$Id: DatatypeBuilderImpl.java,v 1.13 2005/03/31 23:19:08 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.ngimpl

import org.orbeon.msv.datatype.xsd.{TypeIncubator, XSDatatype, XSDatatypeImpl}
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeBuilder, DatatypeException, ValidationContext}

class DatatypeBuilderImpl(baseType: XSDatatype) extends DatatypeBuilder:

  private var incubator: TypeIncubator = new TypeIncubator(baseType)

  @throws(classOf[DatatypeException])
  def addParameter(name: String, value: String, context: ValidationContext): Unit =
    if name == XSDatatype.FACET_ENUMERATION then
      // the enumeration facet is not allowed for RELAX NG.
      throw new DatatypeException(
        XSDatatypeImpl.localize(XSDatatypeImpl.ERR_NOT_APPLICABLE_FACET, name)
      )

    incubator.addFacet(name, value, false, context)

    if name == XSDatatype.FACET_PATTERN then
      // if the pattern facet is specified, we have to derive a new type
      // so that multiple pattern facets can work as ANDing.
      incubator = new TypeIncubator(incubator.derive(null, null))

  @throws(classOf[DatatypeException])
  def createDatatype(): Datatype =
    incubator.derive(null, null)
