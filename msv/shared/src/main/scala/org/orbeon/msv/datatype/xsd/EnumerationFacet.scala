/*
 * @(#)$Id: EnumerationFacet.java,v 1.24 2005/03/31 23:19:09 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

import java.util.{Collection => JCollection, HashSet => JHashSet, Set => JSet}

/**
 * "enumeration" facets validator.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class EnumerationFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  _values: JCollection[?],
  isFixed: Boolean
) extends DataTypeWithValueConstraintFacet(nsUri, typeName, baseType, XSDatatype.FACET_ENUMERATION, isFixed):

  /** Set of valid values */
  val values: JSet[AnyRef] = new JHashSet[AnyRef](_values.asInstanceOf[JCollection[AnyRef]])

  def _createValue(literal: String, context: ValidationContext): AnyRef =
    val o = baseType._createValue(literal, context)
    if o == null || !values.contains(o) then null
    else o

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit =
    if _createValue(content, context) != null then return

    if values.size <= 4 then
      val members = values.toArray()
      if members.length > 0 && (members(0).isInstanceOf[String] || members(0).isInstanceOf[Number]) then
        var r = "\"" + members(0).toString + "\""
        var i = 1
        while i < members.length do
          r += "/\"" + members(i).toString + "\""
          i += 1
        r = "(" + r + ")"
        throw new DatatypeException(
          DatatypeException.UNKNOWN,
          XSDatatypeImpl.localize(XSDatatypeImpl.ERR_ENUMERATION_WITH_ARG, r)
        )

    throw new DatatypeException(
      DatatypeException.UNKNOWN,
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_ENUMERATION)
    )
