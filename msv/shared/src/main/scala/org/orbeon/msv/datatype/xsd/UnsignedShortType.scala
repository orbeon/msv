/*
 * @(#)$Id: UnsignedShortType.java,v 1.17 2003/06/09 20:49:24 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * "unsignedShort" type.
 * 
 * Type of the value object is `java.lang.Integer`.
 * See http://www.w3.org/TR/xmlschema-2/#unsignedShort for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class UnsignedShortType private () extends IntType(
  "unsignedShort",
  IntegerDerivedType.createRangeFacet(
    UnsignedIntType.theInstance,
    null,
    java.lang.Integer.valueOf(65535)
  )
):

  override def getBaseType(): XSDatatype = UnsignedIntType.theInstance

  override def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    try
      val v = super._createValue(lexicalValue, context).asInstanceOf[java.lang.Integer]
      if v == null then null
      else if v.intValue() < 0 then null
      else if v.intValue() > UnsignedShortType.upperBound then null
      else v
    catch
      case _: NumberFormatException => null

object UnsignedShortType:
  val theInstance: UnsignedShortType = new UnsignedShortType()
  private val upperBound: Int = 65535
