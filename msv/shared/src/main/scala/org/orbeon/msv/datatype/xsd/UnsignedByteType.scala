/*
 * @(#)$Id: UnsignedByteType.java,v 1.18 2003/06/09 20:49:24 kk122374 Exp $
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
 * "unsignedByte" type.
 * 
 * Type of the value object is `java.lang.Short`.
 * See http://www.w3.org/TR/xmlschema-2/#unsignedByte for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class UnsignedByteType private () extends ShortType(
  "unsignedByte",
  IntegerDerivedType.createRangeFacet(
    UnsignedShortType.theInstance,
    null,
    java.lang.Short.valueOf(255.toShort)
  )
):

  final override def getBaseType(): XSDatatype = UnsignedShortType.theInstance

  override def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    try
      val v = super._createValue(lexicalValue, context).asInstanceOf[java.lang.Short]
      if v == null then null
      else if v.shortValue() < 0 then null
      else if v.shortValue() > UnsignedByteType.upperBound then null
      else v
    catch
      case _: NumberFormatException => null

object UnsignedByteType:
  val theInstance: UnsignedByteType = new UnsignedByteType()
  private val upperBound: Short = 255
