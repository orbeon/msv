/*
 * @(#)$Id: UnsignedIntType.java,v 1.17 2003/06/09 20:49:24 kk122374 Exp $
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
 * "unsignedInt" type.
 * 
 * Type of the value object is `java.lang.Long`.
 * See http://www.w3.org/TR/xmlschema-2/#unsignedInt for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class UnsignedIntType private () extends LongType(
  "unsignedInt",
  IntegerDerivedType.createRangeFacet(
    UnsignedLongType.theInstance,
    null,
    java.lang.Long.valueOf(4294967295L)
  )
):

  final override def getBaseType(): XSDatatype = UnsignedLongType.theInstance

  override def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    try
      val v = super._createValue(lexicalValue, context).asInstanceOf[java.lang.Long]
      if v == null then null
      else if v.longValue() < 0 then null
      else if v.longValue() > UnsignedIntType.upperBound then null
      else v
    catch
      case _: NumberFormatException => null

object UnsignedIntType:
  val theInstance: UnsignedIntType = new UnsignedIntType()
  private val upperBound: Long = 4294967295L
