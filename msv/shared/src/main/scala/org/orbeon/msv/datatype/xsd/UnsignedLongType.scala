/*
 * @(#)$Id: UnsignedLongType.java,v 1.16 2003/06/09 20:49:24 kk122374 Exp $
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
 * "unsignedLong" type.
 * 
 * Type of the value object is {@link IntegerValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#unsignedLong for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class UnsignedLongType private () extends IntegerType(
  "unsignedLong",
  IntegerDerivedType.createRangeFacet(
    NonNegativeIntegerType.theInstance,
    null,
    IntegerValueType.create("18446744073709551615")
  )
):

  final override def getBaseType(): XSDatatype = NonNegativeIntegerType.theInstance

  override def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    val v = IntegerValueType.create(lexicalValue)
    if v == null then return null
    if !v.isNonNegative then return null
    if UnsignedLongType.upperBound.compareTo(v) < 0 then return null
    v

object UnsignedLongType:
  val theInstance: UnsignedLongType = new UnsignedLongType()

  private val upperBound: IntegerValueType =
    IntegerValueType.create("18446744073709551615")
