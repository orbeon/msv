/*
 * @(#)$Id: PositiveIntegerType.java,v 1.17 2003/06/09 20:49:23 kk122374 Exp $
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
 * "positiveInteger" type.
 * 
 * Type of the value object is {@link IntegerValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#positiveInteger for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class PositiveIntegerType private () extends IntegerType(
  "positiveInteger",
  IntegerDerivedType.createRangeFacet(
    NonNegativeIntegerType.theInstance,
    IntegerValueType.create("1"),
    null
  )
):

  final override def getBaseType(): XSDatatype = NonNegativeIntegerType.theInstance

  override def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    val o = super._createValue(lexicalValue, context)
    if o == null then return null

    val v = o.asInstanceOf[IntegerValueType]
    if !v.isPositive then null
    else v

object PositiveIntegerType:
  val theInstance: PositiveIntegerType = new PositiveIntegerType()
