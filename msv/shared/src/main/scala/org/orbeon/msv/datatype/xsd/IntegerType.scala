/*
 * @(#)$Id: IntegerType.java,v 1.25 2005/03/31 23:19:10 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

import java.math.BigInteger

/**
 * "integer" type.
 * 
 * Type of the value object is {@link IntegerValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#integer for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IntegerType protected (typeName: String, baseFacets: XSDatatypeImpl)
  extends IntegerDerivedType(typeName, baseFacets):

  def getBaseType(): XSDatatype = NumberType.theInstance

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    IntegerValueType.create(lexicalValue)

  override def _createJavaObject(literal: String, context: ValidationContext): AnyRef =
    val o = _createValue(literal, context).asInstanceOf[IntegerValueType]
    if o == null then null
    else new BigInteger(o.toString)

  def getJavaObjectType(): Class[?] = classOf[BigInteger]

object IntegerType:
  val theInstance: IntegerType =
    try
      new IntegerType(
        "integer",
        new FractionDigitsFacet(null, null, NumberType.theInstance, 0, true)
      )
    catch
      case _: DatatypeException =>
        throw new InternalError()

  def load(s: String): BigInteger =
    val o = IntegerValueType.create(s)
    if o == null then null
    else new BigInteger(o.toString)

  def save(v: BigInteger): String =
    v.toString
