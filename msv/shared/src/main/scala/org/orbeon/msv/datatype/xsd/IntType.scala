/*
 * @(#)$Id: IntType.java,v 1.21 2003/06/09 20:49:22 kk122374 Exp $
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
 * "int" type.
 * 
 * Type of the value object is `java.lang.Integer`.
 * See http://www.w3.org/TR/xmlschema-2/#int for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IntType protected (typeName: String, baseFacets: XSDatatypeImpl)
  extends IntegerDerivedType(typeName, baseFacets):

  def getBaseType(): XSDatatype = LongType.theInstance

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    IntType.load(lexicalValue)

  def getJavaObjectType(): Class[?] = classOf[java.lang.Integer]

object IntType:
  val theInstance: IntType =
    new IntType(
      "int",
      IntegerDerivedType.createRangeFacet(
        LongType.theInstance,
        java.lang.Integer.valueOf(java.lang.Integer.MIN_VALUE),
        java.lang.Integer.valueOf(java.lang.Integer.MAX_VALUE)
      )
    )

  def load(s: String): java.lang.Integer =
    try
      java.lang.Integer.valueOf(IntegerDerivedType.removeOptionalPlus(s))
    catch
      case _: NumberFormatException => null

  def save(v: java.lang.Integer): String =
    v.toString
