/*
 * @(#)$Id: ShortType.java,v 1.20 2003/06/09 20:49:24 kk122374 Exp $
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
 * "short" type.
 * 
 * Type of the value object is `java.lang.Short`.
 * See http://www.w3.org/TR/xmlschema-2/#short for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ShortType protected (typeName: String, baseFacets: XSDatatypeImpl)
  extends IntegerDerivedType(typeName, baseFacets):

  def getBaseType(): XSDatatype = IntType.theInstance

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    ShortType.load(lexicalValue)

  def getJavaObjectType(): Class[?] = classOf[java.lang.Short]

object ShortType:
  val theInstance: ShortType =
    new ShortType(
      "short",
      IntegerDerivedType.createRangeFacet(
        IntType.theInstance,
        java.lang.Short.valueOf(java.lang.Short.MIN_VALUE),
        java.lang.Short.valueOf(java.lang.Short.MAX_VALUE)
      )
    )

  def load(s: String): java.lang.Short =
    try
      java.lang.Short.valueOf(IntegerDerivedType.removeOptionalPlus(s))
    catch
      case _: NumberFormatException => null

  def save(v: java.lang.Short): String =
    v.toString
