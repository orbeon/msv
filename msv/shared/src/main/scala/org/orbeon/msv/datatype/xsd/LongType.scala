/*
 * @(#)$Id: LongType.java,v 1.20 2003/06/09 20:49:22 kk122374 Exp $
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
 * "long" type.
 * 
 * Type of the value object is `java.lang.Long`.
 * See http://www.w3.org/TR/xmlschema-2/#long for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class LongType protected (typeName: String, baseFacets: XSDatatypeImpl)
  extends IntegerDerivedType(typeName, baseFacets):

  private def this() =
    this(
      "long",
      IntegerDerivedType.createRangeFacet(
        IntegerType.theInstance,
        java.lang.Long.valueOf(java.lang.Long.MIN_VALUE),
        java.lang.Long.valueOf(java.lang.Long.MAX_VALUE)
      )
    )

  def getBaseType(): XSDatatype = IntegerType.theInstance

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    LongType.load(lexicalValue)

  def getJavaObjectType(): Class[?] = classOf[java.lang.Long]

object LongType:
  val theInstance: LongType = new LongType()

  def load(s: String): java.lang.Long =
    try
      java.lang.Long.valueOf(IntegerDerivedType.removeOptionalPlus(s))
    catch
      case _: NumberFormatException => null

  def save(v: java.lang.Long): String =
    v.toString
