/*
 * @(#)$Id: ByteType.java,v 1.23 2003/06/09 20:49:20 kk122374 Exp $
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
 * "byte" type.
 * 
 * Type of the value object is `java.lang.Byte`.
 * See http://www.w3.org/TR/xmlschema-2/#byte for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ByteType private () extends IntegerDerivedType(
  "byte",
  IntegerDerivedType.createRangeFacet(
    ShortType.theInstance,
    java.lang.Byte.valueOf(java.lang.Byte.MIN_VALUE),
    java.lang.Byte.valueOf(java.lang.Byte.MAX_VALUE)
  )
):

  final def getBaseType(): XSDatatype = ShortType.theInstance

  def _createValue(content: String, context: ValidationContext): AnyRef =
    ByteType.load(content)

  def getJavaObjectType(): Class[?] = classOf[java.lang.Byte]

object ByteType:
  val theInstance: ByteType = new ByteType()

  def load(s: String): java.lang.Byte =
    try
      java.lang.Byte.valueOf(IntegerDerivedType.removeOptionalPlus(s))
    catch
      case _: NumberFormatException => null

  def save(v: java.lang.Byte): String =
    v.toString
