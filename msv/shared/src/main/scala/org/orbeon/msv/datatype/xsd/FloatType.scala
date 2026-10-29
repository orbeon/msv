/*
 * @(#)$Id: FloatType.java,v 1.22 2005/03/31 23:19:10 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * "float" type.
 * 
 * Type of the value object is `java.lang.Float`.
 * See http://www.w3.org/TR/xmlschema-2/#float for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class FloatType private () extends FloatingNumberType("float"):

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    FloatType.load(lexicalValue)

  def getJavaObjectType(): Class[?] = classOf[java.lang.Float]

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case f: java.lang.Float => FloatType.save(f)
      case _                  => throw new IllegalArgumentException()

object FloatType:
  val theInstance: FloatType = new FloatType()

  def load(s: String): java.lang.Float =
    try
      if s == "NaN" then java.lang.Float.valueOf(java.lang.Float.NaN)
      else if s == "INF" then java.lang.Float.valueOf(java.lang.Float.POSITIVE_INFINITY)
      else if s == "-INF" then java.lang.Float.valueOf(java.lang.Float.NEGATIVE_INFINITY)
      else if s.length == 0 ||
              !FloatingNumberType.isDigitOrPeriodOrSign(s.charAt(0)) ||
              !FloatingNumberType.isDigitOrPeriodOrSign(s.charAt(s.length - 1)) ||
              FloatingNumberType.hasHexIndicator(s) then
        null
      else
        java.lang.Float.valueOf(s)
    catch
      case _: NumberFormatException => null

  def save(value: java.lang.Float): String =
    val v = value.floatValue()
    if java.lang.Float.isNaN(v) then "NaN"
    else if v == java.lang.Float.POSITIVE_INFINITY then "INF"
    else if v == java.lang.Float.NEGATIVE_INFINITY then "-INF"
    else if v == 0.0f && (1.0f / v) < 0.0f then "-0.0"
    else value.toString
