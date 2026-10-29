/*
 * @(#)$Id: DoubleType.java,v 1.22 2005/03/31 23:19:09 kohsuke Exp $
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
 * "double" type.
 * 
 * Type of the value object is `java.lang.Double`.
 * See http://www.w3.org/TR/xmlschema-2/#double for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DoubleType private () extends FloatingNumberType("double"):

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    DoubleType.load(lexicalValue)

  def getJavaObjectType(): Class[?] = classOf[java.lang.Double]

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case d: java.lang.Double => DoubleType.save(d)
      case _                   => throw new IllegalArgumentException()

object DoubleType:
  val theInstance: DoubleType = new DoubleType()

  def load(lexicalValue: String): java.lang.Double =
    try
      if lexicalValue == "NaN" then java.lang.Double.valueOf(java.lang.Double.NaN)
      else if lexicalValue == "INF" then java.lang.Double.valueOf(java.lang.Double.POSITIVE_INFINITY)
      else if lexicalValue == "-INF" then java.lang.Double.valueOf(java.lang.Double.NEGATIVE_INFINITY)
      else if lexicalValue.length == 0 ||
              !FloatingNumberType.isDigitOrPeriodOrSign(lexicalValue.charAt(0)) ||
              !FloatingNumberType.isDigitOrPeriodOrSign(lexicalValue.charAt(lexicalValue.length - 1)) ||
              FloatingNumberType.hasHexIndicator(lexicalValue) then
        null
      else
        java.lang.Double.valueOf(lexicalValue)
    catch
      case _: NumberFormatException => null

  def save(value: java.lang.Double): String =
    val v = value.doubleValue()
    if java.lang.Double.isNaN(v) then "NaN"
    else if v == java.lang.Double.POSITIVE_INFINITY then "INF"
    else if v == java.lang.Double.NEGATIVE_INFINITY then "-INF"
    else if v == 0.0 && (1.0 / v) < 0.0 then "-0.0"
    else value.toString
