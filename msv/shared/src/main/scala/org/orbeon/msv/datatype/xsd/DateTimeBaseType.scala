/*
 * @(#)$Id: DateTimeBaseType.java,v 1.34 2005/03/31 23:19:09 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.datatype.xsd.datetime.{CalendarFormatter, CalendarParser, IDateTimeValueType, PreciseCalendarFormatter, PreciseCalendarParser}
import org.orbeon.msv.relaxng.datatype.ValidationContext

import java.util.Calendar

/**
 * Base implementation of dateTime and dateTime-truncated types.
 * This class uses IDateTimeValueType as the value object.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class DateTimeBaseType(typeName: String)
  extends BuiltinAtomicType(typeName) with Comparator:

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  final def checkFormat(content: String, context: ValidationContext): Boolean =
    try
      PreciseCalendarParser.parse(getFormat(), content)
      true
    catch
      case _: IllegalArgumentException => false

  final def _createValue(content: String, context: ValidationContext): AnyRef =
    try
      PreciseCalendarParser.parse(getFormat(), content)
    catch
      case _: IllegalArgumentException => null

  @throws(classOf[IllegalArgumentException])
  final def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case dtv: IDateTimeValueType =>
        PreciseCalendarFormatter.format(getFormat(), dtv)
      case _ =>
        throw new IllegalArgumentException()

  final override def _createJavaObject(literal: String, context: ValidationContext): AnyRef =
    try
      CalendarParser.parse(getFormat(), literal)
    catch
      case _: IllegalArgumentException => null

  override def serializeJavaObject(value: AnyRef, context: SerializationContext): String =
    value match
      case cal: Calendar => CalendarFormatter.format(getFormat(), cal)
      case _             => throw new IllegalArgumentException()

  def getJavaObjectType(): Class[?] = classOf[Calendar]

  protected def getFormat(): String

  def compare(lhs: AnyRef, rhs: AnyRef): Int =
    lhs.asInstanceOf[IDateTimeValueType].compare(rhs.asInstanceOf[IDateTimeValueType])

  final def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_ENUMERATION ||
       facetName == XSDatatype.FACET_WHITESPACE ||
       facetName == XSDatatype.FACET_MAXINCLUSIVE ||
       facetName == XSDatatype.FACET_MAXEXCLUSIVE ||
       facetName == XSDatatype.FACET_MININCLUSIVE ||
       facetName == XSDatatype.FACET_MINEXCLUSIVE then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED
