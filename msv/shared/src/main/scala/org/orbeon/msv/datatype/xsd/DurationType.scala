/*
 * @(#)$Id: DurationType.java,v 1.25 2005/03/31 23:19:09 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.datatype.xsd.datetime.{BigTimeDurationValueType, ITimeDurationValueType}
import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * "duration" type.
 * 
 * Type of the value object is {@link ITimeDurationValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#duration for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class DurationType private () extends BuiltinAtomicType("duration") with Comparator:

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  def checkFormat(content: String, context: ValidationContext): Boolean =
    try
      new BigTimeDurationValueType(content)
      true
    catch
      case _: IllegalArgumentException => false

  def _createValue(content: String, context: ValidationContext): AnyRef =
    try
      new BigTimeDurationValueType(content)
    catch
      case _: IllegalArgumentException => null

  def getJavaObjectType(): Class[?] = classOf[ITimeDurationValueType]

  def compare(lhs: AnyRef, rhs: AnyRef): Int =
    lhs.asInstanceOf[ITimeDurationValueType].compare(rhs.asInstanceOf[ITimeDurationValueType])

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

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case _: ITimeDurationValueType => value.toString
      case _                         => throw new IllegalArgumentException()

object DurationType:
  val theInstance: DurationType = new DurationType()
