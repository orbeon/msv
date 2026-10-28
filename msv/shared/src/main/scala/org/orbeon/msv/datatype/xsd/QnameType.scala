/*
 * @(#)$Id: QnameType.java,v 1.34 2005/03/31 23:19:10 kohsuke Exp $
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
 * "QName" type.
 * 
 * Type of the value object is {@link QnameValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#QName for the spec.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class QnameType private () extends BuiltinAtomicType("QName") with Discrete:

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  override def isContextDependent(): Boolean = true

  def checkFormat(value: String, context: ValidationContext): Boolean =
    val first = value.indexOf(':')
    if first <= 0 then
      XmlNames.isUnqualifiedName(value)
    else
      val last = value.lastIndexOf(':')
      if last != first then return false

      val prefix = value.substring(0, first)
      XmlNames.isUnqualifiedName(prefix) &&
        XmlNames.isUnqualifiedName(value.substring(first + 1)) &&
        context.resolveNamespacePrefix(prefix) != null

  def _createValue(value: String, context: ValidationContext): AnyRef =
    val first = value.indexOf(':')
    val (uri, localPart) =
      if first <= 0 then
        if !XmlNames.isUnqualifiedName(value) then return null
        (context.resolveNamespacePrefix(""), value)
      else
        val last = value.lastIndexOf(':')
        if last != first then return null

        val prefix = value.substring(0, first)
        val lp = value.substring(first + 1)

        if !XmlNames.isUnqualifiedName(prefix) || !XmlNames.isUnqualifiedName(lp) then
          return null

        (context.resolveNamespacePrefix(prefix), lp)

    if uri == null then null
    else new QnameValueType(uri, localPart)

  final def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_ENUMERATION ||
       facetName == XSDatatype.FACET_WHITESPACE ||
       facetName == XSDatatype.FACET_LENGTH ||
       facetName == XSDatatype.FACET_MAXLENGTH ||
       facetName == XSDatatype.FACET_MINLENGTH then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

  final def countLength(value: AnyRef): Int =
    val v = value.asInstanceOf[QnameValueType]
    UnicodeUtil.countLength(v.namespaceURI) + UnicodeUtil.countLength(v.localPart)

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(o: AnyRef, context: SerializationContext): String =
    o match
      case v: QnameValueType => serialize(v.namespaceURI, v.localPart, context)
      case _                 => throw new UnsupportedOperationException()

  override def serializeJavaObject(value: AnyRef, context: SerializationContext): String =
    value match
      case input: Array[String] if input.length == 2 =>
        serialize(input(0), input(1), context)
      case _ =>
        throw new IllegalArgumentException()

  private def serialize(uri: String, local: String, context: SerializationContext): String =
    val prefix = context.getNamespacePrefix(uri)
    if prefix == null then local
    else prefix + ":" + local

  override def _createJavaObject(literal: String, context: ValidationContext): AnyRef =
    val v = createValue(literal, context).asInstanceOf[QnameValueType]
    if v == null then null
    else Array(v.namespaceURI, v.localPart)

  def getJavaObjectType(): Class[?] = classOf[Array[String]]

object QnameType:
  val theInstance: QnameType = new QnameType()
