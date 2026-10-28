/*
 * @(#)$Id: ListType.java,v 1.31 2005/03/31 23:19:10 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeException, ValidationContext}

import java.util.ArrayList

/**
 * List type.
 * 
 * Type of the value object is {@link ListValueType}.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class ListType(
  nsUri: String,
  newTypeName: String,
  val itemType: XSDatatypeImpl
) extends ConcreteType(nsUri, newTypeName) with Discrete:

  if itemType.isFinal(XSDatatype.DERIVATION_BY_LIST) then
    throw new DatatypeException(XSDatatypeImpl.localize(XSDatatypeImpl.ERR_INVALID_ITEMTYPE))

  final override def displayName(): String =
    val name = getName
    if name != null then name
    else itemType.displayName() + "-list"

  final override def getVariety(): Int = XSDatatype.VARIETY_LIST

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  override def isContextDependent(): Boolean = itemType.isContextDependent()

  override def getIdType(): Int =
    itemType.getIdType() match
      case Datatype.ID_TYPE_NULL  => Datatype.ID_TYPE_NULL
      case Datatype.ID_TYPE_ID    => Datatype.ID_TYPE_NULL
      case Datatype.ID_TYPE_IDREF => Datatype.ID_TYPE_IDREFS
      case Datatype.ID_TYPE_IDREFS => Datatype.ID_TYPE_IDREFS
      case _ => throw new Error()

  final override def isFinal(derivationType: Int): Boolean =
    if derivationType == XSDatatype.DERIVATION_BY_LIST then true
    else itemType.isFinal(derivationType)

  override def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_LENGTH ||
       facetName == XSDatatype.FACET_MINLENGTH ||
       facetName == XSDatatype.FACET_MAXLENGTH ||
       facetName == XSDatatype.FACET_ENUMERATION ||
       facetName == XSDatatype.FACET_PATTERN then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

  private def tokenize(content: String): Array[String] =
    val list = new ArrayList[String]()
    val len = content.length
    var i = 0
    while i < len do
      while i < len && WhiteSpaceProcessor.isWhiteSpace(content.charAt(i)) do
        i += 1
      val start = i
      while i < len && !WhiteSpaceProcessor.isWhiteSpace(content.charAt(i)) do
        i += 1
      if start < i then
        list.add(content.substring(start, i))
    list.toArray(new Array[String](list.size()))

  final def checkFormat(content: String, context: ValidationContext): Boolean =
    val tokens = tokenize(content)
    var i = 0
    while i < tokens.length do
      if !itemType.isValid(tokens(i), context) then return false
      i += 1
    true

  def _createValue(content: String, context: ValidationContext): AnyRef =
    val tokens = tokenize(content)
    val values = new Array[AnyRef](tokens.length)
    var i = 0
    while i < tokens.length do
      val v = itemType._createValue(tokens(i), context)
      if v == null then return null
      values(i) = v
      i += 1
    new ListValueType(values)

  def getJavaObjectType(): Class[?] = classOf[Array[AnyRef]]

  final def countLength(value: AnyRef): Int =
    value.asInstanceOf[ListValueType].values.length

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case lv: ListValueType =>
        val r = new java.lang.StringBuilder()
        var i = 0
        while i < lv.values.length do
          if i != 0 then r.append(' ')
          r.append(itemType.convertToLexicalValue(lv.values(i), context))
          i += 1
        r.toString
      case _ => throw new IllegalArgumentException()

  @throws(classOf[DatatypeException])
  override def _checkValid(content: String, context: ValidationContext): Unit =
    val tokens = tokenize(content)
    var i = 0
    while i < tokens.length do
      itemType.checkValid(tokens(i), context)
      i += 1
