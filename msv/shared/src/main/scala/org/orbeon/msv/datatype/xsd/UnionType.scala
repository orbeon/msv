/*
 * @(#)$Id: UnionType.java,v 1.26 2005/03/31 23:19:11 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

/**
 * Union type.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class UnionType(
  nsUri: String,
  newTypeName: String,
  memberTypes0: Array[XSDatatype]
) extends ConcreteType(nsUri, newTypeName):

  if memberTypes0.length == 0 then
    throw new DatatypeException(XSDatatypeImpl.localize(XSDatatypeImpl.ERR_EMPTY_UNION))

  val memberTypes: Array[XSDatatypeImpl] = new Array[XSDatatypeImpl](memberTypes0.length)
  System.arraycopy(memberTypes0, 0, memberTypes, 0, memberTypes0.length)

  var i = 0
  while i < memberTypes.length do
    if memberTypes(i).isFinal(XSDatatype.DERIVATION_BY_UNION) then
      throw new DatatypeException(
        XSDatatypeImpl.localize(XSDatatypeImpl.ERR_INVALID_MEMBER_TYPE, memberTypes(i).displayName())
      )
    i += 1

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  final override def displayName(): String =
    val name = getName
    if name != null then name
    else "union"

  override def isContextDependent(): Boolean =
    var j = 0
    while j < memberTypes.length do
      if memberTypes(j).isContextDependent() then return true
      j += 1
    false

  final override def getVariety(): Int = XSDatatype.VARIETY_UNION

  override def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_PATTERN || facetName == XSDatatype.FACET_ENUMERATION then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

  final def checkFormat(content: String, context: ValidationContext): Boolean =
    var j = 0
    while j < memberTypes.length do
      if memberTypes(j).checkFormat(content, context) then return true
      j += 1
    false

  def _createValue(content: String, context: ValidationContext): AnyRef =
    var j = 0
    while j < memberTypes.length do
      val o = memberTypes(j)._createValue(content, context)
      if o != null then return o
      j += 1
    null

  def getJavaObjectType(): Class[?] = classOf[AnyRef]

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(o: AnyRef, context: SerializationContext): String =
    var j = 0
    while j < memberTypes.length do
      try
        return memberTypes(j).convertToLexicalValue(o, context)
      catch
        case _: Exception => ()
      j += 1

    throw new IllegalArgumentException()

  @throws(classOf[DatatypeException])
  override def _checkValid(content: String, context: ValidationContext): Unit =
    if checkFormat(content, context) then ()
    else throw new DatatypeException()
