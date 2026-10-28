/*
 * @(#)$Id: Proxy.java,v 1.8 2005/03/31 23:19:10 kohsuke Exp $
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
 * Delegates all methods to the base type.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class Proxy(
  nsUri: String,
  newTypeName: String,
  val baseType: XSDatatypeImpl
) extends XSDatatypeImpl(nsUri, newTypeName, baseType.whiteSpace):

  final def getBaseType(): XSDatatype = baseType

  override def isContextDependent(): Boolean = baseType.isContextDependent()

  override def getIdType(): Int = baseType.getIdType()

  def isFinal(derivationType: Int): Boolean = baseType.isFinal(derivationType)

  def getConcreteType(): ConcreteType = baseType.getConcreteType()

  def displayName(): String = baseType.displayName()

  def getVariety(): Int = baseType.getVariety()

  def isFacetApplicable(facetName: String): Int = baseType.isFacetApplicable(facetName)

  def checkFormat(content: String, context: ValidationContext): Boolean =
    baseType.checkFormat(content, context)

  def _createValue(content: String, context: ValidationContext): AnyRef =
    baseType._createValue(content, context)

  override def getFacetObject(facetName: String): DataTypeWithFacet =
    baseType.getFacetObject(facetName)

  def getJavaObjectType(): Class[?] = baseType.getJavaObjectType()

  def _createJavaObject(literal: String, context: ValidationContext): AnyRef =
    baseType._createJavaObject(literal, context)

  def serializeJavaObject(value: AnyRef, context: SerializationContext): String =
    baseType.serializeJavaObject(value, context)

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    baseType.convertToLexicalValue(value, context)

  @throws(classOf[DatatypeException])
  def _checkValid(content: String, context: ValidationContext): Unit =
    baseType._checkValid(content, context)
