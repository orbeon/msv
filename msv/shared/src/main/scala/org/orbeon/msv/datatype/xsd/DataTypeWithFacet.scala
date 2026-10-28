/*
 * @(#)$Id: DataTypeWithFacet.java,v 1.34 2005/03/31 23:19:09 kohsuke Exp $
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
 * Base implementation of facet-restricted datatype.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class DataTypeWithFacet(
  nsUri: String,
  typeName: String,
  val baseType: XSDatatypeImpl,
  val facetName: String,
  val isFacetFixed: Boolean,
  whiteSpace: WhiteSpaceProcessor
) extends XSDatatypeImpl(nsUri, typeName, whiteSpace):

  def this(
    nsUri: String,
    typeName: String,
    baseType: XSDatatypeImpl,
    facetName: String,
    isFacetFixed: Boolean
  ) =
    this(nsUri, typeName, baseType, facetName, isFacetFixed, baseType.whiteSpace)

  protected val concreteType: ConcreteType = baseType.getConcreteType()
  private val needValueCheckFlag: Boolean  = baseType.needValueCheck()

  baseType.isFacetApplicable(facetName) match
    case XSDatatype.APPLICABLE => ()
    case XSDatatype.NOT_ALLOWED =>
      throw new DatatypeException(XSDatatypeImpl.localize(XSDatatypeImpl.ERR_NOT_APPLICABLE_FACET, facetName))
    case XSDatatype.FIXED =>
      throw new DatatypeException(XSDatatypeImpl.localize(XSDatatypeImpl.ERR_OVERRIDING_FIXED_FACET, facetName))
    case _ => ()

  final def getBaseType(): XSDatatype = baseType

  override def isContextDependent(): Boolean = concreteType.isContextDependent()

  override def getIdType(): Int = concreteType.getIdType()

  def displayName(): String =
    if getName != null then getName
    else concreteType.getName + "-derived"

  def isFacetApplicable(facetName: String): Int =
    if this.facetName == facetName then
      if isFacetFixed then XSDatatype.FIXED
      else XSDatatype.APPLICABLE
    else
      baseType.isFacetApplicable(facetName)

  override def needValueCheck(): Boolean = needValueCheckFlag

  override def getFacetObject(facetName: String): DataTypeWithFacet =
    if this.facetName == facetName then this
    else baseType.getFacetObject(facetName)

  final def getConcreteType(): ConcreteType = concreteType

  final def getVariety(): Int = concreteType.getVariety()

  def isFinal(derivationType: Int): Boolean = baseType.isFinal(derivationType)

  final def convertToLexicalValue(o: AnyRef, context: SerializationContext): String =
    concreteType.convertToLexicalValue(o, context)

  final def getJavaObjectType(): Class[?] = concreteType.getJavaObjectType()

  final def _createJavaObject(literal: String, context: ValidationContext): AnyRef =
    if isValid(literal, context) then
      baseType.createJavaObject(literal, context)
    else
      null

  def serializeJavaObject(value: AnyRef, context: SerializationContext): String =
    baseType.serializeJavaObject(value, context)

  final def _checkValid(content: String, context: ValidationContext): Unit =
    // let the base type complain first.
    baseType._checkValid(content, context)
    // then see if the facet is satisfied.
    diagnoseByFacet(content, context)

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit
