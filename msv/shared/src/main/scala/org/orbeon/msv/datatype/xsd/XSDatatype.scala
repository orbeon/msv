/*
 * @(#)$Id: XSDatatype.java,v 1.22 2005/03/31 23:19:11 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.DatabindableDatatype
import org.orbeon.msv.datatype.SerializationContext

import java.io.Serializable

/**
 * Publicly accessible interface of W3C XML Schema datatype (simple type).
 * 
 * <p>
 * The most important methods are defined in the
 * <code>org.relaxng.datatype.Datatype</code> interface.
 * This interface provides additional information which is not covered by
 * {@link org.relaxng.datatype.Datatype} interface.
 * Also, this interface provides various simple type component properties
 * which are defined in the spec.
 * 
 * <p>
 * Application can use this interface to interact with datatype objects.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait XSDatatype extends Serializable with DatabindableDatatype:

  /**
   * Gets the local name of the type.
   * If this type is an anonymous type, then this method returns null.
   */
  def getName: String

  /**
   * Gets the namespace URI of this simple type definition.
   */
  def getNamespaceUri: String

  /**
   * Gets the displayable name of this type.
   * This method always returns something. It is useful to provide a message to the user.
   */
  def displayName(): String

  /**
   * Converts value object back to the corresponding value in the lexical space.
   */
  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(valueObject: AnyRef, context: SerializationContext): String

  /**
   * Gets the variety of this simple type.
   * 
   * @return VARIETY_ATOMIC, VARIETY_LIST, or VARIETY_UNION.
   */
  def getVariety(): Int

  /**
   * Checks if this type is declared as final for the specified kind of derivation.
   * 
   * @param derivationType one of pre-defined values (DERIVATION_BY_XXX).
   */
  def isFinal(derivationType: Int): Boolean

  /**
   * Returns if the specified facet is applicable to this datatype.
   * 
   * @return APPLICABLE, FIXED, or NOT_ALLOWED
   */
  def isFacetApplicable(facetName: String): Int

  /**
   * Gets the names of all applicable facets.
   */
  def getApplicableFacetNames(): Array[String]

  /**
   * Gets the facet object that restricts the specified facet.
   *
   * @return null if no such facet object exists.
   */
  def getFacetObject(facetName: String): DataTypeWithFacet

  /**
   * Gets the base type of this type.
   * 
   * This method returns null if this object represents the simple ur-type.
   */
  def getBaseType(): XSDatatype

  /**
   * Gets the nearest ancestor built-in type.
   */
  def getAncestorBuiltinType(): XSDatatype

  /**
   * Tests if this type is a derived type of the specified type.
   */
  def isDerivedTypeOf(baseType: XSDatatype, restrictionAllowed: Boolean): Boolean

  /**
   * Returns true if this datatype is known to accept any string.
   */
  def isAlwaysValid(): Boolean

object XSDatatype:
  val VARIETY_ATOMIC: Int = 1
  val VARIETY_LIST: Int   = 2
  val VARIETY_UNION: Int  = 3

  val DERIVATION_BY_RESTRICTION: Int = 0x01
  val DERIVATION_BY_LIST: Int        = 0x02
  val DERIVATION_BY_UNION: Int       = 0x04

  val APPLICABLE: Int  = 0
  val FIXED: Int       = -1
  val NOT_ALLOWED: Int = -2

  val FACET_LENGTH: String         = "length"
  val FACET_MINLENGTH: String      = "minLength"
  val FACET_MAXLENGTH: String      = "maxLength"
  val FACET_PATTERN: String        = "pattern"
  val FACET_ENUMERATION: String    = "enumeration"
  val FACET_TOTALDIGITS: String    = "totalDigits"
  val FACET_FRACTIONDIGITS: String = "fractionDigits"
  val FACET_MININCLUSIVE: String   = "minInclusive"
  val FACET_MAXINCLUSIVE: String   = "maxInclusive"
  val FACET_MINEXCLUSIVE: String   = "minExclusive"
  val FACET_MAXEXCLUSIVE: String   = "maxExclusive"
  val FACET_WHITESPACE: String     = "whiteSpace"

  val XMLSCHEMA_NSURI: String = "http://www.w3.org/2001/XMLSchema"
