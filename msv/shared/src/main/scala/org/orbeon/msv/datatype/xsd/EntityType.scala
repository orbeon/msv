/*
 * @(#)$Id: EntityType.java,v 1.25 2005/03/31 23:19:09 kohsuke Exp $
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
 * "ENTITY" type.
 * 
 * Type of the value object is `java.lang.String`.
 * See http://www.w3.org/TR/xmlschema-2/#ENTITY for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class EntityType private () extends BuiltinAtomicType("ENTITY"):

  final def getBaseType(): XSDatatype = NcnameType.theInstance

  override def isContextDependent(): Boolean = true

  final def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_LENGTH ||
       facetName == XSDatatype.FACET_MINLENGTH ||
       facetName == XSDatatype.FACET_MAXLENGTH ||
       facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_WHITESPACE ||
       facetName == XSDatatype.FACET_ENUMERATION then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

  def checkFormat(content: String, context: ValidationContext): Boolean =
    context.isUnparsedEntity(content)

  def _createValue(content: String, context: ValidationContext): AnyRef =
    if context.isUnparsedEntity(content) then content
    else null

  def getJavaObjectType(): Class[?] = classOf[String]

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case s: String => s
      case _         => throw new IllegalArgumentException()

object EntityType:
  val theInstance: EntityType = new EntityType()
