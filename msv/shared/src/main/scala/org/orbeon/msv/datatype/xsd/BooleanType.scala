/*
 * @(#)$Id: BooleanType.java,v 1.27 2005/03/31 23:19:09 kohsuke Exp $
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
 * "boolean" type.
 * 
 * Type of the value object is `java.lang.Boolean`.
 * See http://www.w3.org/TR/xmlschema-2/#boolean for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class BooleanType private () extends BuiltinAtomicType("boolean"):

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  def checkFormat(content: String, context: ValidationContext): Boolean =
    "true" == content || "false" == content || "0" == content || "1" == content

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    BooleanType.load(lexicalValue)

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case b: java.lang.Boolean => BooleanType.save(b)
      case _                    => throw new IllegalArgumentException()

  def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_ENUMERATION ||
       facetName == XSDatatype.FACET_WHITESPACE then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

  def getJavaObjectType(): Class[?] = classOf[java.lang.Boolean]

object BooleanType:
  val theInstance: BooleanType = new BooleanType()

  def load(s: String): java.lang.Boolean =
    if "true" == s || "1" == s then java.lang.Boolean.TRUE
    else if "0" == s || "false" == s then java.lang.Boolean.FALSE
    else null

  def save(b: java.lang.Boolean): String =
    if b.booleanValue() then "true" else "false"
