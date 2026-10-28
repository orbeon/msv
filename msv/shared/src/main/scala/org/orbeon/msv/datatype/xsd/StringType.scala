/*
 * @(#)$Id: StringType.java,v 1.23 2005/03/31 23:19:10 kohsuke Exp $
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
 * "string" type.
 * 
 * Type of the value object is `java.lang.String`.
 * See http://www.w3.org/TR/xmlschema-2/#string for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class StringType(
  typeName: String,
  whiteSpace: WhiteSpaceProcessor,
  private val _isAlwaysValid: Boolean
) extends BuiltinAtomicType(typeName, whiteSpace) with Discrete:

  protected def this(typeName: String, whiteSpace: WhiteSpaceProcessor) =
    this(typeName, whiteSpace, false)

  def getBaseType(): XSDatatype = SimpleURType.theInstance

  final def checkFormat(content: String, context: ValidationContext): Boolean =
    _createValue(content, context) != null

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    lexicalValue

  def getJavaObjectType(): Class[?] = classOf[String]

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case s: String => s
      case _         => throw new IllegalArgumentException()

  final def countLength(value: AnyRef): Int =
    UnicodeUtil.countLength(value.asInstanceOf[String])

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

  override def isAlwaysValid(): Boolean = _isAlwaysValid

object StringType:
  val theInstance: StringType =
    new StringType("string", WhiteSpaceProcessor.thePreserve, true)
