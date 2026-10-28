/*
 * @(#)$Id: SimpleURType.java,v 1.10 2005/03/31 23:19:10 kohsuke Exp $
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
 * Simple "ur-type" type.
 * 
 * Type of the value object is `java.lang.String`.
 * See http://www.w3.org/TR/xmlschema-1/#simple-ur-type-itself for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SimpleURType protected () extends BuiltinAtomicType("anySimpleType", WhiteSpaceProcessor.thePreserve):

  final def getBaseType(): XSDatatype = null

  final def checkFormat(content: String, context: ValidationContext): Boolean = true

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef = lexicalValue

  def getJavaObjectType(): Class[?] = classOf[String]

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case s: String => s
      case _         => throw new IllegalArgumentException()

  final def isFacetApplicable(facetName: String): Int = XSDatatype.APPLICABLE

object SimpleURType:
  val theInstance: SimpleURType = new SimpleURType()
