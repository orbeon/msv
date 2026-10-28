/*
 * @(#)$Id: BinaryBaseType.java,v 1.19 2005/03/31 23:19:09 kohsuke Exp $
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
 * Base implementation for "hexBinary" and "base64Binary" types.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class BinaryBaseType(typeName: String)
  extends BuiltinAtomicType(typeName) with Discrete:

  final def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_LENGTH ||
       facetName == XSDatatype.FACET_MAXLENGTH ||
       facetName == XSDatatype.FACET_MINLENGTH ||
       facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_WHITESPACE ||
       facetName == XSDatatype.FACET_ENUMERATION then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  final def countLength(value: AnyRef): Int =
    value.asInstanceOf[BinaryValueType].rawData.length

  override def _createJavaObject(literal: String, context: ValidationContext): AnyRef =
    val v = createValue(literal, context).asInstanceOf[BinaryValueType]
    if v == null then null
    else v.rawData

  override def serializeJavaObject(value: AnyRef, context: SerializationContext): String

  def getJavaObjectType(): Class[?] = classOf[Array[Byte]]
