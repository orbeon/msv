/*
 * @(#)$Id: ConcreteType.java,v 1.30 2005/03/31 23:19:09 kohsuke Exp $
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
 * Base class for types that union/list/atomic.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ConcreteType(
  nsUri: String,
  typeName: String,
  whiteSpace: WhiteSpaceProcessor
) extends XSDatatypeImpl(nsUri, typeName, whiteSpace):

  def this(nsUri: String, typeName: String) =
    this(nsUri, typeName, WhiteSpaceProcessor.theCollapse)

  final def getConcreteType(): ConcreteType = this

  def isFinal(derivationType: Int): Boolean = false

  @throws(classOf[DatatypeException])
  def _checkValid(content: String, context: ValidationContext): Unit =
    if checkFormat(content, context) then return

    throw new DatatypeException(
      DatatypeException.UNKNOWN,
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_INAPPROPRIATE_FOR_TYPE, content, getName)
    )

  def _createJavaObject(literal: String, context: ValidationContext): AnyRef =
    _createValue(literal, context)

  def serializeJavaObject(value: AnyRef, context: SerializationContext): String =
    val literal = convertToLexicalValue(value, context)
    if !isValid(literal, XSDatatypeImpl.serializedValueChecker) then null
    else literal
