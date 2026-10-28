/*
 * @(#)$Id: ErrorType.java,v 1.5 2005/03/31 23:19:10 kohsuke Exp $
 *
 * Copyright 2002 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * A dummy datatype that can be used to recover from errors.
 * This datatype accepts any values and any facets.
 * The TypeIncubator class also recognizes this class and
 * any operation on ErrorType will never cause any error and
 * silently return another ErrorType.
 * 
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
class ErrorType protected () extends BuiltinAtomicType("error"):

  def _createValue(content: String, context: ValidationContext): AnyRef = this

  def checkFormat(literal: String, context: ValidationContext): Boolean = true

  def convertToLexicalValue(valueObject: AnyRef, context: SerializationContext): String = ""

  def isFacetApplicable(facetName: String): Int = XSDatatype.APPLICABLE

  def getBaseType(): XSDatatype = SimpleURType.theInstance

  def getJavaObjectType(): Class[?] = getClass

object ErrorType:
  val theInstance: ErrorType = new ErrorType()
