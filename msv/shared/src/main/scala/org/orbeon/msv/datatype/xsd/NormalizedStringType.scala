/*
 * @(#)$Id: NormalizedStringType.java,v 1.13 2003/06/09 20:49:23 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * "normalizedString" type.
 * 
 * Type of the value object is `java.lang.String`.
 * See http://www.w3.org/TR/xmlschema-2/#normalizedString for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NormalizedStringType protected (typeName: String, isAlwaysValid: Boolean)
  extends StringType(typeName, WhiteSpaceProcessor.theReplace, isAlwaysValid):

  override def getBaseType(): XSDatatype = StringType.theInstance

object NormalizedStringType:
  val theInstance: NormalizedStringType =
    new NormalizedStringType("normalizedString", true)
