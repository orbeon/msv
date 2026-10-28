/*
 * @(#)$Id: TokenType.java,v 1.13 2003/06/09 20:49:24 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * "token" type.
 * 
 * Type of the value object is `java.lang.String`.
 * See http://www.w3.org/TR/xmlschema-2/#token for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TokenType protected (typeName: String, isAlwaysValid: Boolean)
  extends StringType(typeName, WhiteSpaceProcessor.theCollapse, isAlwaysValid):

  override def getBaseType(): XSDatatype = NormalizedStringType.theInstance

object TokenType:
  val theInstance: TokenType = new TokenType("token", true)
