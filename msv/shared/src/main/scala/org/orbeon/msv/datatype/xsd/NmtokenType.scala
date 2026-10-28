/*
 * @(#)$Id: NmtokenType.java,v 1.17 2003/06/09 20:49:23 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * "NMTOKEN" type.
 * 
 * Type of the value object is `java.lang.String`.
 * See http://www.w3.org/TR/xmlschema-2/#NMTOKEN for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NmtokenType protected (typeName: String) extends TokenType(typeName, false):

  final override def getBaseType(): XSDatatype = TokenType.theInstance

  override def _createValue(content: String, context: ValidationContext): AnyRef =
    if XmlNames.isNmtoken(content) then content
    else null

object NmtokenType:
  val theInstance: NmtokenType = new NmtokenType("NMTOKEN")
