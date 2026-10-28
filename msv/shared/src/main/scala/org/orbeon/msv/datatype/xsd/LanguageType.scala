/*
 * @(#)$Id: LanguageType.java,v 1.18 2003/06/09 20:49:22 kk122374 Exp $
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
 * "language" type.
 * 
 * Type of the value object is `java.lang.String`.
 * See http://www.w3.org/TR/xmlschema-2/#language for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class LanguageType private () extends TokenType("language", false):

  final override def getBaseType(): XSDatatype = TokenType.theInstance

  override def _createValue(content: String, context: ValidationContext): AnyRef =
    val len = content.length
    var i = 0
    var tokenSize = 0

    while i < len do
      val ch = content.charAt(i)
      i += 1
      if ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') then
        tokenSize += 1
        if tokenSize == 9 then
          return null // maximum 8 characters are allowed
      else if ch == '-' then
        if tokenSize == 0 then return null // at least one alphabet precedes '-'
        tokenSize = 0
      else
        return null // invalid characters

    if tokenSize == 0 then return null // string is empty or ends with '-'

    content.toLowerCase

object LanguageType:
  val theInstance: LanguageType = new LanguageType()
