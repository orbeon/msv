/*
 * @(#)$Id: UnicodeUtil.java,v 1.8 2003/06/09 20:49:24 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * Unicode-related utility functions.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
object UnicodeUtil:
  /**
   * Count the number of "character" in Unicode string.
   * 
   * "character" here is defined by http://www.w3.org/TR/REC-xml#NT-Char
   * Basically, all the work this function will do is
   * to take care of surrogate pairs.
   * 
   * If string contains any char ('char' in Java datatype) other than those
   * allowed in XML spec, the behavior is undefined. However, we can safely
   * assume that XML parser performs this check before we receive the value.
   */
  def countLength(str: String): Int =
    val len = str.length
    var count = 0
    var i = 0
    while i < len do
      val ch = str.charAt(i)
      // skip the first half of surrogate pair
      // we can safely assume that the last half of surrogate pair follows.
      // because that's a requirement for XML parser
      if !(0xd800 <= ch && ch < 0xdc00) then
        count += 1
      i += 1
    count
