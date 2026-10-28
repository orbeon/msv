/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Xerces" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation and was
 * originally based on software copyright (c) 1999, International
 * Business Machines, Inc., http://www.apache.org.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

package org.orbeon.msv.datatype.regexp



/**
 * @version REUtil.java,v 1.7 2002/11/20 00:49:47 twl Exp
 */
final class REUtil private ()

object REUtil {
  def composeFromSurrogates(high: Int, low: Int): Int = {
    0x10000 + ((high - 0xd800) << 10) + low - 0xdc00
  }

  def isLowSurrogate(ch: Int): Boolean = {
    (ch & 0xfc00) == 0xdc00
  }

  def isHighSurrogate(ch: Int): Boolean = {
    (ch & 0xfc00) == 0xd800
  }

  def decomposeToSurrogates(ch0: Int): String = {
    val chs = new Array[Char](2)
    val ch = ch0 - 0x10000
    chs(0) = ((ch >> 10) + 0xd800).toChar
    chs(1) = ((ch & 0x3ff) + 0xdc00).toChar
    new String(chs)
  }


  // ================================================================

  def getOptionValue(ch: Int): Int = {
    var ret = 0
    ch match {
      case 'i' => ret = RegularExpression.IGNORE_CASE
      case 'm' => ret = RegularExpression.MULTIPLE_LINES
      case 's' => ret = RegularExpression.SINGLE_LINE
      case 'x' => ret = RegularExpression.EXTENDED_COMMENT
      case 'u' => ret = RegularExpression.USE_UNICODE_CATEGORY
      case 'w' => ret = RegularExpression.UNICODE_WORD_BOUNDARY
      case 'F' => ret = RegularExpression.PROHIBIT_FIXED_STRING_OPTIMIZATION
      case 'H' => ret = RegularExpression.PROHIBIT_HEAD_CHARACTER_OPTIMIZATION
      case 'X' => ret = RegularExpression.XMLSCHEMA_MODE
      case ',' => ret = RegularExpression.SPECIAL_COMMA
      case _ =>
    }
    ret
  }

  @throws[ParseException]
  def parseOptions(opts: String): Int = {
    if (opts == null) return 0
    var options = 0
    var i = 0
    while (i < opts.length) {
      val v = getOptionValue(opts.charAt(i).toInt)
      if (v == 0)
        throw new ParseException("Unknown Option: " + opts.substring(i), -1)
      options |= v
      i += 1
    }
    options
  }

  def createOptionString(options: Int): String = {
    val sb = new java.lang.StringBuilder(9)
    if ((options & RegularExpression.PROHIBIT_FIXED_STRING_OPTIMIZATION) != 0)
      sb.append('F')
    if ((options & RegularExpression.PROHIBIT_HEAD_CHARACTER_OPTIMIZATION) != 0)
      sb.append('H')
    if ((options & RegularExpression.XMLSCHEMA_MODE) != 0)
      sb.append('X')
    if ((options & RegularExpression.IGNORE_CASE) != 0)
      sb.append('i')
    if ((options & RegularExpression.MULTIPLE_LINES) != 0)
      sb.append('m')
    if ((options & RegularExpression.SINGLE_LINE) != 0)
      sb.append('s')
    if ((options & RegularExpression.USE_UNICODE_CATEGORY) != 0)
      sb.append('u')
    if ((options & RegularExpression.UNICODE_WORD_BOUNDARY) != 0)
      sb.append('w')
    if ((options & RegularExpression.EXTENDED_COMMENT) != 0)
      sb.append('x')
    if ((options & RegularExpression.SPECIAL_COMMA) != 0)
      sb.append(',')
    sb.toString.intern()
  }

  // ================================================================

  def stripExtendedComment(regex: String): String = {
    val len = regex.length
    val buffer = new java.lang.StringBuilder(len)
    var offset = 0
    while (offset < len) {
      var ch = regex.charAt(offset).toInt
      offset += 1
      // Skips a white space.
      if (ch == '\t' || ch == '\n' || ch == '\f' || ch == '\r' || ch == ' ') {
        // continue
      } else if (ch == '#') { // Skips characters between '#' and a line end.
        var breakInner = false
        while (offset < len && !breakInner) {
          ch = regex.charAt(offset).toInt
          offset += 1
          if (ch == '\r' || ch == '\n') {
            breakInner = true
          }
        }
      } else if (ch == '\\' && offset < len) {
        val next = regex.charAt(offset).toInt
        if (next == '#' || next == '\t' || next == '\n' || next == '\f' || next == '\r' || next == ' ') {
          buffer.append(next.toChar)
          offset += 1
        } else { // Other escaped character.
          buffer.append('\\')
          buffer.append(next.toChar)
          offset += 1
        }
      } else {
        buffer.append(ch.toChar)
      }
    }
    buffer.toString
  }

  // ================================================================

  final val CACHESIZE: Int = 20
  val regexCache: Array[RegularExpression] = new Array[RegularExpression](CACHESIZE)

  /**
   * Creates a RegularExpression instance.
   * This method caches created instances.
   */
  @throws[ParseException]
  def createRegex(pattern: String, options: String): RegularExpression = {
    var re: RegularExpression = null
    val intOptions = REUtil.parseOptions(options)
    regexCache.synchronized {
      var i = 0
      var breakLoop = false
      while (i < CACHESIZE && !breakLoop) {
        val cached = regexCache(i)
        if (cached == null) {
          i = -1
          breakLoop = true
        } else if (cached.equals(pattern, intOptions)) {
          re = cached
          breakLoop = true
        } else {
          i += 1
        }
      }
      if (re != null) {
        if (i != 0) {
          System.arraycopy(regexCache, 0, regexCache, 1, i)
          regexCache(0) = re
        }
      } else {
        re = new RegularExpression(pattern, options)
        System.arraycopy(regexCache, 0, regexCache, 1, CACHESIZE - 1)
        regexCache(0) = re
      }
    }
    re
  }

  /**
   * @see RegularExpression#matches(java.lang.String)
   */
  @throws[ParseException]
  def matches(regex: String, target: String): Boolean = {
    createRegex(regex, null).matches(target)
  }

  /**
   * @see RegularExpression#matches(java.lang.String)
   */
  @throws[ParseException]
  def matches(regex: String, options: String, target: String): Boolean = {
    createRegex(regex, options).matches(target)
  }

  // ================================================================

  def quoteMeta(literal: String): String = {
    val len = literal.length
    var buffer: java.lang.StringBuilder = null
    var i = 0
    while (i < len) {
      val ch = literal.charAt(i)
      if (".*+?{[()|\\^$".indexOf(ch) >= 0) {
        if (buffer == null) {
          buffer = new java.lang.StringBuilder(i + (len - i) * 2)
          if (i > 0) buffer.append(literal.substring(0, i))
        }
        buffer.append('\\')
        buffer.append(ch)
      } else if (buffer != null) {
        buffer.append(ch)
      }
      i += 1
    }
    if (buffer != null) buffer.toString else literal
  }

  // ================================================================

  def dumpString(v: String): Unit = {
    var i = 0
    while (i < v.length) {
      System.out.print(java.lang.Integer.toHexString(v.charAt(i).toInt))
      System.out.print(" ")
      i += 1
    }
    System.out.println()
  }
}
