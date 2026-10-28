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
 * This class represents a character class such as [a-z] or a period.
 *
 * @version RangeToken.java,v 1.4 2002/08/09 15:18:17 neilg Exp
 */
final class RangeToken(tokenType: Int) extends Token(tokenType) with java.io.Serializable {

  var ranges: Array[Int] = null
  var sorted: Boolean = false
  var compacted: Boolean = false
  var icaseCache: RangeToken = null
  var map: Array[Int] = null
  var nonMapIndex: Int = 0

  this.setSorted(false)

  // for RANGE or NRANGE
  override protected[regexp] def addRange(start: Int, end: Int): Unit = {
    this.icaseCache = null
    val (r1, r2) = if (start <= end) (start, end) else (end, start)

    if (this.ranges == null) {
      this.ranges = new Array[Int](2)
      this.ranges(0) = r1
      this.ranges(1) = r2
      this.setSorted(true)
    } else {
      val pos = this.ranges.length
      if (this.ranges(pos - 1) + 1 == r1) {
        this.ranges(pos - 1) = r2
        return
      }
      val temp = new Array[Int](pos + 2)
      System.arraycopy(this.ranges, 0, temp, 0, pos)
      this.ranges = temp
      if (this.ranges(pos - 1) >= r1)
        this.setSorted(false)
      this.ranges(pos) = r1
      this.ranges(pos + 1) = r2
      if (!this.sorted)
        this.sortRanges()
    }
  }

  private def isSorted: Boolean = this.sorted

  private def setSorted(sort: Boolean): Unit = {
    this.sorted = sort
    if (!sort) this.compacted = false
  }

  private def isCompacted: Boolean = this.compacted

  private def setCompacted(): Unit = {
    this.compacted = true
  }

  override protected[regexp] def sortRanges(): Unit = {
    if (this.isSorted) return
    if (this.ranges == null) return

    // Bubble sort
    var i = this.ranges.length - 4
    while (i >= 0) {
      var j = 0
      while (j <= i) {
        if (this.ranges(j) > this.ranges(j + 2)
          || (this.ranges(j) == this.ranges(j + 2) && this.ranges(j + 1) > this.ranges(j + 3))) {
          var tmp = this.ranges(j + 2)
          this.ranges(j + 2) = this.ranges(j)
          this.ranges(j) = tmp
          tmp = this.ranges(j + 3)
          this.ranges(j + 3) = this.ranges(j + 1)
          this.ranges(j + 1) = tmp
        }
        j += 2
      }
      i -= 2
    }
    this.setSorted(true)
  }

  /**
   * this.ranges is sorted.
   */
  override protected[regexp] def compactRanges(): Unit = {
    val DEBUG = false
    if (this.ranges == null || this.ranges.length <= 2) return
    if (this.isCompacted) return
    var base = 0
    var target = 0

    while (target < this.ranges.length) {
      if (base != target) {
        this.ranges(base) = this.ranges(target)
        target += 1
        this.ranges(base + 1) = this.ranges(target)
        target += 1
      } else {
        target += 2
      }
      var baseend = this.ranges(base + 1)
      var breakInner = false
      while (target < this.ranges.length && !breakInner) {
        if (baseend + 1 < this.ranges(target)) {
          breakInner = true
        } else if (baseend + 1 == this.ranges(target)) {
          this.ranges(base + 1) = this.ranges(target + 1)
          baseend = this.ranges(base + 1)
          target += 2
        } else if (baseend >= this.ranges(target + 1)) {
          target += 2
        } else if (baseend < this.ranges(target + 1)) {
          this.ranges(base + 1) = this.ranges(target + 1)
          baseend = this.ranges(base + 1)
          target += 2
        } else {
          throw new RuntimeException(s"Token#compactRanges(): Internal Error: [${this.ranges(base)},${this.ranges(base + 1)}] [${this.ranges(target)},${this.ranges(target + 1)}]")
        }
      }
      base += 2
    }

    if (base != this.ranges.length) {
      val result = new Array[Int](base)
      System.arraycopy(this.ranges, 0, result, 0, base)
      this.ranges = result
    }
    this.setCompacted()
  }

  override protected[regexp] def mergeRanges(token: Token): Unit = {
    val tok = token.asInstanceOf[RangeToken]
    this.sortRanges()
    tok.sortRanges()
    if (tok.ranges == null) return
    this.icaseCache = null
    this.setSorted(true)
    if (this.ranges == null) {
      this.ranges = new Array[Int](tok.ranges.length)
      System.arraycopy(tok.ranges, 0, this.ranges, 0, tok.ranges.length)
      return
    }
    val result = new Array[Int](this.ranges.length + tok.ranges.length)
    var i = 0
    var j = 0
    var k = 0
    while (i < this.ranges.length || j < tok.ranges.length) {
      if (i >= this.ranges.length) {
        result(k) = tok.ranges(j); k += 1; j += 1
        result(k) = tok.ranges(j); k += 1; j += 1
      } else if (j >= tok.ranges.length) {
        result(k) = this.ranges(i); k += 1; i += 1
        result(k) = this.ranges(i); k += 1; i += 1
      } else if (tok.ranges(j) < this.ranges(i)
        || (tok.ranges(j) == this.ranges(i) && tok.ranges(j + 1) < this.ranges(i + 1))) {
        result(k) = tok.ranges(j); k += 1; j += 1
        result(k) = tok.ranges(j); k += 1; j += 1
      } else {
        result(k) = this.ranges(i); k += 1; i += 1
        result(k) = this.ranges(i); k += 1; i += 1
      }
    }
    this.ranges = result
  }

  override protected[regexp] def subtractRanges(token: Token): Unit = {
    if (token.tokenType == Token.NRANGE) {
      this.intersectRanges(token)
      return
    }
    val tok = token.asInstanceOf[RangeToken]
    if (tok.ranges == null || this.ranges == null) return
    this.icaseCache = null
    this.sortRanges()
    this.compactRanges()
    tok.sortRanges()
    tok.compactRanges()

    val result = new Array[Int](this.ranges.length + tok.ranges.length)
    var wp = 0
    var src = 0
    var sub = 0
    while (src < this.ranges.length && sub < tok.ranges.length) {
      val srcbegin = this.ranges(src)
      val srcend = this.ranges(src + 1)
      val subbegin = tok.ranges(sub)
      val subend = tok.ranges(sub + 1)
      if (srcend < subbegin) {
        result(wp) = this.ranges(src); wp += 1; src += 1
        result(wp) = this.ranges(src); wp += 1; src += 1
      } else if (srcend >= subbegin && srcbegin <= subend) {
        if (subbegin <= srcbegin && srcend <= subend) {
          src += 2
        } else if (subbegin <= srcbegin) {
          this.ranges(src) = subend + 1
          sub += 2
        } else if (srcend <= subend) {
          result(wp) = srcbegin; wp += 1
          result(wp) = subbegin - 1; wp += 1
          src += 2
        } else {
          result(wp) = srcbegin; wp += 1
          result(wp) = subbegin - 1; wp += 1
          this.ranges(src) = subend + 1
          sub += 2
        }
      } else if (subend < srcbegin) {
        sub += 2
      } else {
        throw new RuntimeException(s"Token#subtractRanges(): Internal Error: [${this.ranges(src)},${this.ranges(src + 1)}] - [${tok.ranges(sub)},${tok.ranges(sub + 1)}]")
      }
    }
    while (src < this.ranges.length) {
      result(wp) = this.ranges(src); wp += 1; src += 1
      result(wp) = this.ranges(src); wp += 1; src += 1
    }
    this.ranges = new Array[Int](wp)
    System.arraycopy(result, 0, this.ranges, 0, wp)
  }

  override protected[regexp] def intersectRanges(token: Token): Unit = {
    val tok = token.asInstanceOf[RangeToken]
    if (tok.ranges == null || this.ranges == null) return
    this.icaseCache = null
    this.sortRanges()
    this.compactRanges()
    tok.sortRanges()
    tok.compactRanges()

    val result = new Array[Int](this.ranges.length + tok.ranges.length)
    var wp = 0
    var src1 = 0
    var src2 = 0
    while (src1 < this.ranges.length && src2 < tok.ranges.length) {
      val src1begin = this.ranges(src1)
      val src1end = this.ranges(src1 + 1)
      val src2begin = tok.ranges(src2)
      val src2end = tok.ranges(src2 + 1)
      if (src1end < src2begin) {
        src1 += 2
      } else if (src1end >= src2begin && src1begin <= src2end) {
        if (src2begin <= src2begin && src1end <= src2end) {
          result(wp) = src1begin; wp += 1
          result(wp) = src1end; wp += 1
          src1 += 2
        } else if (src2begin <= src1begin) {
          result(wp) = src1begin; wp += 1
          result(wp) = src2end; wp += 1
          this.ranges(src1) = src2end + 1
          src2 += 2
        } else if (src1end <= src2end) {
          result(wp) = src2begin; wp += 1
          result(wp) = src1end; wp += 1
          src1 += 2
        } else {
          result(wp) = src2begin; wp += 1
          result(wp) = src2end; wp += 1
          this.ranges(src1) = src2end + 1
        }
      } else if (src2end < src1begin) {
        src2 += 2
      } else {
        throw new RuntimeException(s"Token#intersectRanges(): Internal Error: [${this.ranges(src1)},${this.ranges(src1 + 1)}] & [${tok.ranges(src2)},${tok.ranges(src2 + 1)}]")
      }
    }
    while (src1 < this.ranges.length) {
      result(wp) = this.ranges(src1); wp += 1; src1 += 1
      result(wp) = this.ranges(src1); wp += 1; src1 += 1
    }
    this.ranges = new Array[Int](wp)
    System.arraycopy(result, 0, this.ranges, 0, wp)
  }

  def getCaseInsensitiveToken: RangeToken = synchronized {
    if (this.icaseCache != null) return this.icaseCache

    val uppers = if (this.tokenType == Token.RANGE) Token.createRange() else Token.createNRange()
    var i = 0
    while (i < this.ranges.length) {
      var ch = this.ranges(i)
      val limit = this.ranges(i + 1)
      while (ch <= limit) {
        if (ch > 0xffff) {
          uppers.addRange(ch, ch)
        } else {
          val uch = Character.toUpperCase(ch.toChar).toInt
          uppers.addRange(uch, uch)
        }
        ch += 1
      }
      i += 2
    }
    val lowers = if (this.tokenType == Token.RANGE) Token.createRange() else Token.createNRange()
    i = 0
    while (i < uppers.ranges.length) {
      var ch = uppers.ranges(i)
      val limit = uppers.ranges(i + 1)
      while (ch <= limit) {
        if (ch > 0xffff) {
          lowers.addRange(ch, ch)
        } else {
          val uch = Character.toUpperCase(ch.toChar).toInt
          lowers.addRange(uch, uch)
        }
        ch += 1
      }
      i += 2
    }
    lowers.mergeRanges(uppers)
    lowers.mergeRanges(this)
    lowers.compactRanges()

    this.icaseCache = lowers
    lowers
  }

  def dumpRanges(): Unit = {
    System.err.print("RANGE: ")
    if (this.ranges == null)
      System.err.println(" NULL")
    var i = 0
    while (i < this.ranges.length) {
      System.err.print(s"[${this.ranges(i)},${this.ranges(i + 1)}] ")
      i += 2
    }
    System.err.println("")
  }

  override def `match`(ch: Int): Boolean = {
    if (this.map == null) this.createMap()
    if (this.tokenType == Token.RANGE) {
      if (ch < RangeToken.MAPSIZE)
        return (this.map(ch / 32) & (1 << (ch & 0x1f))) != 0
      var i = this.nonMapIndex
      while (i < this.ranges.length) {
        if (this.ranges(i) <= ch && ch <= this.ranges(i + 1))
          return true
        i += 2
      }
      false
    } else {
      if (ch < RangeToken.MAPSIZE)
        return (this.map(ch / 32) & (1 << (ch & 0x1f))) == 0
      var i = this.nonMapIndex
      while (i < this.ranges.length) {
        if (this.ranges(i) <= ch && ch <= this.ranges(i + 1))
          return false
        i += 2
      }
      true
    }
  }

  private def createMap(): Unit = {
    val asize = RangeToken.MAPSIZE / 32
    this.map = new Array[Int](asize)
    this.nonMapIndex = this.ranges.length
    var i = 0
    while (i < asize) {
      this.map(i) = 0
      i += 1
    }
    i = 0
    var breakLoop = false
    while (i < this.ranges.length && !breakLoop) {
      val s = this.ranges(i)
      val e = this.ranges(i + 1)
      if (s < RangeToken.MAPSIZE) {
        var j = s
        while (j <= e && j < RangeToken.MAPSIZE) {
          this.map(j / 32) |= (1 << (j & 0x1f))
          j += 1
        }
      } else {
        this.nonMapIndex = i
        breakLoop = true
      }
      if (!breakLoop && e >= RangeToken.MAPSIZE) {
        this.nonMapIndex = i
        breakLoop = true
      }
      i += 2
    }
  }

  override def toString(options: Int): String = {
    if (this.tokenType == Token.RANGE) {
      if (this eq Token.token_dot) "."
      else if (this eq Token.token_0to9) "\\d"
      else if (this eq Token.token_wordchars) "\\w"
      else if (this eq Token.token_spaces) "\\s"
      else {
        val sb = new java.lang.StringBuilder()
        sb.append("[")
        var i = 0
        while (i < this.ranges.length) {
          if ((options & RegularExpression.SPECIAL_COMMA) != 0 && i > 0) sb.append(",")
          if (this.ranges(i) == this.ranges(i + 1)) {
            sb.append(RangeToken.escapeCharInCharClass(this.ranges(i)))
          } else {
            sb.append(RangeToken.escapeCharInCharClass(this.ranges(i)))
            sb.append('-')
            sb.append(RangeToken.escapeCharInCharClass(this.ranges(i + 1)))
          }
          i += 2
        }
        sb.append("]")
        sb.toString
      }
    } else {
      if (this eq Token.token_not_0to9) "\\D"
      else if (this eq Token.token_not_wordchars) "\\W"
      else if (this eq Token.token_not_spaces) "\\S"
      else {
        val sb = new java.lang.StringBuilder()
        sb.append("[^")
        var i = 0
        while (i < this.ranges.length) {
          if ((options & RegularExpression.SPECIAL_COMMA) != 0 && i > 0) sb.append(",")
          if (this.ranges(i) == this.ranges(i + 1)) {
            sb.append(RangeToken.escapeCharInCharClass(this.ranges(i)))
          } else {
            sb.append(RangeToken.escapeCharInCharClass(this.ranges(i)))
            sb.append('-')
            sb.append(RangeToken.escapeCharInCharClass(this.ranges(i + 1)))
          }
          i += 2
        }
        sb.append("]")
        sb.toString
      }
    }
  }
}

object RangeToken {
  private final val MAPSIZE: Int = 256

  /**
   * for RANGE: Creates complement.
   * for NRANGE: Creates the same meaning RANGE.
   */
  def complementRanges(token: Token): Token = {
    if (token.tokenType != Token.RANGE && token.tokenType != Token.NRANGE)
      throw new IllegalArgumentException("Token#complementRanges(): must be RANGE: " + token.tokenType)
    val tok = token.asInstanceOf[RangeToken]
    tok.sortRanges()
    tok.compactRanges()
    var len = tok.ranges.length + 2
    if (tok.ranges(0) == 0)
      len -= 2
    val last = tok.ranges(tok.ranges.length - 1)
    if (last == Token.UTF16_MAX)
      len -= 2
    val ret = Token.createRange()
    ret.ranges = new Array[Int](len)
    var wp = 0
    if (tok.ranges(0) > 0) {
      ret.ranges(wp) = 0; wp += 1
      ret.ranges(wp) = tok.ranges(0) - 1; wp += 1
    }
    var i = 1
    while (i < tok.ranges.length - 2) {
      ret.ranges(wp) = tok.ranges(i) + 1; wp += 1
      ret.ranges(wp) = tok.ranges(i + 1) - 1; wp += 1
      i += 2
    }
    if (last != Token.UTF16_MAX) {
      ret.ranges(wp) = last + 1; wp += 1
      ret.ranges(wp) = Token.UTF16_MAX
    }
    ret.setCompacted()
    ret
  }

  private def escapeCharInCharClass(ch: Int): String = {
    ch match {
      case '[' | ']' | '-' | '^' | ',' | '\\' => "\\" + ch.toChar
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case 0x1b => "\\e"
      case _ =>
        if (ch < 0x20) {
          val pre = "0" + java.lang.Integer.toHexString(ch)
          "\\x" + pre.substring(pre.length - 2, pre.length)
        } else if (ch >= 0x10000) {
          val pre = "0" + java.lang.Integer.toHexString(ch)
          "\\v" + pre.substring(pre.length - 6, pre.length)
        } else {
          ch.toChar.toString
        }
    }
  }
}
