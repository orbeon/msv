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

import java.util.{ArrayList, HashMap}

/**
 * This class represents a node in parse tree.
 *
 * @version Token.java,v 1.7 2003/02/25 14:43:13 sandygao Exp
 */
class Token(val tokenType: Int) extends java.io.Serializable {

  /**
   * A number of children.
   */
  def size(): Int = 0

  def getChild(index: Int): Token = null

  def addChild(tok: Token): Unit = {
    throw new RuntimeException("Not supported.")
  }

  // for RANGE or NRANGE
  protected[regexp] def addRange(start: Int, end: Int): Unit = {
    throw new RuntimeException("Not supported.")
  }

  protected[regexp] def sortRanges(): Unit = {
    throw new RuntimeException("Not supported.")
  }

  protected[regexp] def compactRanges(): Unit = {
    throw new RuntimeException("Not supported.")
  }

  protected[regexp] def mergeRanges(tok: Token): Unit = {
    throw new RuntimeException("Not supported.")
  }

  protected[regexp] def subtractRanges(tok: Token): Unit = {
    throw new RuntimeException("Not supported.")
  }

  protected[regexp] def intersectRanges(tok: Token): Unit = {
    throw new RuntimeException("Not supported.")
  }

  def setMin(min: Int): Unit = () // for CLOSURE
  def setMax(max: Int): Unit = () // for CLOSURE
  def getMin: Int = -1 // for CLOSURE
  def getMax: Int = -1 // for CLOSURE
  def getReferenceNumber: Int = 0 // for STRING
  def getString: String = null // for STRING
  def getParenNumber: Int = 0
  def getChar: Int = -1

  override def toString: String = this.toString(0)

  def toString(options: Int): String = {
    if (this.tokenType == Token.DOT) "." else ""
  }

  /**
   * How many characters are needed?
   */
  final def getMinLength: Int = {
    this.tokenType match {
      case Token.CONCAT =>
        var sum = 0
        var i = 0
        while (i < this.size()) {
          sum += this.getChild(i).getMinLength
          i += 1
        }
        sum

      case Token.CONDITION | Token.UNION =>
        if (this.size() == 0) return 0
        var ret = this.getChild(0).getMinLength
        var i = 1
        while (i < this.size()) {
          val min = this.getChild(i).getMinLength
          if (min < ret) ret = min
          i += 1
        }
        ret

      case Token.CLOSURE | Token.NONGREEDYCLOSURE =>
        if (this.getMin >= 0)
          this.getMin * this.getChild(0).getMinLength
        else
          0

      case Token.EMPTY | Token.ANCHOR =>
        0

      case Token.DOT | Token.CHAR | Token.RANGE | Token.NRANGE =>
        1

      case Token.INDEPENDENT | Token.PAREN | Token.MODIFIERGROUP =>
        this.getChild(0).getMinLength

      case Token.BACKREFERENCE =>
        0

      case Token.STRING =>
        this.getString.length

      case Token.LOOKAHEAD | Token.NEGATIVELOOKAHEAD | Token.LOOKBEHIND | Token.NEGATIVELOOKBEHIND =>
        0

      case _ =>
        throw new RuntimeException("Token#getMinLength(): Invalid Type: " + this.tokenType)
    }
  }

  final def getMaxLength: Int = {
    this.tokenType match {
      case Token.CONCAT =>
        var sum = 0
        var i = 0
        var breakLoop = false
        while (i < this.size() && !breakLoop) {
          val d = this.getChild(i).getMaxLength
          if (d < 0) {
            sum = -1
            breakLoop = true
          } else {
            sum += d
            i += 1
          }
        }
        sum

      case Token.CONDITION | Token.UNION =>
        if (this.size() == 0) return 0
        var ret = this.getChild(0).getMaxLength
        var i = 1
        while (ret >= 0 && i < this.size()) {
          val max = this.getChild(i).getMaxLength
          if (max < 0) {
            ret = -1
          } else {
            if (max > ret) ret = max
            i += 1
          }
        }
        ret

      case Token.CLOSURE | Token.NONGREEDYCLOSURE =>
        if (this.getMax >= 0)
          this.getMax * this.getChild(0).getMaxLength
        else
          -1

      case Token.EMPTY | Token.ANCHOR =>
        0

      case Token.CHAR =>
        1

      case Token.DOT | Token.RANGE | Token.NRANGE =>
        2

      case Token.INDEPENDENT | Token.PAREN | Token.MODIFIERGROUP =>
        this.getChild(0).getMaxLength

      case Token.BACKREFERENCE =>
        -1

      case Token.STRING =>
        this.getString.length

      case Token.LOOKAHEAD | Token.NEGATIVELOOKAHEAD | Token.LOOKBEHIND | Token.NEGATIVELOOKBEHIND =>
        0

      case _ =>
        throw new RuntimeException("Token#getMaxLength(): Invalid Type: " + this.tokenType)
    }
  }

  final def analyzeFirstCharacter(result: RangeToken, options: Int): Int = {
    this.tokenType match {
      case Token.CONCAT =>
        var ret = Token.FC_CONTINUE
        var i = 0
        var breakLoop = false
        while (i < this.size() && !breakLoop) {
          ret = this.getChild(i).analyzeFirstCharacter(result, options)
          if (ret != Token.FC_CONTINUE) {
            breakLoop = true
          } else {
            i += 1
          }
        }
        ret

      case Token.UNION =>
        if (this.size() == 0) return Token.FC_CONTINUE
        var ret2 = Token.FC_CONTINUE
        var hasEmpty = false
        var i = 0
        var breakLoop = false
        while (i < this.size() && !breakLoop) {
          ret2 = this.getChild(i).analyzeFirstCharacter(result, options)
          if (ret2 == Token.FC_ANY) {
            breakLoop = true
          } else {
            if (ret2 == Token.FC_CONTINUE) hasEmpty = true
            i += 1
          }
        }
        if (hasEmpty) Token.FC_CONTINUE else ret2

      case Token.CONDITION =>
        val ret3 = this.getChild(0).analyzeFirstCharacter(result, options)
        if (this.size() == 1) return Token.FC_CONTINUE
        if (ret3 == Token.FC_ANY) return ret3
        val ret4 = this.getChild(1).analyzeFirstCharacter(result, options)
        if (ret4 == Token.FC_ANY) return ret4
        if (ret3 == Token.FC_CONTINUE || ret4 == Token.FC_CONTINUE) Token.FC_CONTINUE else Token.FC_TERMINAL

      case Token.CLOSURE | Token.NONGREEDYCLOSURE =>
        this.getChild(0).analyzeFirstCharacter(result, options)
        Token.FC_CONTINUE

      case Token.EMPTY | Token.ANCHOR =>
        Token.FC_CONTINUE

      case Token.CHAR =>
        var ch = this.getChar
        result.addRange(ch, ch)
        if (ch < 0x10000 && Token.isSet(options, RegularExpression.IGNORE_CASE)) {
          ch = Character.toUpperCase(ch.toChar).toInt
          result.addRange(ch, ch)
          ch = Character.toLowerCase(ch.toChar).toInt
          result.addRange(ch, ch)
        }
        Token.FC_TERMINAL

      case Token.DOT =>
        Token.FC_CONTINUE

      case Token.RANGE =>
        if (Token.isSet(options, RegularExpression.IGNORE_CASE)) {
          result.mergeRanges(this.asInstanceOf[RangeToken].getCaseInsensitiveToken)
        } else {
          result.mergeRanges(this)
        }
        Token.FC_TERMINAL

      case Token.NRANGE =>
        if (Token.isSet(options, RegularExpression.IGNORE_CASE)) {
          result.mergeRanges(Token.complementRanges(this.asInstanceOf[RangeToken].getCaseInsensitiveToken))
        } else {
          result.mergeRanges(Token.complementRanges(this))
        }
        Token.FC_TERMINAL

      case Token.INDEPENDENT | Token.PAREN =>
        this.getChild(0).analyzeFirstCharacter(result, options)

      case Token.MODIFIERGROUP =>
        var opts = options | this.asInstanceOf[Token.ModifierToken].getOptions
        opts &= ~this.asInstanceOf[Token.ModifierToken].getOptionsMask
        this.getChild(0).analyzeFirstCharacter(result, opts)

      case Token.BACKREFERENCE =>
        result.addRange(0, Token.UTF16_MAX)
        Token.FC_ANY

      case Token.STRING =>
        var cha = this.getString.charAt(0).toInt
        if (REUtil.isHighSurrogate(cha) && this.getString.length >= 2) {
          val ch2 = this.getString.charAt(1).toInt
          if (REUtil.isLowSurrogate(ch2))
            cha = REUtil.composeFromSurrogates(cha, ch2)
        }
        result.addRange(cha, cha)
        if (cha < 0x10000 && Token.isSet(options, RegularExpression.IGNORE_CASE)) {
          cha = Character.toUpperCase(cha.toChar).toInt
          result.addRange(cha, cha)
          cha = Character.toLowerCase(cha.toChar).toInt
          result.addRange(cha, cha)
        }
        Token.FC_TERMINAL

      case Token.LOOKAHEAD | Token.NEGATIVELOOKAHEAD | Token.LOOKBEHIND | Token.NEGATIVELOOKBEHIND =>
        Token.FC_CONTINUE

      case _ =>
        throw new RuntimeException("Token#analyzeHeadCharacter(): Invalid Type: " + this.tokenType)
    }
  }

  private def isShorterThan(tok: Token): Boolean = {
    if (tok == null) return false
    val mylength = if (this.tokenType == Token.STRING) this.getString.length
    else throw new RuntimeException("Internal Error: Illegal type: " + this.tokenType)
    val otherlength = if (tok.tokenType == Token.STRING) tok.getString.length
    else throw new RuntimeException("Internal Error: Illegal type: " + tok.tokenType)
    mylength < otherlength
  }

  final def findFixedString(container: Token.FixedStringContainer, options: Int): Unit = {
    this.tokenType match {
      case Token.CONCAT =>
        var prevToken: Token = null
        var prevOptions = 0
        var i = 0
        while (i < this.size()) {
          this.getChild(i).findFixedString(container, options)
          if (prevToken == null || prevToken.isShorterThan(container.token)) {
            prevToken = container.token
            prevOptions = container.options
          }
          i += 1
        }
        container.token = prevToken
        container.options = prevOptions

      case Token.UNION | Token.CLOSURE | Token.NONGREEDYCLOSURE | Token.EMPTY | Token.ANCHOR |
           Token.RANGE | Token.DOT | Token.NRANGE | Token.BACKREFERENCE | Token.LOOKAHEAD |
           Token.NEGATIVELOOKAHEAD | Token.LOOKBEHIND | Token.NEGATIVELOOKBEHIND | Token.CONDITION =>
        container.token = null

      case Token.CHAR =>
        container.token = null

      case Token.STRING =>
        container.token = this
        container.options = options

      case Token.INDEPENDENT | Token.PAREN =>
        this.getChild(0).findFixedString(container, options)

      case Token.MODIFIERGROUP =>
        var opts = options | this.asInstanceOf[Token.ModifierToken].getOptions
        opts &= ~this.asInstanceOf[Token.ModifierToken].getOptionsMask
        this.getChild(0).findFixedString(container, opts)

      case _ =>
        throw new RuntimeException("Token#findFixedString(): Invalid Type: " + this.tokenType)
    }
  }

  def `match`(ch: Int): Boolean = {
    throw new RuntimeException("NFAArrow#match(): Internal error: " + this.tokenType)
  }
}

object Token {
  final val COUNTTOKENS: Boolean = true
  var tokens: Int = 0

  final val CHAR: Int = 0 // Literal char
  final val DOT: Int = 11 // .
  final val CONCAT: Int = 1 // XY
  final val UNION: Int = 2 // X|Y|Z
  final val CLOSURE: Int = 3 // X*
  final val RANGE: Int = 4 // [a-zA-Z] etc.
  final val NRANGE: Int = 5 // [^a-zA-Z] etc.
  final val PAREN: Int = 6 // (X) or (?:X)
  final val EMPTY: Int = 7
  final val ANCHOR: Int = 8 // ^ $ \b \B \< \> \A \Z \z
  final val NONGREEDYCLOSURE: Int = 9 // *? +?
  final val STRING: Int = 10 // strings
  final val BACKREFERENCE: Int = 12 // back references
  final val LOOKAHEAD: Int = 20 // (?=...)
  final val NEGATIVELOOKAHEAD: Int = 21 // (?!...)
  final val LOOKBEHIND: Int = 22 // (?<=...)
  final val NEGATIVELOOKBEHIND: Int = 23 // (?<!...)
  final val INDEPENDENT: Int = 24 // (?>...)
  final val MODIFIERGROUP: Int = 25 // (?ims-ims:...)
  final val CONDITION: Int = 26 // (?(...)yes|no)

  final val UTF16_MAX: Int = 0x10ffff

  val token_empty: Token = new Token(Token.EMPTY)
  val token_linebeginning: CharToken = Token.createAnchor('^')
  val token_linebeginning2: CharToken = Token.createAnchor('@')
  val token_lineend: CharToken = Token.createAnchor('$')
  val token_stringbeginning: CharToken = Token.createAnchor('A')
  val token_stringend: CharToken = Token.createAnchor('z')
  val token_stringend2: CharToken = Token.createAnchor('Z')
  val token_wordedge: CharToken = Token.createAnchor('b')
  val token_not_wordedge: CharToken = Token.createAnchor('B')
  val token_wordbeginning: CharToken = Token.createAnchor('<')
  val token_wordend: CharToken = Token.createAnchor('>')

  val token_dot: Token = new Token(Token.DOT)

  val token_0to9: RangeToken = {
    val t = Token.createRange()
    t.addRange('0', '9')
    t
  }

  val token_wordchars: RangeToken = {
    val t = Token.createRange()
    t.addRange('0', '9')
    t.addRange('A', 'Z')
    t.addRange('_', '_')
    t.addRange('a', 'z')
    t
  }

  val token_spaces: RangeToken = {
    val t = Token.createRange()
    t.addRange('\t', '\t')
    t.addRange('\n', '\n')
    t.addRange('\f', '\f')
    t.addRange('\r', '\r')
    t.addRange(' ', ' ')
    t
  }

  val token_not_0to9: Token = Token.complementRanges(Token.token_0to9)
  val token_not_wordchars: Token = Token.complementRanges(Token.token_wordchars)
  val token_not_spaces: Token = Token.complementRanges(Token.token_spaces)

  def createLook(opType: Int, child: Token): ParenToken = {
    if (COUNTTOKENS) tokens += 1
    new ParenToken(opType, child, 0)
  }

  def createParen(child: Token, pnumber: Int): ParenToken = {
    if (COUNTTOKENS) tokens += 1
    new ParenToken(Token.PAREN, child, pnumber)
  }

  def createClosure(tok: Token): ClosureToken = {
    if (COUNTTOKENS) tokens += 1
    new ClosureToken(Token.CLOSURE, tok)
  }

  def createNGClosure(tok: Token): ClosureToken = {
    if (COUNTTOKENS) tokens += 1
    new ClosureToken(Token.NONGREEDYCLOSURE, tok)
  }

  def createConcat(tok1: Token, tok2: Token): ConcatToken = {
    if (COUNTTOKENS) tokens += 1
    new ConcatToken(tok1, tok2)
  }

  def createConcat(): UnionToken = {
    if (COUNTTOKENS) tokens += 1
    new UnionToken(Token.CONCAT)
  }

  def createUnion(): UnionToken = {
    if (COUNTTOKENS) tokens += 1
    new UnionToken(Token.UNION)
  }

  def createEmpty(): Token = {
    Token.token_empty
  }

  def createRange(): RangeToken = {
    if (COUNTTOKENS) tokens += 1
    new RangeToken(Token.RANGE)
  }

  def createNRange(): RangeToken = {
    if (COUNTTOKENS) tokens += 1
    new RangeToken(Token.NRANGE)
  }

  def createChar(ch: Int): CharToken = {
    if (COUNTTOKENS) tokens += 1
    new CharToken(Token.CHAR, ch)
  }

  private def createAnchor(ch: Int): CharToken = {
    if (COUNTTOKENS) tokens += 1
    new CharToken(Token.ANCHOR, ch)
  }

  def createBackReference(refno: Int): StringToken = {
    if (COUNTTOKENS) tokens += 1
    new StringToken(Token.BACKREFERENCE, null, refno)
  }

  def createString(str: String): StringToken = {
    if (COUNTTOKENS) tokens += 1
    new StringToken(Token.STRING, str, 0)
  }

  def createModifierGroup(child: Token, add: Int, mask: Int): ModifierToken = {
    if (COUNTTOKENS) tokens += 1
    new ModifierToken(child, add, mask)
  }

  def createCondition(refno: Int, condition: Token, yespat: Token, nopat: Token): ConditionToken = {
    if (COUNTTOKENS) tokens += 1
    new ConditionToken(refno, condition, yespat, nopat)
  }

  def complementRanges(tok: Token): Token = {
    RangeToken.complementRanges(tok)
  }

  final val FC_CONTINUE: Int = 0
  final val FC_TERMINAL: Int = 1
  final val FC_ANY: Int = 2

  private def isSet(options: Int, flag: Int): Boolean = {
    (options & flag) == flag
  }

  class FixedStringContainer {
    var token: Token = null
    var options: Int = 0
  }

  // ------------------------------------------------------
  private val categories = new HashMap[String, Token]()
  private val categories2 = new HashMap[String, Token]()
  private val categoryNames: Array[String] = Array(
    "Cn", "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Me", "Mc", "Nd",
    "Nl", "No", "Zs", "Zl", "Zp", "Cc", "Cf", null, "Co", "Cs",
    "Pd", "Ps", "Pe", "Pc", "Po", "Sm", "Sc", "Sk", "So", // 28
    "Pi", "Pf", // 29, 30
    "L", "M", "N", "Z", "C", "P", "S" // 31-37
  )

  final val CHAR_INIT_QUOTE: Int = 29
  final val CHAR_FINAL_QUOTE: Int = 30
  final val CHAR_LETTER: Int = 31
  final val CHAR_MARK: Int = 32
  final val CHAR_NUMBER: Int = 33
  final val CHAR_SEPARATOR: Int = 34
  final val CHAR_OTHER: Int = 35
  final val CHAR_PUNCTUATION: Int = 36
  final val CHAR_SYMBOL: Int = 37

  private val blockNames: Array[String] = Array(
    "Basic Latin",
    "Latin-1 Supplement",
    "Latin Extended-A",
    "Latin Extended-B",
    "IPA Extensions",
    "Spacing Modifier Letters",
    "Combining Diacritical Marks",
    "Greek",
    "Cyrillic",
    "Armenian",
    "Hebrew",
    "Arabic",
    "Syriac",
    "Thaana",
    "Devanagari",
    "Bengali",
    "Gurmukhi",
    "Gujarati",
    "Oriya",
    "Tamil",
    "Telugu",
    "Kannada",
    "Malayalam",
    "Sinhala",
    "Thai",
    "Lao",
    "Tibetan",
    "Myanmar",
    "Georgian",
    "Hangul Jamo",
    "Ethiopic",
    "Cherokee",
    "Unified Canadian Aboriginal Syllabics",
    "Ogham",
    "Runic",
    "Khmer",
    "Mongolian",
    "Latin Extended Additional",
    "Greek Extended",
    "General Punctuation",
    "Superscripts and Subscripts",
    "Currency Symbols",
    "Combining Marks for Symbols",
    "Letterlike Symbols",
    "Number Forms",
    "Arrows",
    "Mathematical Operators",
    "Miscellaneous Technical",
    "Control Pictures",
    "Optical Character Recognition",
    "Enclosed Alphanumerics",
    "Box Drawing",
    "Block Elements",
    "Geometric Shapes",
    "Miscellaneous Symbols",
    "Dingbats",
    "Braille Patterns",
    "CJK Radicals Supplement",
    "Kangxi Radicals",
    "Ideographic Description Characters",
    "CJK Symbols and Punctuation",
    "Hiragana",
    "Katakana",
    "Bopomofo",
    "Hangul Compatibility Jamo",
    "Kanbun",
    "Bopomofo Extended",
    "Enclosed CJK Letters and Months",
    "CJK Compatibility",
    "CJK Unified Ideographs Extension A",
    "CJK Unified Ideographs",
    "Yi Syllables",
    "Yi Radicals",
    "Hangul Syllables",
    "Private Use",
    "CJK Compatibility Ideographs",
    "Alphabetic Presentation Forms",
    "Arabic Presentation Forms-A",
    "Combining Half Marks",
    "CJK Compatibility Forms",
    "Small Form Variants",
    "Arabic Presentation Forms-B",
    "Specials",
    "Halfwidth and Fullwidth Forms",
    "Old Italic", // 84
    "Gothic",
    "Deseret",
    "Byzantine Musical Symbols",
    "Musical Symbols",
    "Mathematical Alphanumeric Symbols",
    "CJK Unified Ideographs Extension B",
    "CJK Compatibility Ideographs Supplement",
    "Tags"
  )

  final val blockRanges: String =
    "\u0000\u007F\u0080\u00FF\u0100\u017F\u0180\u024F\u0250\u02AF\u02B0\u02FF\u0300\u036F" +
    "\u0370\u03FF\u0400\u04FF\u0530\u058F\u0590\u05FF\u0600\u06FF\u0700\u074F\u0780\u07BF" +
    "\u0900\u097F\u0980\u09FF\u0A00\u0A7F\u0A80\u0AFF\u0B00\u0B7F\u0B80\u0BFF\u0C00\u0C7F\u0C80\u0CFF" +
    "\u0D00\u0D7F\u0D80\u0DFF\u0E00\u0E7F\u0E80\u0EFF\u0F00\u0FFF\u1000\u109F\u10A0\u10FF\u1100\u11FF" +
    "\u1200\u137F\u13A0\u13FF\u1400\u167F\u1680\u169F\u16A0\u16FF\u1780\u17FF\u1800\u18AF\u1E00\u1EFF" +
    "\u1F00\u1FFF\u2000\u206F\u2070\u209F\u20A0\u20CF\u20D0\u20FF\u2100\u214F\u2150\u218F\u2190\u21FF\u2200\u22FF" +
    "\u2300\u23FF\u2400\u243F\u2440\u245F\u2460\u24FF\u2500\u257F\u2580\u259F\u25A0\u25FF\u2600\u26FF\u2700\u27BF" +
    "\u2800\u28FF\u2E80\u2EFF\u2F00\u2FDF\u2FF0\u2FFF\u3000\u303F\u3040\u309F\u30A0\u30FF\u3100\u312F\u3130\u318F" +
    "\u3190\u319F\u31A0\u31BF\u3200\u32FF\u3300\u33FF\u3400\u4DB5\u4E00\u9FFF\uA000\uA48F\uA490\uA4CF" +
    "\uAC00\uD7A3\uE000\uF8FF\uF900\uFAFF\uFB00\uFB4F\uFB50\uFDFF" +
    "\uFE20\uFE2F\uFE30\uFE4F\uFE50\uFE6F\uFE70\uFEFE\uFEFF\uFEFF\uFF00\uFFEF"

  final val nonBMPBlockRanges: Array[Int] = Array(
    0x10300, 0x1032F, // 84
    0x10330, 0x1034F,
    0x10400, 0x1044F,
    0x1D000, 0x1D0FF,
    0x1D100, 0x1D1FF,
    0x1D400, 0x1D7FF,
    0x20000, 0x2A6D6,
    0x2F800, 0x2FA1F,
    0xE0000, 0xE007F
  )
  private final val NONBMP_BLOCK_START: Int = 84

  def getRange(name: String, positive: Boolean): RangeToken = {
    if (Token.categories.size() == 0) {
      Token.categories.synchronized {
        val ranges = new Array[Token](Token.categoryNames.length)
        var i = 0
        while (i < ranges.length) {
          ranges(i) = Token.createRange()
          i += 1
        }
        var ch = 0
        while (ch < 0x10000) {
          var charType = Character.getType(ch.toChar)
          if (charType == Character.START_PUNCTUATION || charType == Character.END_PUNCTUATION) {
            if (ch == 0x00AB || ch == 0x2018 || ch == 0x201B || ch == 0x201C || ch == 0x201F || ch == 0x2039) {
              charType = CHAR_INIT_QUOTE
            }
            if (ch == 0x00BB || ch == 0x2019 || ch == 0x201D || ch == 0x203A) {
              charType = CHAR_FINAL_QUOTE
            }
          }
          ranges(charType).addRange(ch, ch)
          charType match {
            case Character.UPPERCASE_LETTER | Character.LOWERCASE_LETTER | Character.TITLECASE_LETTER |
                 Character.MODIFIER_LETTER | Character.OTHER_LETTER =>
              charType = CHAR_LETTER
            case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK =>
              charType = CHAR_MARK
            case Character.DECIMAL_DIGIT_NUMBER | Character.LETTER_NUMBER | Character.OTHER_NUMBER =>
              charType = CHAR_NUMBER
            case Character.SPACE_SEPARATOR | Character.LINE_SEPARATOR | Character.PARAGRAPH_SEPARATOR =>
              charType = CHAR_SEPARATOR
            case Character.CONTROL | Character.FORMAT | Character.SURROGATE | Character.PRIVATE_USE |
                 Character.UNASSIGNED =>
              charType = CHAR_OTHER
            case Character.CONNECTOR_PUNCTUATION | Character.DASH_PUNCTUATION | Character.START_PUNCTUATION |
                 Character.END_PUNCTUATION | CHAR_INIT_QUOTE | CHAR_FINAL_QUOTE | Character.OTHER_PUNCTUATION =>
              charType = CHAR_PUNCTUATION
            case Character.MATH_SYMBOL | Character.CURRENCY_SYMBOL | Character.MODIFIER_SYMBOL |
                 Character.OTHER_SYMBOL =>
              charType = CHAR_SYMBOL
            case _ =>
              throw new RuntimeException("org.apache.xerces.utils.regex.Token#getRange(): Unknown Unicode category: " + charType)
          }
          ranges(charType).addRange(ch, ch)
          ch += 1
        }
        ranges(Character.UNASSIGNED.toInt).addRange(0x10000, Token.UTF16_MAX)

        i = 0
        while (i < ranges.length) {
          if (Token.categoryNames(i) != null) {
            if (i == Character.UNASSIGNED) {
              ranges(i).addRange(0x10000, Token.UTF16_MAX)
            }
            Token.categories.put(Token.categoryNames(i), ranges(i))
            Token.categories2.put(Token.categoryNames(i), Token.complementRanges(ranges(i)))
          }
          i += 1
        }

        val buffer = new java.lang.StringBuilder(50)
        i = 0
        while (i < Token.blockNames.length) {
          val r1 = Token.createRange()
          if (i < NONBMP_BLOCK_START) {
            val location = i * 2
            val rstart = Token.blockRanges.charAt(location).toInt
            val rend = Token.blockRanges.charAt(location + 1).toInt
            r1.addRange(rstart, rend)
          } else {
            val location = (i - NONBMP_BLOCK_START) * 2
            r1.addRange(Token.nonBMPBlockRanges(location), Token.nonBMPBlockRanges(location + 1))
          }
          val n = Token.blockNames(i)
          if (n == "Specials")
            r1.addRange(0xfff0, 0xfffd)
          if (n == "Private Use") {
            r1.addRange(0xF0000, 0xFFFFD)
            r1.addRange(0x100000, 0x10FFFD)
          }
          Token.categories.put(n, r1)
          Token.categories2.put(n, Token.complementRanges(r1))
          buffer.setLength(0)
          buffer.append("Is")
          if (n.indexOf(' ') >= 0) {
            var ci = 0
            while (ci < n.length) {
              if (n.charAt(ci) != ' ') buffer.append(n.charAt(ci))
              ci += 1
            }
          } else {
            buffer.append(n)
          }
          Token.setAlias(buffer.toString, n, true)
          i += 1
        }

        // TR#18 1.2
        Token.setAlias("ASSIGNED", "Cn", false)
        Token.setAlias("UNASSIGNED", "Cn", true)
        val all = Token.createRange()
        all.addRange(0, Token.UTF16_MAX)
        Token.categories.put("ALL", all)
        Token.categories2.put("ALL", Token.complementRanges(all))
        Token.registerNonXS("ASSIGNED")
        Token.registerNonXS("UNASSIGNED")
        Token.registerNonXS("ALL")

        val isalpha = Token.createRange()
        isalpha.mergeRanges(ranges(Character.UPPERCASE_LETTER.toInt))
        isalpha.mergeRanges(ranges(Character.LOWERCASE_LETTER.toInt))
        isalpha.mergeRanges(ranges(Character.OTHER_LETTER.toInt))
        Token.categories.put("IsAlpha", isalpha)
        Token.categories2.put("IsAlpha", Token.complementRanges(isalpha))
        Token.registerNonXS("IsAlpha")

        val isalnum = Token.createRange()
        isalnum.mergeRanges(isalpha)
        isalnum.mergeRanges(ranges(Character.DECIMAL_DIGIT_NUMBER.toInt))
        Token.categories.put("IsAlnum", isalnum)
        Token.categories2.put("IsAlnum", Token.complementRanges(isalnum))
        Token.registerNonXS("IsAlnum")

        val isspace = Token.createRange()
        isspace.mergeRanges(Token.token_spaces)
        isspace.mergeRanges(ranges(CHAR_SEPARATOR))
        Token.categories.put("IsSpace", isspace)
        Token.categories2.put("IsSpace", Token.complementRanges(isspace))
        Token.registerNonXS("IsSpace")

        val isword = Token.createRange()
        isword.mergeRanges(isalnum)
        isword.addRange('_', '_')
        Token.categories.put("IsWord", isword)
        Token.categories2.put("IsWord", Token.complementRanges(isword))
        Token.registerNonXS("IsWord")

        val isascii = Token.createRange()
        isascii.addRange(0, 127)
        Token.categories.put("IsASCII", isascii)
        Token.categories2.put("IsASCII", Token.complementRanges(isascii))
        Token.registerNonXS("IsASCII")

        val isnotgraph = Token.createRange()
        isnotgraph.mergeRanges(ranges(CHAR_OTHER))
        isnotgraph.addRange(' ', ' ')
        Token.categories.put("IsGraph", Token.complementRanges(isnotgraph))
        Token.categories2.put("IsGraph", isnotgraph)
        Token.registerNonXS("IsGraph")

        val isxdigit = Token.createRange()
        isxdigit.addRange('0', '9')
        isxdigit.addRange('A', 'F')
        isxdigit.addRange('a', 'f')
        Token.categories.put("IsXDigit", Token.complementRanges(isxdigit))
        Token.categories2.put("IsXDigit", isxdigit)
        Token.registerNonXS("IsXDigit")

        Token.setAlias("IsDigit", "Nd", true)
        Token.setAlias("IsUpper", "Lu", true)
        Token.setAlias("IsLower", "Ll", true)
        Token.setAlias("IsCntrl", "C", true)
        Token.setAlias("IsPrint", "C", false)
        Token.setAlias("IsPunct", "P", true)
        Token.registerNonXS("IsDigit")
        Token.registerNonXS("IsUpper")
        Token.registerNonXS("IsLower")
        Token.registerNonXS("IsCntrl")
        Token.registerNonXS("IsPrint")
        Token.registerNonXS("IsPunct")

        Token.setAlias("alpha", "IsAlpha", true)
        Token.setAlias("alnum", "IsAlnum", true)
        Token.setAlias("ascii", "IsASCII", true)
        Token.setAlias("cntrl", "IsCntrl", true)
        Token.setAlias("digit", "IsDigit", true)
        Token.setAlias("graph", "IsGraph", true)
        Token.setAlias("lower", "IsLower", true)
        Token.setAlias("print", "IsPrint", true)
        Token.setAlias("punct", "IsPunct", true)
        Token.setAlias("space", "IsSpace", true)
        Token.setAlias("upper", "IsUpper", true)
        Token.setAlias("word", "IsWord", true)
        Token.setAlias("xdigit", "IsXDigit", true)
        Token.registerNonXS("alpha")
        Token.registerNonXS("alnum")
        Token.registerNonXS("ascii")
        Token.registerNonXS("cntrl")
        Token.registerNonXS("digit")
        Token.registerNonXS("graph")
        Token.registerNonXS("lower")
        Token.registerNonXS("print")
        Token.registerNonXS("punct")
        Token.registerNonXS("space")
        Token.registerNonXS("upper")
        Token.registerNonXS("word")
        Token.registerNonXS("xdigit")
      }
    }
    val tok = if (positive) Token.categories.get(name) else Token.categories2.get(name)
    tok.asInstanceOf[RangeToken]
  }

  def getRange(name: String, positive: Boolean, xs: Boolean): RangeToken = {
    var range = Token.getRange(name, positive)
    if (xs && range != null && Token.isRegisterNonXS(name))
      range = null
    range
  }

  var nonxs: HashMap[String, String] = null

  protected[regexp] def registerNonXS(name: String): Unit = {
    if (Token.nonxs == null)
      Token.nonxs = new HashMap[String, String]()
    Token.nonxs.put(name, name)
  }

  protected[regexp] def isRegisterNonXS(name: String): Boolean = {
    if (Token.nonxs == null) false
    else Token.nonxs.containsKey(name)
  }

  private def setAlias(newName: String, name: String, positive: Boolean): Unit = {
    val t1 = Token.categories.get(name)
    val t2 = Token.categories2.get(name)
    if (positive) {
      Token.categories.put(newName, t1)
      Token.categories2.put(newName, t2)
    } else {
      Token.categories2.put(newName, t1)
      Token.categories.put(newName, t2)
    }
  }

  // ------------------------------------------------------

  final val viramaString: String =
    "\u094D" + // ;DEVANAGARI SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u09CD" + //;BENGALI SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u0A4D" + //;GURMUKHI SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u0ACD" + //;GUJARATI SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u0B4D" + //;ORIYA SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u0BCD" + //;TAMIL SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u0C4D" + //;TELUGU SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u0CCD" + //;KANNADA SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u0D4D" + //;MALAYALAM SIGN VIRAMA;Mn;9;ON;;;;;N;;;;;
    "\u0E3A" + //;THAI CHARACTER PHINTHU;Mn;9;ON;;;;;N;THAI VOWEL SIGN PHINTHU;;;;
    "\u0F84"   //;TIBETAN MARK HALANTA;Mn;9;ON;;;;;N;TIBETAN VIRAMA;;;;

  private var token_grapheme: Token = null

  def getGraphemePattern(): Token = synchronized {
    if (Token.token_grapheme != null)
      return Token.token_grapheme

    val base_char = Token.createRange()
    base_char.mergeRanges(Token.getRange("ASSIGNED", true))
    base_char.subtractRanges(Token.getRange("M", true))
    base_char.subtractRanges(Token.getRange("C", true))

    val virama = Token.createRange()
    var i = 0
    while (i < Token.viramaString.length) {
      virama.addRange(i, i)
      i += 1
    }

    val combiner_wo_virama = Token.createRange()
    combiner_wo_virama.mergeRanges(Token.getRange("M", true))
    combiner_wo_virama.addRange(0x1160, 0x11ff)
    combiner_wo_virama.addRange(0xff9e, 0xff9f)

    val left = Token.createUnion()
    left.addChild(base_char)
    left.addChild(Token.token_empty)

    var foo: Token = Token.createUnion()
    foo.addChild(Token.createConcat(virama, Token.getRange("L", true)))
    foo.addChild(combiner_wo_virama)

    foo = Token.createClosure(foo)
    foo = Token.createConcat(left, foo)

    Token.token_grapheme = foo
    Token.token_grapheme
  }

  private var token_ccs: Token = null

  def getCombiningCharacterSequence(): Token = synchronized {
    if (Token.token_ccs != null)
      return Token.token_ccs

    var foo: Token = Token.createClosure(Token.getRange("M", true))
    foo = Token.createConcat(Token.getRange("M", false), foo)
    Token.token_ccs = foo
    Token.token_ccs
  }

  // ------------------------------------------------------
  class StringToken(tokenType: Int, var string: String, val refNumber: Int)
      extends Token(tokenType) with java.io.Serializable {

    override def getReferenceNumber: Int = this.refNumber
    override def getString: String = this.string

    override def toString(options: Int): String = {
      if (this.tokenType == BACKREFERENCE)
        "\\" + this.refNumber
      else
        REUtil.quoteMeta(this.string)
    }
  }

  class ConcatToken(val child: Token, val child2: Token)
      extends Token(Token.CONCAT) with java.io.Serializable {

    override def size(): Int = 2
    override def getChild(index: Int): Token = if (index == 0) this.child else this.child2

    override def toString(options: Int): String = {
      if (this.child2.tokenType == CLOSURE && (this.child2.getChild(0) eq this.child)) {
        this.child.toString(options) + "+"
      } else if (this.child2.tokenType == NONGREEDYCLOSURE && (this.child2.getChild(0) eq this.child)) {
        this.child.toString(options) + "+?"
      } else {
        this.child.toString(options) + this.child2.toString(options)
      }
    }
  }

  class CharToken(tokenType: Int, val chardata: Int)
      extends Token(tokenType) with java.io.Serializable {

    override def getChar: Int = this.chardata

    override def toString(options: Int): String = {
      this.tokenType match {
        case CHAR =>
          this.chardata match {
            case '|' | '*' | '+' | '?' | '(' | ')' | '.' | '[' | '{' | '\\' =>
              "\\" + this.chardata.toChar
            case '\f' => "\\f"
            case '\n' => "\\n"
            case '\r' => "\\r"
            case '\t' => "\\t"
            case 0x1b => "\\e"
            case _ =>
              if (this.chardata >= 0x10000) {
                val pre = "0" + java.lang.Integer.toHexString(this.chardata)
                "\\v" + pre.substring(pre.length - 6, pre.length)
              } else {
                this.chardata.toChar.toString
              }
          }
        case ANCHOR =>
          if ((this eq Token.token_linebeginning) || (this eq Token.token_lineend))
            this.chardata.toChar.toString
          else
            "\\" + this.chardata.toChar
        case _ => null
      }
    }

    override def `match`(ch: Int): Boolean = {
      if (this.tokenType == CHAR) {
        ch == this.chardata
      } else {
        throw new RuntimeException("NFAArrow#match(): Internal error: " + this.tokenType)
      }
    }
  }

  class ClosureToken(tokenType: Int, val child: Token)
      extends Token(tokenType) with java.io.Serializable {

    private var min: Int = -1
    private var max: Int = -1

    override def size(): Int = 1
    override def getChild(index: Int): Token = this.child

    override def setMin(min: Int): Unit = this.min = min
    override def setMax(max: Int): Unit = this.max = max
    override def getMin: Int = this.min
    override def getMax: Int = this.max

    override def toString(options: Int): String = {
      if (this.tokenType == CLOSURE) {
        if (this.getMin < 0 && this.getMax < 0) {
          this.child.toString(options) + "*"
        } else if (this.getMin == this.getMax) {
          this.child.toString(options) + "{" + this.getMin + "}"
        } else if (this.getMin >= 0 && this.getMax >= 0) {
          this.child.toString(options) + "{" + this.getMin + "," + this.getMax + "}"
        } else if (this.getMin >= 0 && this.getMax < 0) {
          this.child.toString(options) + "{" + this.getMin + ",}"
        } else {
          throw new RuntimeException("Token#toString(): CLOSURE " + this.getMin + ", " + this.getMax)
        }
      } else {
        if (this.getMin < 0 && this.getMax < 0) {
          this.child.toString(options) + "*?"
        } else if (this.getMin == this.getMax) {
          this.child.toString(options) + "{" + this.getMin + "}?"
        } else if (this.getMin >= 0 && this.getMax >= 0) {
          this.child.toString(options) + "{" + this.getMin + "," + this.getMax + "}?"
        } else if (this.getMin >= 0 && this.getMax < 0) {
          this.child.toString(options) + "{" + this.getMin + ",}?"
        } else {
          throw new RuntimeException("Token#toString(): NONGREEDYCLOSURE " + this.getMin + ", " + this.getMax)
        }
      }
    }
  }

  class ParenToken(tokenType: Int, val child: Token, val parennumber: Int)
      extends Token(tokenType) with java.io.Serializable {

    override def size(): Int = 1
    override def getChild(index: Int): Token = this.child
    override def getParenNumber: Int = this.parennumber

    override def toString(options: Int): String = {
      this.tokenType match {
        case PAREN =>
          if (this.parennumber == 0)
            "(?:" + this.child.toString(options) + ")"
          else
            "(" + this.child.toString(options) + ")"
        case LOOKAHEAD => "(?=" + this.child.toString(options) + ")"
        case NEGATIVELOOKAHEAD => "(?!" + this.child.toString(options) + ")"
        case LOOKBEHIND => "(?<=" + this.child.toString(options) + ")"
        case NEGATIVELOOKBEHIND => "(?<!" + this.child.toString(options) + ")"
        case INDEPENDENT => "(?>" + this.child.toString(options) + ")"
        case _ => null
      }
    }
  }

  class ConditionToken(val refNumber: Int, val condition: Token, val yes: Token, val no: Token)
      extends Token(Token.CONDITION) with java.io.Serializable {

    override def size(): Int = if (this.no == null) 1 else 2
    override def getChild(index: Int): Token = {
      if (index == 0) return this.yes
      if (index == 1) return this.no
      throw new RuntimeException("Internal Error: " + index)
    }

    override def toString(options: Int): String = {
      var ret = if (refNumber > 0) {
        "(?(" + refNumber + ")"
      } else if (this.condition.tokenType == Token.ANCHOR) {
        "(?(" + this.condition.toString(options) + ")"
      } else {
        "(?" + this.condition.toString(options)
      }

      if (this.no == null) {
        ret = ret + this.yes.toString(options) + ")"
      } else {
        ret = ret + this.yes.toString(options) + "|" + this.no.toString(options) + ")"
      }
      ret
    }
  }

  class ModifierToken(val child: Token, val add: Int, val mask: Int)
      extends Token(Token.MODIFIERGROUP) with java.io.Serializable {

    override def size(): Int = 1
    override def getChild(index: Int): Token = this.child

    def getOptions: Int = this.add
    def getOptionsMask: Int = this.mask

    override def toString(options: Int): String = {
      "(?" +
        (if (this.add == 0) "" else REUtil.createOptionString(this.add)) +
        (if (this.mask == 0) "" else REUtil.createOptionString(this.mask)) +
        ":" +
        this.child.toString(options) +
        ")"
    }
  }

  class UnionToken(tokenType: Int) extends Token(tokenType) with java.io.Serializable {
    var children: ArrayList[Token] = null

    override def addChild(tok: Token): Unit = {
      if (tok == null) return
      if (this.children == null) this.children = new ArrayList[Token]()
      if (this.tokenType == UNION) {
        this.children.add(tok)
        return
      }
      // This is CONCAT, and new child is CONCAT.
      if (tok.tokenType == CONCAT) {
        var i = 0
        while (i < tok.size()) {
          this.addChild(tok.getChild(i))
          i += 1
        }
        return
      }
      val sz = this.children.size()
      if (sz == 0) {
        this.children.add(tok)
        return
      }
      var previous = this.children.get(sz - 1)
      if (!((previous.tokenType == CHAR || previous.tokenType == STRING)
        && (tok.tokenType == CHAR || tok.tokenType == STRING))) {
        this.children.add(tok)
        return
      }

      val nextMaxLength = if (tok.tokenType == CHAR) 2 else tok.getString.length
      val buffer = if (previous.tokenType == CHAR) {
        val buf = new java.lang.StringBuilder(2 + nextMaxLength)
        val ch = previous.getChar
        if (ch >= 0x10000)
          buf.append(REUtil.decomposeToSurrogates(ch))
        else
          buf.append(ch.toChar)
        previous = Token.createString(null)
        this.children.set(sz - 1, previous)
        buf
      } else {
        val buf = new java.lang.StringBuilder(previous.getString.length + nextMaxLength)
        buf.append(previous.getString)
        buf
      }

      if (tok.tokenType == CHAR) {
        val ch = tok.getChar
        if (ch >= 0x10000)
          buffer.append(REUtil.decomposeToSurrogates(ch))
        else
          buffer.append(ch.toChar)
      } else {
        buffer.append(tok.getString)
      }

      previous.asInstanceOf[StringToken].string = buffer.toString
    }

    override def size(): Int = if (this.children == null) 0 else this.children.size()
    override def getChild(index: Int): Token = this.children.get(index)

    override def toString(options: Int): String = {
      if (this.tokenType == CONCAT) {
        if (this.children.size() == 2) {
          val ch = this.getChild(0)
          val ch2 = this.getChild(1)
          if (ch2.tokenType == CLOSURE && (ch2.getChild(0) eq ch)) {
            ch.toString(options) + "+"
          } else if (ch2.tokenType == NONGREEDYCLOSURE && (ch2.getChild(0) eq ch)) {
            ch.toString(options) + "+?"
          } else {
            ch.toString(options) + ch2.toString(options)
          }
        } else {
          val sb = new java.lang.StringBuilder()
          var i = 0
          while (i < this.children.size()) {
            sb.append(this.children.get(i).toString(options))
            i += 1
          }
          sb.toString
        }
      } else {
        if (this.children.size() == 2 && this.getChild(1).tokenType == EMPTY) {
          this.getChild(0).toString(options) + "?"
        } else if (this.children.size() == 2 && this.getChild(0).tokenType == EMPTY) {
          this.getChild(1).toString(options) + "??"
        } else {
          val sb = new java.lang.StringBuilder()
          sb.append(this.children.get(0).toString(options))
          var i = 1
          while (i < this.children.size()) {
            sb.append('|')
            sb.append(this.children.get(i).toString(options))
            i += 1
          }
          sb.toString
        }
      }
    }
  }
}
