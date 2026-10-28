/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
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

import java.util.{ArrayList, Locale}

/**
 * A Regular Expression Parser.
 *
 * @version RegexParser.java,v 1.8 2003/03/25 14:47:06 sandygao Exp
 */
class RegexParser {
  import RegexParser.*

  var offset: Int = 0
  var regex: String = null
  var regexlen: Int = 0
  var options: Int = 0
  var chardata: Int = 0
  var nexttoken: Int = 0
  var context: Int = S_NORMAL
  var parennumber: Int = 1
  var hasBackReferences: Boolean = false
  var references: ArrayList[ReferencePosition] = null

  def this(locale: Locale) = {
    this()
    this.setLocale(locale)
  }

  def setLocale(locale: Locale): Unit = ()

  final def ex(key: String, loc: Int): ParseException = {
    new ParseException(DefaultMessages.getOrElse(key, key), loc)
  }

  private def isSet(flag: Int): Boolean = {
    (this.options & flag) == flag
  }

  @throws[ParseException]
  def parse(regex0: String, options: Int): Token = synchronized {
    this.options = options
    this.offset = 0
    this.setContext(S_NORMAL)
    this.parennumber = 1
    this.hasBackReferences = false
    this.regex = regex0
    if (this.isSet(RegularExpression.EXTENDED_COMMENT))
      this.regex = REUtil.stripExtendedComment(this.regex)
    this.regexlen = this.regex.length

    this.next()
    val ret = this.parseRegex()
    if (this.offset != this.regexlen)
      throw ex("parser.parse.1", this.offset)
    if (this.references != null) {
      var i = 0
      while (i < this.references.size()) {
        val position = this.references.get(i)
        if (this.parennumber <= position.refNumber)
          throw ex("parser.parse.2", position.position)
        i += 1
      }
      this.references.clear()
    }
    ret
  }

  protected final def setContext(con: Int): Unit = {
    this.context = con
  }

  final def read(): Int = {
    this.nexttoken
  }

  final def next(): Unit = {
    if (this.offset >= this.regexlen) {
      this.chardata = -1
      this.nexttoken = T_EOF
      return
    }

    var ret = 0
    val ch = this.regex.charAt(this.offset).toInt
    this.offset += 1
    this.chardata = ch

    if (this.context == S_INBRACKETS) {
      ch match {
        case '\\' =>
          ret = T_BACKSOLIDUS
          if (this.offset >= this.regexlen)
            throw ex("parser.next.1", this.offset - 1)
          this.chardata = this.regex.charAt(this.offset).toInt
          this.offset += 1

        case '-' =>
          if (this.isSet(RegularExpression.XMLSCHEMA_MODE)
            && this.offset < this.regexlen && this.regex.charAt(this.offset) == '[') {
            this.offset += 1
            ret = T_XMLSCHEMA_CC_SUBTRACTION
          } else {
            ret = T_CHAR
          }

        case '[' =>
          if (!this.isSet(RegularExpression.XMLSCHEMA_MODE)
            && this.offset < this.regexlen && this.regex.charAt(this.offset) == ':') {
            this.offset += 1
            ret = T_POSIX_CHARCLASS_START
          } else {
            if (REUtil.isHighSurrogate(ch) && this.offset < this.regexlen) {
              val low = this.regex.charAt(this.offset).toInt
              if (REUtil.isLowSurrogate(low)) {
                this.chardata = REUtil.composeFromSurrogates(ch, low)
                this.offset += 1
              }
            }
            ret = T_CHAR
          }

        case _ =>
          if (REUtil.isHighSurrogate(ch) && this.offset < this.regexlen) {
            val low = this.regex.charAt(this.offset).toInt
            if (REUtil.isLowSurrogate(low)) {
              this.chardata = REUtil.composeFromSurrogates(ch, low)
              this.offset += 1
            }
          }
          ret = T_CHAR
      }
      this.nexttoken = ret
      return
    }

    ch match {
      case '|' => ret = T_OR
      case '*' => ret = T_STAR
      case '+' => ret = T_PLUS
      case '?' => ret = T_QUESTION
      case ')' => ret = T_RPAREN
      case '.' => ret = T_DOT
      case '[' => ret = T_LBRACKET
      case '^' => ret = T_CARET
      case '$' => ret = T_DOLLAR
      case '(' =>
        ret = T_LPAREN
        if (this.offset < this.regexlen && this.regex.charAt(this.offset) == '?') {
          this.offset += 1
          if (this.offset >= this.regexlen)
            throw ex("parser.next.2", this.offset - 1)
          var ch2 = this.regex.charAt(this.offset).toInt
          this.offset += 1
          ch2 match {
            case ':' => ret = T_LPAREN2
            case '=' => ret = T_LOOKAHEAD
            case '!' => ret = T_NEGATIVELOOKAHEAD
            case '[' => ret = T_SET_OPERATIONS
            case '>' => ret = T_INDEPENDENT
            case '<' =>
              if (this.offset >= this.regexlen)
                throw ex("parser.next.2", this.offset - 3)
              ch2 = this.regex.charAt(this.offset).toInt
              this.offset += 1
              if (ch2 == '=') {
                ret = T_LOOKBEHIND
              } else if (ch2 == '!') {
                ret = T_NEGATIVELOOKBEHIND
              } else {
                throw ex("parser.next.3", this.offset - 3)
              }
            case '#' =>
              while (this.offset < this.regexlen) {
                ch2 = this.regex.charAt(this.offset).toInt
                this.offset += 1
                if (ch2 == ')') {
                  // break
                  this.offset -= 1 // rewind for check below
                  this.offset += 1
                  ch2 = ')'
                  // break out of while
                  this.offset = this.regexlen + 1 // sentinel
                }
              }
              // re-check actual loop termination
              if (this.offset <= this.regexlen && ch2 != ')')
                throw ex("parser.next.4", this.offset - 1)
              ret = T_COMMENT
            case _ =>
              if (ch2 == '-' || ('a' <= ch2 && ch2 <= 'z') || ('A' <= ch2 && ch2 <= 'Z')) {
                this.offset -= 1
                ret = T_MODIFIERS
              } else if (ch2 == '(') {
                ret = T_CONDITION
              } else {
                throw ex("parser.next.2", this.offset - 2)
              }
          }
        }

      case '\\' =>
        ret = T_BACKSOLIDUS
        if (this.offset >= this.regexlen)
          throw ex("parser.next.1", this.offset - 1)
        this.chardata = this.regex.charAt(this.offset).toInt
        this.offset += 1

      case _ =>
        ret = T_CHAR
    }
    this.nexttoken = ret
  }

  @throws[ParseException]
  def parseRegex(): Token = {
    var tok = this.parseTerm()
    var parent: Token = null
    while (this.read() == T_OR) {
      this.next() // '|'
      if (parent == null) {
        parent = Token.createUnion()
        parent.addChild(tok)
        tok = parent
      }
      tok.addChild(this.parseTerm())
    }
    tok
  }

  @throws[ParseException]
  def parseTerm(): Token = {
    var ch = this.read()
    if (ch == T_OR || ch == T_RPAREN || ch == T_EOF) {
      Token.createEmpty()
    } else {
      var tok = this.parseFactor()
      var concat: Token = null
      ch = this.read()
      while (ch != T_OR && ch != T_RPAREN && ch != T_EOF) {
        if (concat == null) {
          concat = Token.createConcat()
          concat.addChild(tok)
          tok = concat
        }
        concat.addChild(this.parseFactor())
        ch = this.read()
      }
      tok
    }
  }

  @throws[ParseException]
  def processCaret(): Token = {
    this.next()
    Token.token_linebeginning
  }

  @throws[ParseException]
  def processDollar(): Token = {
    this.next()
    Token.token_lineend
  }

  @throws[ParseException]
  def processLookahead(): Token = {
    this.next()
    val tok = Token.createLook(Token.LOOKAHEAD, this.parseRegex())
    if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
    this.next() // ')'
    tok
  }

  @throws[ParseException]
  def processNegativelookahead(): Token = {
    this.next()
    val tok = Token.createLook(Token.NEGATIVELOOKAHEAD, this.parseRegex())
    if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
    this.next() // ')'
    tok
  }

  @throws[ParseException]
  def processLookbehind(): Token = {
    this.next()
    val tok = Token.createLook(Token.LOOKBEHIND, this.parseRegex())
    if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
    this.next() // ')'
    tok
  }

  @throws[ParseException]
  def processNegativelookbehind(): Token = {
    this.next()
    val tok = Token.createLook(Token.NEGATIVELOOKBEHIND, this.parseRegex())
    if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
    this.next() // ')'
    tok
  }

  @throws[ParseException]
  def processBacksolidus_A(): Token = {
    this.next()
    Token.token_stringbeginning
  }

  @throws[ParseException]
  def processBacksolidus_Z(): Token = {
    this.next()
    Token.token_stringend2
  }

  @throws[ParseException]
  def processBacksolidus_z(): Token = {
    this.next()
    Token.token_stringend
  }

  @throws[ParseException]
  def processBacksolidus_b(): Token = {
    this.next()
    Token.token_wordedge
  }

  @throws[ParseException]
  def processBacksolidus_B(): Token = {
    this.next()
    Token.token_not_wordedge
  }

  @throws[ParseException]
  def processBacksolidus_lt(): Token = {
    this.next()
    Token.token_wordbeginning
  }

  @throws[ParseException]
  def processBacksolidus_gt(): Token = {
    this.next()
    Token.token_wordend
  }

  @throws[ParseException]
  def processStar(tok: Token): Token = {
    this.next()
    if (this.read() == T_QUESTION) {
      this.next()
      Token.createNGClosure(tok)
    } else {
      Token.createClosure(tok)
    }
  }

  @throws[ParseException]
  def processPlus(tok: Token): Token = {
    this.next()
    if (this.read() == T_QUESTION) {
      this.next()
      Token.createConcat(tok, Token.createNGClosure(tok))
    } else {
      Token.createConcat(tok, Token.createClosure(tok))
    }
  }

  @throws[ParseException]
  def processQuestion(tok: Token): Token = {
    this.next()
    val par = Token.createUnion()
    if (this.read() == T_QUESTION) {
      this.next()
      par.addChild(Token.createEmpty())
      par.addChild(tok)
    } else {
      par.addChild(tok)
      par.addChild(Token.createEmpty())
    }
    par
  }

  def checkQuestion(off: Int): Boolean = {
    off < this.regexlen && this.regex.charAt(off) == '?'
  }

  @throws[ParseException]
  def processParen(): Token = {
    this.next()
    val p = this.parennumber
    this.parennumber += 1
    val tok = Token.createParen(this.parseRegex(), p)
    if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
    this.next() // Skips ')'
    tok
  }

  @throws[ParseException]
  def processParen2(): Token = {
    this.next()
    val tok = Token.createParen(this.parseRegex(), 0)
    if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
    this.next() // Skips ')'
    tok
  }

  @throws[ParseException]
  def processCondition(): Token = {
    if (this.offset + 1 >= this.regexlen) throw ex("parser.factor.4", this.offset)
    var refno = -1
    var condition: Token = null
    val ch = this.regex.charAt(this.offset)
    if ('1' <= ch && ch <= '9') {
      refno = ch - '0'
      this.hasBackReferences = true
      if (this.references == null) this.references = new ArrayList[ReferencePosition]()
      this.references.add(new ReferencePosition(refno, this.offset))
      this.offset += 1
      if (this.regex.charAt(this.offset) != ')') throw ex("parser.factor.1", this.offset)
      this.offset += 1
    } else {
      if (ch == '?') this.offset -= 1
      this.next()
      condition = this.parseFactor()
      condition.tokenType match {
        case Token.LOOKAHEAD | Token.NEGATIVELOOKAHEAD | Token.LOOKBEHIND | Token.NEGATIVELOOKBEHIND =>
        case Token.ANCHOR =>
          if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
        case _ =>
          throw ex("parser.factor.5", this.offset)
      }
    }
    this.next()
    val yesPattern = this.parseRegex()
    var noPattern: Token = null
    var finalYes = yesPattern
    if (yesPattern.tokenType == Token.UNION) {
      if (yesPattern.size() != 2) throw ex("parser.factor.6", this.offset)
      noPattern = yesPattern.getChild(1)
      finalYes = yesPattern.getChild(0)
    }
    if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
    this.next()
    Token.createCondition(refno, condition, finalYes, noPattern)
  }

  @throws[ParseException]
  def processModifiers(): Token = {
    var add = 0
    var mask = 0
    var ch = -1
    var breakLoop = false
    while (this.offset < this.regexlen && !breakLoop) {
      ch = this.regex.charAt(this.offset).toInt
      val v = REUtil.getOptionValue(ch)
      if (v == 0) {
        breakLoop = true
      } else {
        add |= v
        this.offset += 1
      }
    }
    if (this.offset >= this.regexlen) throw ex("parser.factor.2", this.offset - 1)
    if (ch == '-') {
      this.offset += 1
      breakLoop = false
      while (this.offset < this.regexlen && !breakLoop) {
        ch = this.regex.charAt(this.offset).toInt
        val v = REUtil.getOptionValue(ch)
        if (v == 0) {
          breakLoop = true
        } else {
          mask |= v
          this.offset += 1
        }
      }
      if (this.offset >= this.regexlen) throw ex("parser.factor.2", this.offset - 1)
    }
    var tok: Token = null
    if (ch == ':') {
      this.offset += 1
      this.next()
      tok = Token.createModifierGroup(this.parseRegex(), add, mask)
      if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
      this.next()
    } else if (ch == ')') {
      this.offset += 1
      this.next()
      tok = Token.createModifierGroup(this.parseRegex(), add, mask)
    } else {
      throw ex("parser.factor.3", this.offset)
    }
    tok
  }

  @throws[ParseException]
  def processIndependent(): Token = {
    this.next()
    val tok = Token.createLook(Token.INDEPENDENT, this.parseRegex())
    if (this.read() != T_RPAREN) throw ex("parser.factor.1", this.offset - 1)
    this.next()
    tok
  }

  @throws[ParseException]
  def processBacksolidus_c(): Token = {
    if (this.offset >= this.regexlen)
      throw ex("parser.atom.1", this.offset - 1)
    val ch2 = this.regex.charAt(this.offset).toInt
    this.offset += 1
    if ((ch2 & 0xffe0) != 0x0040)
      throw ex("parser.atom.1", this.offset - 1)
    this.next()
    Token.createChar(ch2 - 0x40)
  }

  @throws[ParseException]
  def processBacksolidus_C(): Token = {
    throw ex("parser.process.1", this.offset)
  }

  @throws[ParseException]
  def processBacksolidus_i(): Token = {
    val tok = Token.createChar('i')
    this.next()
    tok
  }

  @throws[ParseException]
  def processBacksolidus_I(): Token = {
    throw ex("parser.process.1", this.offset)
  }

  @throws[ParseException]
  def processBacksolidus_g(): Token = {
    this.next()
    Token.getGraphemePattern()
  }

  @throws[ParseException]
  def processBacksolidus_X(): Token = {
    this.next()
    Token.getCombiningCharacterSequence()
  }

  @throws[ParseException]
  def processBackreference(): Token = {
    val refnum = this.chardata - '0'
    val tok = Token.createBackReference(refnum)
    this.hasBackReferences = true
    if (this.references == null) this.references = new ArrayList[ReferencePosition]()
    this.references.add(new ReferencePosition(refnum, this.offset - 2))
    this.next()
    tok
  }

  @throws[ParseException]
  def parseFactor(): Token = {
    val ch = this.read()
    var tok: Token = null
    ch match {
      case T_CARET if this.isSet(RegularExpression.XMLSCHEMA_MODE) =>
        tok = this.processCaret()
      case T_CARET => return this.processCaret()
      case T_DOLLAR if this.isSet(RegularExpression.XMLSCHEMA_MODE) =>
        tok = this.processDollar()
      case T_DOLLAR => return this.processDollar()
      case T_LOOKAHEAD => return this.processLookahead()
      case T_NEGATIVELOOKAHEAD => return this.processNegativelookahead()
      case T_LOOKBEHIND => return this.processLookbehind()
      case T_NEGATIVELOOKBEHIND => return this.processNegativelookbehind()
      case T_COMMENT =>
        this.next()
        return Token.createEmpty()
      case T_BACKSOLIDUS =>
        this.chardata match {
          case 'A' => return this.processBacksolidus_A()
          case 'Z' => return this.processBacksolidus_Z()
          case 'z' => return this.processBacksolidus_z()
          case 'b' => return this.processBacksolidus_b()
          case 'B' => return this.processBacksolidus_B()
          case '<' => return this.processBacksolidus_lt()
          case '>' => return this.processBacksolidus_gt()
          case _ =>
        }
      case _ =>
    }
    if (tok == null)
      tok = this.parseAtom()
    var ch2 = this.read()
    ch2 match {
      case T_STAR => return this.processStar(tok)
      case T_PLUS => return this.processPlus(tok)
      case T_QUESTION => return this.processQuestion(tok)
      case T_CHAR =>
        if (this.chardata == '{' && this.offset < this.regexlen) {
          var off = this.offset
          var min = 0
          var max = -1
          var c = this.regex.charAt(off).toInt
          off += 1

          if (c >= '0' && c <= '9') {
            min = c - '0'
            var breakDigits = false
            while (off < this.regexlen && !breakDigits) {
              c = this.regex.charAt(off).toInt
              off += 1
              if (c >= '0' && c <= '9') {
                min = min * 10 + c - '0'
                if (min < 0) throw ex("parser.quantifier.5", this.offset)
              } else {
                breakDigits = true
              }
            }
          } else {
            throw ex("parser.quantifier.1", this.offset)
          }

          max = min
          if (c == ',') {
            if (off >= this.regexlen) {
              throw ex("parser.quantifier.3", this.offset)
            } else {
              c = this.regex.charAt(off).toInt
              off += 1
              if (c >= '0' && c <= '9') {
                max = c - '0'
                var breakMax = false
                while (off < this.regexlen && !breakMax) {
                  c = this.regex.charAt(off).toInt
                  off += 1
                  if (c >= '0' && c <= '9') {
                    max = max * 10 + c - '0'
                    if (max < 0) throw ex("parser.quantifier.5", this.offset)
                  } else {
                    breakMax = true
                  }
                }
                if (min > max) throw ex("parser.quantifier.4", this.offset)
              } else {
                max = -1
              }
            }
          }

          if (c != '}') throw ex("parser.quantifier.2", this.offset)

          if (this.checkQuestion(off)) {
            tok = Token.createNGClosure(tok)
            this.offset = off + 1
          } else {
            tok = Token.createClosure(tok)
            this.offset = off
          }

          tok.setMin(min)
          tok.setMax(max)
          this.next()
        }
      case _ =>
    }
    tok
  }

  @throws[ParseException]
  def parseAtom(): Token = {
    val ch = this.read()
    var tok: Token = null
    ch match {
      case T_LPAREN => return this.processParen()
      case T_LPAREN2 => return this.processParen2()
      case T_CONDITION => return this.processCondition()
      case T_MODIFIERS => return this.processModifiers()
      case T_INDEPENDENT => return this.processIndependent()
      case T_DOT =>
        this.next()
        tok = Token.token_dot

      case T_LBRACKET => return this.parseCharacterClass(true)
      case T_SET_OPERATIONS => return this.parseSetOperations()

      case T_BACKSOLIDUS =>
        this.chardata match {
          case 'd' | 'D' | 'w' | 'W' | 's' | 'S' =>
            tok = this.getTokenForShorthand(this.chardata)
            this.next()
            return tok

          case 'e' | 'f' | 'n' | 'r' | 't' | 'u' | 'v' | 'x' =>
            val ch2 = this.decodeEscaped()
            if (ch2 < 0x10000) {
              tok = Token.createChar(ch2)
            } else {
              tok = Token.createString(REUtil.decomposeToSurrogates(ch2))
            }

          case 'c' => return this.processBacksolidus_c()
          case 'C' => return this.processBacksolidus_C()
          case 'i' => return this.processBacksolidus_i()
          case 'I' => return this.processBacksolidus_I()
          case 'g' => return this.processBacksolidus_g()
          case 'X' => return this.processBacksolidus_X()
          case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            return this.processBackreference()

          case 'P' | 'p' =>
            val pstart = this.offset
            tok = processBacksolidus_pP(this.chardata)
            if (tok == null) throw this.ex("parser.atom.5", pstart)

          case _ =>
            tok = Token.createChar(this.chardata)
        }
        this.next()

      case T_CHAR =>
        if (this.chardata == ']' || this.chardata == '{' || this.chardata == '}')
          throw this.ex("parser.atom.4", this.offset - 1)
        tok = Token.createChar(this.chardata)
        val high = this.chardata
        this.next()
        if (REUtil.isHighSurrogate(high) && this.read() == T_CHAR && REUtil.isLowSurrogate(this.chardata)) {
          val sur = Array[Char](high.toChar, this.chardata.toChar)
          tok = Token.createParen(Token.createString(new String(sur)), 0)
          this.next()
        }

      case _ =>
        throw this.ex("parser.atom.4", this.offset - 1)
    }
    tok
  }

  @throws[ParseException]
  protected def processBacksolidus_pP(c: Int): RangeToken = {
    this.next()
    if (this.read() != T_CHAR || this.chardata != '{')
      throw this.ex("parser.atom.2", this.offset - 1)

    val positive = c == 'p'
    val namestart = this.offset
    val nameend = this.regex.indexOf('}', namestart)
    if (nameend < 0)
      throw this.ex("parser.atom.3", this.offset)

    val pname = this.regex.substring(namestart, nameend)
    this.offset = nameend + 1
    Token.getRange(pname, positive, this.isSet(RegularExpression.XMLSCHEMA_MODE))
  }

  def processCIinCharacterClass(tok: RangeToken, c: Int): Int = {
    this.decodeEscaped()
  }

  @throws[ParseException]
  protected def parseCharacterClass(useNrange: Boolean): RangeToken = {
    this.setContext(S_INBRACKETS)
    this.next() // '['
    var nrange = false
    var base: RangeToken = null
    var tok: RangeToken = null
    if (this.read() == T_CHAR && this.chardata == '^') {
      nrange = true
      this.next() // '^'
      if (useNrange) {
        tok = Token.createNRange()
      } else {
        base = Token.createRange()
        base.addRange(0, Token.UTF16_MAX)
        tok = Token.createRange()
      }
    } else {
      tok = Token.createRange()
    }
    var firstloop = true
    var type_ = this.read()
    var breakClass = false
    while (type_ != T_EOF && !breakClass) {
      if (type_ == T_CHAR && this.chardata == ']' && !firstloop) {
        breakClass = true
      } else {
        firstloop = false
        var c = this.chardata
        var end = false
        if (type_ == T_BACKSOLIDUS) {
          c match {
            case 'd' | 'D' | 'w' | 'W' | 's' | 'S' =>
              tok.mergeRanges(this.getTokenForShorthand(c))
              end = true
            case 'i' | 'I' | 'c' | 'C' =>
              c = this.processCIinCharacterClass(tok, c)
              if (c < 0) end = true
            case 'p' | 'P' =>
              val pstart = this.offset
              val tok2 = this.processBacksolidus_pP(c)
              if (tok2 == null) throw this.ex("parser.atom.5", pstart)
              tok.mergeRanges(tok2)
              end = true
            case _ =>
              c = this.decodeEscaped()
          }
        } else if (type_ == T_POSIX_CHARCLASS_START) {
          val nameend = this.regex.indexOf(':', this.offset)
          if (nameend < 0) throw this.ex("parser.cc.1", this.offset)
          var positive = true
          if (this.regex.charAt(this.offset) == '^') {
            this.offset += 1
            positive = false
          }
          val name = this.regex.substring(this.offset, nameend)
          val range = Token.getRange(name, positive, this.isSet(RegularExpression.XMLSCHEMA_MODE))
          if (range == null) throw this.ex("parser.cc.3", this.offset)
          tok.mergeRanges(range)
          end = true
          if (nameend + 1 >= this.regexlen || this.regex.charAt(nameend + 1) != ']')
            throw this.ex("parser.cc.1", nameend)
          this.offset = nameend + 2
        }
        this.next()
        if (!end) {
          if (this.read() != T_CHAR || this.chardata != '-') {
            tok.addRange(c, c)
          } else {
            this.next() // Skips '-'
            type_ = this.read()
            if (type_ == T_EOF) throw this.ex("parser.cc.2", this.offset)
            if (type_ == T_CHAR && this.chardata == ']') {
              tok.addRange(c, c)
              tok.addRange('-', '-')
            } else {
              var rangeend = this.chardata
              if (type_ == T_BACKSOLIDUS)
                rangeend = this.decodeEscaped()
              this.next()
              tok.addRange(c, rangeend)
            }
          }
        }
        if (this.isSet(RegularExpression.SPECIAL_COMMA)
          && this.read() == T_CHAR && this.chardata == ',')
          this.next()

        type_ = this.read()
      }
    }
    if (this.read() == T_EOF)
      throw this.ex("parser.cc.2", this.offset)
    if (!useNrange && nrange) {
      base.subtractRanges(tok)
      tok = base
    }
    tok.sortRanges()
    tok.compactRanges()
    this.setContext(S_NORMAL)
    this.next() // Skips ']'
    tok
  }

  @throws[ParseException]
  protected def parseSetOperations(): RangeToken = {
    val tok = this.parseCharacterClass(false)
    var type_ = this.read()
    while (type_ != T_RPAREN) {
      val ch = this.chardata
      if ((type_ == T_CHAR && (ch == '-' || ch == '&')) || type_ == T_PLUS) {
        this.next()
        if (this.read() != T_LBRACKET) throw ex("parser.ope.1", this.offset - 1)
        val t2 = this.parseCharacterClass(false)
        if (type_ == T_PLUS)
          tok.mergeRanges(t2)
        else if (ch == '-')
          tok.subtractRanges(t2)
        else if (ch == '&')
          tok.intersectRanges(t2)
        else
          throw new RuntimeException("ASSERT")
      } else {
        throw ex("parser.ope.2", this.offset - 1)
      }
      type_ = this.read()
    }
    this.next()
    tok
  }

  def getTokenForShorthand(ch: Int): Token = {
    val tok: Token = ch match {
      case 'd' =>
        if (this.isSet(RegularExpression.USE_UNICODE_CATEGORY))
          Token.getRange("Nd", true)
        else Token.token_0to9
      case 'D' =>
        if (this.isSet(RegularExpression.USE_UNICODE_CATEGORY))
          Token.getRange("Nd", false)
        else Token.token_not_0to9
      case 'w' =>
        if (this.isSet(RegularExpression.USE_UNICODE_CATEGORY))
          Token.getRange("IsWord", true)
        else Token.token_wordchars
      case 'W' =>
        if (this.isSet(RegularExpression.USE_UNICODE_CATEGORY))
          Token.getRange("IsWord", false)
        else Token.token_not_wordchars
      case 's' =>
        if (this.isSet(RegularExpression.USE_UNICODE_CATEGORY))
          Token.getRange("IsSpace", true)
        else Token.token_spaces
      case 'S' =>
        if (this.isSet(RegularExpression.USE_UNICODE_CATEGORY))
          Token.getRange("IsSpace", false)
        else Token.token_not_spaces
      case _ =>
        throw new RuntimeException("Internal Error: shorthands: \\u" + java.lang.Integer.toString(ch, 16))
    }
    tok
  }

  @throws[ParseException]
  def decodeEscaped(): Int = {
    if (this.read() != T_BACKSOLIDUS) throw ex("parser.next.1", this.offset - 1)
    var c = this.chardata
    c match {
      case 'e' => c = 0x1b
      case 'f' => c = '\f'.toInt
      case 'n' => c = '\n'.toInt
      case 'r' => c = '\r'.toInt
      case 't' => c = '\t'.toInt
      case 'x' =>
        this.next()
        if (this.read() != T_CHAR) throw ex("parser.descape.1", this.offset - 1)
        if (this.chardata == '{') {
          var uv = 0
          var breakLoop = false
          while (!breakLoop) {
            this.next()
            if (this.read() != T_CHAR) throw ex("parser.descape.1", this.offset - 1)
            val v1 = hexChar(this.chardata)
            if (v1 < 0) {
              breakLoop = true
            } else {
              if (uv > uv * 16) throw ex("parser.descape.2", this.offset - 1)
              uv = uv * 16 + v1
            }
          }
          if (this.chardata != '}') throw ex("parser.descape.3", this.offset - 1)
          if (uv > Token.UTF16_MAX) throw ex("parser.descape.4", this.offset - 1)
          c = uv
        } else {
          var v1 = hexChar(this.chardata)
          if (this.read() != T_CHAR || v1 < 0)
            throw ex("parser.descape.1", this.offset - 1)
          var uv = v1
          this.next()
          v1 = hexChar(this.chardata)
          if (this.read() != T_CHAR || v1 < 0)
            throw ex("parser.descape.1", this.offset - 1)
          uv = uv * 16 + v1
          c = uv
        }

      case 'u' =>
        var v1 = hexChar(this.chardata)
        this.next()
        v1 = hexChar(this.chardata)
        if (this.read() != T_CHAR || v1 < 0) throw ex("parser.descape.1", this.offset - 1)
        var uv = v1
        this.next()
        v1 = hexChar(this.chardata)
        if (this.read() != T_CHAR || v1 < 0) throw ex("parser.descape.1", this.offset - 1)
        uv = uv * 16 + v1
        this.next()
        v1 = hexChar(this.chardata)
        if (this.read() != T_CHAR || v1 < 0) throw ex("parser.descape.1", this.offset - 1)
        uv = uv * 16 + v1
        this.next()
        v1 = hexChar(this.chardata)
        if (this.read() != T_CHAR || v1 < 0) throw ex("parser.descape.1", this.offset - 1)
        uv = uv * 16 + v1
        c = uv

      case 'v' =>
        this.next()
        var v1 = hexChar(this.chardata)
        if (this.read() != T_CHAR || v1 < 0) throw ex("parser.descape.1", this.offset - 1)
        var uv = v1
        var i = 0
        while (i < 5) {
          this.next()
          v1 = hexChar(this.chardata)
          if (this.read() != T_CHAR || v1 < 0) throw ex("parser.descape.1", this.offset - 1)
          uv = uv * 16 + v1
          i += 1
        }
        if (uv > Token.UTF16_MAX) throw ex("parser.descappe.4", this.offset - 1)
        c = uv

      case 'A' | 'Z' | 'z' =>
        throw ex("parser.descape.5", this.offset - 2)

      case _ =>
    }
    c
  }
}

object RegexParser {
  final val T_CHAR: Int = 0
  final val T_EOF: Int = 1
  final val T_OR: Int = 2
  final val T_STAR: Int = 3
  final val T_PLUS: Int = 4
  final val T_QUESTION: Int = 5
  final val T_LPAREN: Int = 6
  final val T_RPAREN: Int = 7
  final val T_DOT: Int = 8
  final val T_LBRACKET: Int = 9
  final val T_BACKSOLIDUS: Int = 10
  final val T_CARET: Int = 11
  final val T_DOLLAR: Int = 12
  final val T_LPAREN2: Int = 13
  final val T_LOOKAHEAD: Int = 14
  final val T_NEGATIVELOOKAHEAD: Int = 15
  final val T_LOOKBEHIND: Int = 16
  final val T_NEGATIVELOOKBEHIND: Int = 17
  final val T_INDEPENDENT: Int = 18
  final val T_SET_OPERATIONS: Int = 19
  final val T_POSIX_CHARCLASS_START: Int = 20
  final val T_COMMENT: Int = 21
  final val T_MODIFIERS: Int = 22
  final val T_CONDITION: Int = 23
  final val T_XMLSCHEMA_CC_SUBTRACTION: Int = 24

  class ReferencePosition(val refNumber: Int, val position: Int)

  protected[regexp] final val S_NORMAL: Int = 0
  protected[regexp] final val S_INBRACKETS: Int = 1
  protected[regexp] final val S_INXBRACKETS: Int = 2

  private def hexChar(ch: Int): Int = {
    if (ch < '0') -1
    else if (ch > 'f') -1
    else if (ch <= '9') ch - '0'
    else if (ch < 'A') -1
    else if (ch <= 'F') ch - 'A' + 10
    else if (ch < 'a') -1
    else ch - 'a' + 10
  }

  val DefaultMessages: Map[String, String] = Map(
    "parser.parse.1" -> "Wrong character.",
    "parser.parse.2" -> "Invalid reference number.",
    "parser.next.1" -> "A character is required after \\.",
    "parser.next.2" -> "'?' is not expected.  '(?:' or '(?=' or '(?!' or '(?<' or '(?#' or '(?>'?",
    "parser.next.3" -> "'(?<=' or '(?<!' is expected.",
    "parser.next.4" -> "A comment is not terminated.",
    "parser.factor.1" -> "')' is expected.",
    "parser.factor.2" -> "Unexpected end of the pattern in a modifier group.",
    "parser.factor.3" -> "':' is expected.",
    "parser.factor.4" -> "Unexpected end of the pattern in a conditional group.",
    "parser.factor.5" -> "A back reference or an anchor or a lookahead or a lookbehind is expected in a conditional pattern.",
    "parser.factor.6" -> "There are more than three choices in a conditional group.",
    "parser.atom.1" -> "A character in U+0040-U+005f must follow \\c.",
    "parser.atom.2" -> "A '{' is required before a character category.",
    "parser.atom.3" -> "A property name is not closed by '}'.",
    "parser.atom.4" -> "Unexpected meta character.",
    "parser.atom.5" -> "Unknown property.",
    "parser.cc.1" -> "A POSIX character class must be closed by ':]'.",
    "parser.cc.2" -> "Unexpected end of the pattern in a character class.",
    "parser.cc.3" -> "Unknown name for a POSIX character class.",
    "parser.cc.4" -> "'-' is invalid here.",
    "parser.cc.5" -> "']' is expected.",
    "parser.cc.6" -> "'[' is invalid in a character class.  Write '\\['.",
    "parser.cc.7" -> "']' is invalid in a character class.  Write '\\]'.",
    "parser.cc.8" -> "'-' is an invalid character range. Write '\\-'.",
    "parser.ope.1" -> "'[' is expected.",
    "parser.ope.2" -> "')' or '-[' or '+[' or '&[' is expected.",
    "parser.ope.3" -> "The range end code point is less than the start code point.",
    "parser.descape.1" -> "Invalid Unicode hex notation.",
    "parser.descape.2" -> "Overflow in a hex notation.",
    "parser.descape.3" -> "'\\x{' must be closed by '}'.",
    "parser.descape.4" -> "Invalid Unicode code point.",
    "parser.descape.5" -> "An anchor must not be here.",
    "parser.process.1" -> "This expression is not supported in the current option setting.",
    "parser.quantifier.1" -> "Invalid quantifier. A digit is expected.",
    "parser.quantifier.2" -> "Invalid quantifier. Invalid quantity or a '}' is missing.",
    "parser.quantifier.3" -> "Invalid quantifier. A digit or '}' is expected.",
    "parser.quantifier.4" -> "Invalid quantifier. A min quantity must be <= a max quantity.",
    "parser.quantifier.5" -> "Invalid quantifier. A quantity value overflow."
  )
}
