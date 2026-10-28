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
 * A regular expression matching engine using Non-deterministic Finite Automaton (NFA).
 *
 * @author TAMURA Kent &lt;kent@trl.ibm.co.jp&gt;
 * @version RegularExpression.java,v 1.7 2003/07/15 12:28:25 neeraj Exp
 */
class RegularExpression extends java.io.Serializable {
  import RegularExpression.*

  var regex: String = null
  var options: Int = 0
  var nofparen: Int = 0
  var tokentree: Token = null
  var hasBackReferences: Boolean = false

  @transient var minlength: Int = 0
  @transient var operations: Op = null
  @transient var numberOfClosures: Int = 0
  @transient var context: Context = null
  @transient var firstChar: RangeToken = null

  @transient var fixedString: String = null
  @transient var fixedStringOptions: Int = 0
  @transient var fixedStringTable: BMPattern = null
  @transient var fixedStringOnly: Boolean = false

  @throws[ParseException]
  def this(regex: String) = {
    this()
    this.setPattern(regex, null: String)
  }

  @throws[ParseException]
  def this(regex: String, options: String) = {
    this()
    this.setPattern(regex, options)
  }

  def this(regex: String, tok: Token, parens: Int, hasBackReferences: Boolean, options: Int) = {
    this()
    this.regex = regex
    this.tokentree = tok
    this.nofparen = parens
    this.options = options
    this.hasBackReferences = hasBackReferences
  }

  @throws[ParseException]
  def setPattern(newPattern: String): Unit = {
    this.setPattern(newPattern, this.options)
  }

  @throws[ParseException]
  private def setPattern(newPattern: String, options: Int): Unit = {
    this.regex = newPattern
    this.options = options
    val rp = if (RegularExpression.isSet(this.options, RegularExpression.XMLSCHEMA_MODE))
      new ParserForXMLSchema()
    else
      new RegexParser()
    this.tokentree = rp.parse(this.regex, this.options)
    this.nofparen = rp.parennumber
    this.hasBackReferences = rp.hasBackReferences

    this.operations = null
    this.context = null
  }

  @throws[ParseException]
  def setPattern(newPattern: String, options: String): Unit = {
    this.setPattern(newPattern, REUtil.parseOptions(options))
  }

  def getPattern: String = this.regex

  override def toString: String = this.tokentree.toString(this.options)

  def getOptions: String = REUtil.createOptionString(this.options)

  override def equals(obj: Any): Boolean = {
    obj match {
      case r: RegularExpression =>
        this.regex == r.regex && this.options == r.options
      case _ => false
    }
  }

  def equals(pattern: String, options: Int): Boolean = {
    this.regex == pattern && this.options == options
  }

  override def hashCode(): Int = {
    (this.regex + "/" + this.getOptions).hashCode
  }

  def getNumberOfGroups: Int = this.nofparen

  // ================================================================
  // Compilation
  // ================================================================

  private def compile(tok: Token): Unit = synchronized {
    if (this.operations != null) return
    this.numberOfClosures = 0
    this.operations = this.compile(tok, null, false)
  }

  private def compile(tok: Token, next: Op, reverse: Boolean): Op = {
    var ret: Op = null
    tok.tokenType match {
      case Token.DOT =>
        ret = Op.createDot()
        ret.next = next

      case Token.CHAR =>
        ret = Op.createChar(tok.getChar)
        ret.next = next

      case Token.ANCHOR =>
        ret = Op.createAnchor(tok.getChar)
        ret.next = next

      case Token.RANGE | Token.NRANGE =>
        ret = Op.createRange(tok)
        ret.next = next

      case Token.CONCAT =>
        ret = next
        if (!reverse) {
          var i = tok.size() - 1
          while (i >= 0) {
            ret = compile(tok.getChild(i), ret, false)
            i -= 1
          }
        } else {
          var i = 0
          while (i < tok.size()) {
            ret = compile(tok.getChild(i), ret, true)
            i += 1
          }
        }

      case Token.UNION =>
        val uni = Op.createUnion(tok.size())
        var i = 0
        while (i < tok.size()) {
          uni.addElement(compile(tok.getChild(i), next, reverse))
          i += 1
        }
        ret = uni

      case Token.CLOSURE | Token.NONGREEDYCLOSURE =>
        val child = tok.getChild(0)
        val min = tok.getMin
        var max = tok.getMax
        if (min >= 0 && min == max) {
          ret = next
          var i = 0
          while (i < min) {
            ret = compile(child, ret, reverse)
            i += 1
          }
        } else {
          if (min > 0 && max > 0) max -= min
          if (max > 0) {
            ret = next
            var i = 0
            while (i < max) {
              val q = Op.createQuestion(tok.tokenType == Token.NONGREEDYCLOSURE)
              q.next = next
              q.setChild(compile(child, ret, reverse))
              ret = q
              i += 1
            }
          } else {
            val op = if (tok.tokenType == Token.NONGREEDYCLOSURE) {
              Op.createNonGreedyClosure()
            } else {
              if (child.getMinLength == 0)
                Op.createClosure({ val n = this.numberOfClosures; this.numberOfClosures += 1; n })
              else
                Op.createClosure(-1)
            }
            op.next = next
            op.setChild(compile(child, op, reverse))
            ret = op
          }
          if (min > 0) {
            var i = 0
            while (i < min) {
              ret = compile(child, ret, reverse)
              i += 1
            }
          }
        }

      case Token.EMPTY =>
        ret = next

      case Token.STRING =>
        ret = Op.createString(tok.getString)
        ret.next = next

      case Token.BACKREFERENCE =>
        ret = Op.createBackReference(tok.getReferenceNumber)
        ret.next = next

      case Token.PAREN =>
        if (tok.getParenNumber == 0) {
          ret = compile(tok.getChild(0), next, reverse)
        } else if (reverse) {
          var n: Op = Op.createCapture(tok.getParenNumber, next)
          n = compile(tok.getChild(0), n, reverse)
          ret = Op.createCapture(-tok.getParenNumber, n)
        } else {
          var n: Op = Op.createCapture(-tok.getParenNumber, next)
          n = compile(tok.getChild(0), n, reverse)
          ret = Op.createCapture(tok.getParenNumber, n)
        }

      case Token.LOOKAHEAD =>
        ret = Op.createLook(Op.LOOKAHEAD, next, compile(tok.getChild(0), null, false))

      case Token.NEGATIVELOOKAHEAD =>
        ret = Op.createLook(Op.NEGATIVELOOKAHEAD, next, compile(tok.getChild(0), null, false))

      case Token.LOOKBEHIND =>
        ret = Op.createLook(Op.LOOKBEHIND, next, compile(tok.getChild(0), null, true))

      case Token.NEGATIVELOOKBEHIND =>
        ret = Op.createLook(Op.NEGATIVELOOKBEHIND, next, compile(tok.getChild(0), null, true))

      case Token.INDEPENDENT =>
        ret = Op.createIndependent(next, compile(tok.getChild(0), null, reverse))

      case Token.MODIFIERGROUP =>
        ret = Op.createModifier(next, compile(tok.getChild(0), null, reverse),
          tok.asInstanceOf[Token.ModifierToken].getOptions,
          tok.asInstanceOf[Token.ModifierToken].getOptionsMask)

      case Token.CONDITION =>
        val ctok = tok.asInstanceOf[Token.ConditionToken]
        val ref = ctok.refNumber
        val condition = if (ctok.condition == null) null else compile(ctok.condition, null, reverse)
        val yes = compile(ctok.yes, next, reverse)
        val no = if (ctok.no == null) null else compile(ctok.no, next, reverse)
        ret = Op.createCondition(next, ref, condition, yes, no)

      case _ =>
        throw new RuntimeException("Unknown token type: " + tok.tokenType)
    }
    ret
  }

  def prepare(): Unit = {
    if (Op.COUNT) Op.nofinstances = 0
    this.compile(this.tokentree)
    this.minlength = this.tokentree.getMinLength
    this.firstChar = null

    if (!isSet(this.options, PROHIBIT_HEAD_CHARACTER_OPTIMIZATION)
      && !isSet(this.options, XMLSCHEMA_MODE)) {
      val fc = Token.createRange()
      val fresult = this.tokentree.analyzeFirstCharacter(fc, this.options)
      if (fresult == Token.FC_TERMINAL) {
        fc.compactRanges()
        this.firstChar = fc
      }
    }

    if (this.operations != null
      && (this.operations.opType == Op.STRING || this.operations.opType == Op.CHAR)
      && this.operations.next == null) {
      this.fixedStringOnly = true
      if (this.operations.opType == Op.STRING) {
        this.fixedString = this.operations.getString()
      } else if (this.operations.getData() >= 0x10000) {
        this.fixedString = REUtil.decomposeToSurrogates(this.operations.getData())
      } else {
        this.fixedString = this.operations.getData().toChar.toString
      }
      this.fixedStringOptions = this.options
      this.fixedStringTable = new BMPattern(this.fixedString, 256, isSet(this.fixedStringOptions, IGNORE_CASE))
    } else if (!isSet(this.options, PROHIBIT_FIXED_STRING_OPTIMIZATION)
      && !isSet(this.options, XMLSCHEMA_MODE)) {
      val container = new Token.FixedStringContainer()
      this.tokentree.findFixedString(container, this.options)
      this.fixedString = if (container.token == null) null else container.token.getString
      this.fixedStringOptions = container.options
      if (this.fixedString != null && this.fixedString.length < 2)
        this.fixedString = null
      if (this.fixedString != null) {
        this.fixedStringTable = new BMPattern(this.fixedString, 256, isSet(this.fixedStringOptions, IGNORE_CASE))
      }
    }
  }

  // ================================================================
  // Public matches methods
  // ================================================================

  def matches(target: Array[Char]): Boolean = {
    this.matches(target, 0, target.length, null)
  }

  def matches(target: Array[Char], start: Int, end: Int): Boolean = {
    this.matches(target, start, end, null)
  }

  def matches(target: Array[Char], matchResult: Match): Boolean = {
    this.matches(target, 0, target.length, matchResult)
  }

  def matches(target: Array[Char], start: Int, end: Int, matchResult: Match): Boolean = {
    val src = new CharArraySource(target)
    this.matchesInternal(src, start, end, matchResult, charsTarget = target, strTarget = null)
  }

  def matches(target: String): Boolean = {
    this.matches(target, 0, target.length, null)
  }

  def matches(target: String, start: Int, end: Int): Boolean = {
    this.matches(target, start, end, null)
  }

  def matches(target: String, matchResult: Match): Boolean = {
    this.matches(target, 0, target.length, matchResult)
  }

  def matches(target: String, start: Int, end: Int, matchResult: Match): Boolean = {
    val src = new StringSource(target)
    this.matchesInternal(src, start, end, matchResult, charsTarget = null, strTarget = target)
  }

  // ================================================================
  // Internal matcher
  // ================================================================

  private def matchesInternal(src: CharSource, start: Int, end: Int, matchResult0: Match,
                              charsTarget: Array[Char], strTarget: String): Boolean = {
    synchronized {
      if (this.operations == null) this.prepare()
      if (this.context == null) this.context = new Context()
    }
    var con: Context = null
    this.context.synchronized {
      con = if (this.context.inuse) new Context() else this.context
      con.reset(start, end, this.numberOfClosures)
    }

    var m = matchResult0
    if (m != null) {
      m.setNumberOfGroups(this.nofparen)
      if (charsTarget != null) m.setSource(charsTarget)
      else if (strTarget != null) m.setSource(strTarget)
    } else if (this.hasBackReferences) {
      m = new Match()
      m.setNumberOfGroups(this.nofparen)
    }
    con.matchResult = m

    if (RegularExpression.isSet(this.options, XMLSCHEMA_MODE)) {
      val matchEnd = this.matchSource(con, this.operations, con.start, 1, this.options, src)
      if (matchEnd == con.limit) {
        if (con.matchResult != null) {
          con.matchResult.setBeginning(0, con.start)
          con.matchResult.setEnd(0, matchEnd)
        }
        con.inuse = false
        return true
      }
      con.inuse = false
      return false
    }

    if (this.fixedStringOnly) {
      val o = if (charsTarget != null) this.fixedStringTable.matches(charsTarget, con.start, con.limit)
      else this.fixedStringTable.matches(strTarget, con.start, con.limit)
      if (o >= 0) {
        if (con.matchResult != null) {
          con.matchResult.setBeginning(0, o)
          con.matchResult.setEnd(0, o + this.fixedString.length)
        }
        con.inuse = false
        return true
      }
      con.inuse = false
      return false
    }

    if (this.fixedString != null) {
      val o = if (charsTarget != null) this.fixedStringTable.matches(charsTarget, con.start, con.limit)
      else this.fixedStringTable.matches(strTarget, con.start, con.limit)
      if (o < 0) {
        con.inuse = false
        return false
      }
    }

    val limit = con.limit - this.minlength
    var matchStart = con.start
    var matchEnd = -1

    if (this.operations != null
      && this.operations.opType == Op.CLOSURE && this.operations.getChild().opType == Op.DOT) {
      if (isSet(this.options, SINGLE_LINE)) {
        matchStart = con.start
        matchEnd = this.matchSource(con, this.operations, con.start, 1, this.options, src)
      } else {
        var previousIsEOL = true
        matchStart = con.start
        var breakLoop = false
        while (matchStart <= limit && !breakLoop) {
          val ch = src.charAt(matchStart).toInt
          if (isEOLChar(ch)) {
            previousIsEOL = true
          } else {
            if (previousIsEOL) {
              matchEnd = this.matchSource(con, this.operations, matchStart, 1, this.options, src)
              if (matchEnd >= 0) breakLoop = true
            }
            previousIsEOL = false
          }
          if (!breakLoop) matchStart += 1
        }
      }
    } else if (this.firstChar != null) {
      var range = this.firstChar
      if (RegularExpression.isSet(this.options, IGNORE_CASE)) {
        range = this.firstChar.getCaseInsensitiveToken
        matchStart = con.start
        var breakLoop = false
        while (matchStart <= limit && !breakLoop) {
          var ch = src.charAt(matchStart).toInt
          if (REUtil.isHighSurrogate(ch) && matchStart + 1 < con.limit) {
            ch = REUtil.composeFromSurrogates(ch, src.charAt(matchStart + 1).toInt)
            if (range.`match`(ch)) {
              matchEnd = this.matchSource(con, this.operations, matchStart, 1, this.options, src)
              if (matchEnd >= 0) breakLoop = true
            }
          } else {
            if (range.`match`(ch)) {
              matchEnd = this.matchSource(con, this.operations, matchStart, 1, this.options, src)
              if (matchEnd >= 0) breakLoop = true
            } else {
              val ch1 = Character.toUpperCase(ch.toChar).toInt
              if (range.`match`(ch1) || range.`match`(Character.toLowerCase(ch1.toChar).toInt)) {
                matchEnd = this.matchSource(con, this.operations, matchStart, 1, this.options, src)
                if (matchEnd >= 0) breakLoop = true
              }
            }
          }
          if (!breakLoop) matchStart += 1
        }
      } else {
        matchStart = con.start
        var breakLoop = false
        while (matchStart <= limit && !breakLoop) {
          var ch = src.charAt(matchStart).toInt
          if (REUtil.isHighSurrogate(ch) && matchStart + 1 < con.limit)
            ch = REUtil.composeFromSurrogates(ch, src.charAt(matchStart + 1).toInt)
          if (range.`match`(ch)) {
            matchEnd = this.matchSource(con, this.operations, matchStart, 1, this.options, src)
            if (matchEnd >= 0) breakLoop = true
          }
          if (!breakLoop) matchStart += 1
        }
      }
    } else {
      matchStart = con.start
      var breakLoop = false
      while (matchStart <= limit && !breakLoop) {
        matchEnd = this.matchSource(con, this.operations, matchStart, 1, this.options, src)
        if (matchEnd >= 0) breakLoop = true
        else matchStart += 1
      }
    }

    if (matchEnd >= 0) {
      if (con.matchResult != null) {
        con.matchResult.setBeginning(0, matchStart)
        con.matchResult.setEnd(0, matchEnd)
      }
      con.inuse = false
      true
    } else {
      con.inuse = false
      false
    }
  }

  // ================================================================
  // matchSource (Core interpreter)
  // ================================================================

  private def matchSource(con: Context, op0: Op, offset0: Int, dx0: Int, opts0: Int, target: CharSource): Int = {
    var op = op0
    var offset = offset0
    var dx = dx0
    var opts = opts0

    while (true) {
      if (op == null)
        return if (isSet(opts, XMLSCHEMA_MODE) && offset != con.limit) -1 else offset
      if (offset > con.limit || offset < con.start)
        return -1

      op.opType match {
        case Op.CHAR =>
          val ch = op.getData()
          if (isSet(opts, IGNORE_CASE)) {
            if (dx > 0) {
              if (offset >= con.limit || !matchIgnoreCase(ch, target.charAt(offset).toInt))
                return -1
              offset += 1
            } else {
              val o1 = offset - 1
              if (o1 >= con.limit || o1 < 0 || !matchIgnoreCase(ch, target.charAt(o1).toInt))
                return -1
              offset = o1
            }
          } else {
            if (dx > 0) {
              if (offset >= con.limit || ch != target.charAt(offset).toInt)
                return -1
              offset += 1
            } else {
              val o1 = offset - 1
              if (o1 >= con.limit || o1 < 0 || ch != target.charAt(o1).toInt)
                return -1
              offset = o1
            }
          }
          op = op.next

        case Op.DOT =>
          if (dx > 0) {
            if (offset >= con.limit) return -1
            var ch = target.charAt(offset).toInt
            if (isSet(opts, SINGLE_LINE)) {
              if (REUtil.isHighSurrogate(ch) && offset + 1 < con.limit)
                offset += 1
            } else {
              if (REUtil.isHighSurrogate(ch) && offset + 1 < con.limit) {
                offset += 1
                ch = REUtil.composeFromSurrogates(ch, target.charAt(offset).toInt)
              }
              if (isEOLChar(ch)) return -1
            }
            offset += 1
          } else {
            var o1 = offset - 1
            if (o1 >= con.limit || o1 < 0) return -1
            var ch = target.charAt(o1).toInt
            if (isSet(opts, SINGLE_LINE)) {
              if (REUtil.isLowSurrogate(ch) && o1 - 1 >= 0)
                o1 -= 1
            } else {
              if (REUtil.isLowSurrogate(ch) && o1 - 1 >= 0) {
                o1 -= 1
                ch = REUtil.composeFromSurrogates(target.charAt(o1).toInt, ch)
              }
              if (!isEOLChar(ch)) return -1
            }
            offset = o1
          }
          op = op.next

        case Op.RANGE | Op.NRANGE =>
          if (dx > 0) {
            if (offset >= con.limit) return -1
            var ch = target.charAt(offset).toInt
            if (REUtil.isHighSurrogate(ch) && offset + 1 < con.limit) {
              offset += 1
              ch = REUtil.composeFromSurrogates(ch, target.charAt(offset).toInt)
            }
            var tok = op.getToken()
            if (isSet(opts, IGNORE_CASE)) {
              tok = tok.getCaseInsensitiveToken
              if (!tok.`match`(ch)) {
                if (ch >= 0x10000) return -1
                val uch = Character.toUpperCase(ch.toChar).toInt
                if (!tok.`match`(uch) && !tok.`match`(Character.toLowerCase(uch.toChar).toInt))
                  return -1
              }
            } else {
              if (!tok.`match`(ch)) return -1
            }
            offset += 1
          } else {
            var o1 = offset - 1
            if (o1 >= con.limit || o1 < 0) return -1
            var ch = target.charAt(o1).toInt
            if (REUtil.isLowSurrogate(ch) && o1 - 1 >= 0) {
              o1 -= 1
              ch = REUtil.composeFromSurrogates(target.charAt(o1).toInt, ch)
            }
            var tok = op.getToken()
            if (isSet(opts, IGNORE_CASE)) {
              tok = tok.getCaseInsensitiveToken
              if (!tok.`match`(ch)) {
                if (ch >= 0x10000) return -1
                val uch = Character.toUpperCase(ch.toChar).toInt
                if (!tok.`match`(uch) && !tok.`match`(Character.toLowerCase(uch.toChar).toInt))
                  return -1
              }
            } else {
              if (!tok.`match`(ch)) return -1
            }
            offset = o1
          }
          op = op.next

        case Op.ANCHOR =>
          var go = false
          op.getData() match {
            case '^' =>
              if (isSet(opts, MULTIPLE_LINES)) {
                if (!(offset == con.start || (offset > con.start && isEOLChar(target.charAt(offset - 1).toInt))))
                  return -1
              } else {
                if (offset != con.start) return -1
              }

            case '@' =>
              if (!(offset == con.start || (offset > con.start && isEOLChar(target.charAt(offset - 1).toInt))))
                return -1

            case '$' =>
              if (isSet(opts, MULTIPLE_LINES)) {
                if (!(offset == con.limit || (offset < con.limit && isEOLChar(target.charAt(offset).toInt))))
                  return -1
              } else {
                if (!(offset == con.limit
                  || (offset + 1 == con.limit && isEOLChar(target.charAt(offset).toInt))
                  || (offset + 2 == con.limit && target.charAt(offset) == CARRIAGE_RETURN && target.charAt(offset + 1) == LINE_FEED)))
                  return -1
              }

            case 'A' =>
              if (offset != con.start) return -1

            case 'Z' =>
              if (!(offset == con.limit
                || (offset + 1 == con.limit && isEOLChar(target.charAt(offset).toInt))
                || (offset + 2 == con.limit && target.charAt(offset) == CARRIAGE_RETURN && target.charAt(offset + 1) == LINE_FEED)))
                return -1

            case 'z' =>
              if (offset != con.limit) return -1

            case 'b' =>
              if (con.length == 0) return -1
              val after = getWordType(target, con.start, con.limit, offset, opts)
              if (after == WT_IGNORE) return -1
              val before = getPreviousWordType(target, con.start, con.limit, offset, opts)
              if (after == before) return -1

            case 'B' =>
              if (con.length == 0) {
                go = true
              } else {
                val after = getWordType(target, con.start, con.limit, offset, opts)
                go = after == WT_IGNORE || after == getPreviousWordType(target, con.start, con.limit, offset, opts)
              }
              if (!go) return -1

            case '<' =>
              if (con.length == 0 || offset == con.limit) return -1
              if (getWordType(target, con.start, con.limit, offset, opts) != WT_LETTER
                || getPreviousWordType(target, con.start, con.limit, offset, opts) != WT_OTHER)
                return -1

            case '>' =>
              if (con.length == 0 || offset == con.start) return -1
              if (getWordType(target, con.start, con.limit, offset, opts) != WT_OTHER
                || getPreviousWordType(target, con.start, con.limit, offset, opts) != WT_LETTER)
                return -1

            case _ =>
          }
          op = op.next

        case Op.BACKREFERENCE =>
          val refno = op.getData()
          if (refno <= 0 || refno >= this.nofparen)
            throw new RuntimeException("Internal Error: Reference number must be more than zero: " + refno)
          if (con.matchResult.getBeginning(refno) < 0 || con.matchResult.getEnd(refno) < 0)
            return -1
          val o2 = con.matchResult.getBeginning(refno)
          val literallen = con.matchResult.getEnd(refno) - o2
          if (!isSet(opts, IGNORE_CASE)) {
            if (dx > 0) {
              if (!target.regionMatches(offset, con.limit, o2, literallen)) return -1
              offset += literallen
            } else {
              if (!target.regionMatches(offset - literallen, con.limit, o2, literallen)) return -1
              offset -= literallen
            }
          } else {
            if (dx > 0) {
              if (!target.regionMatchesIgnoreCase(offset, con.limit, o2, literallen)) return -1
              offset += literallen
            } else {
              if (!target.regionMatchesIgnoreCase(offset - literallen, con.limit, o2, literallen)) return -1
              offset -= literallen
            }
          }
          op = op.next

        case Op.STRING =>
          val literal = op.getString()
          val literallen = literal.length
          if (!isSet(opts, IGNORE_CASE)) {
            if (dx > 0) {
              if (!target.regionMatches(offset, con.limit, literal, literallen)) return -1
              offset += literallen
            } else {
              if (!target.regionMatches(offset - literallen, con.limit, literal, literallen)) return -1
              offset -= literallen
            }
          } else {
            if (dx > 0) {
              if (!target.regionMatchesIgnoreCase(offset, con.limit, literal, literallen)) return -1
              offset += literallen
            } else {
              if (!target.regionMatchesIgnoreCase(offset - literallen, con.limit, literal, literallen)) return -1
              offset -= literallen
            }
          }
          op = op.next

        case Op.CLOSURE =>
          val id = op.getData()
          if (id >= 0) {
            val previousOffset = con.offsets(id)
            if (previousOffset < 0 || previousOffset != offset) {
              con.offsets(id) = offset
            } else {
              con.offsets(id) = -1
              op = op.next
              // continue
            }
          }
          if (op.opType == Op.CLOSURE) {
            val ret = this.matchSource(con, op.getChild(), offset, dx, opts, target)
            if (id >= 0) con.offsets(id) = -1
            if (ret >= 0) return ret
            op = op.next
          }

        case Op.QUESTION =>
          val ret = this.matchSource(con, op.getChild(), offset, dx, opts, target)
          if (ret >= 0) return ret
          op = op.next

        case Op.NONGREEDYCLOSURE | Op.NONGREEDYQUESTION =>
          val ret = this.matchSource(con, op.next, offset, dx, opts, target)
          if (ret >= 0) return ret
          op = op.getChild()

        case Op.UNION =>
          var i = 0
          while (i < op.size()) {
            val ret = this.matchSource(con, op.elementAt(i), offset, dx, opts, target)
            if (ret >= 0) return ret
            i += 1
          }
          return -1

        case Op.CAPTURE =>
          val refno = op.getData()
          if (con.matchResult != null && refno > 0) {
            val save = con.matchResult.getBeginning(refno)
            con.matchResult.setBeginning(refno, offset)
            val ret = this.matchSource(con, op.next, offset, dx, opts, target)
            if (ret < 0) con.matchResult.setBeginning(refno, save)
            return ret
          } else if (con.matchResult != null && refno < 0) {
            val index = -refno
            val save = con.matchResult.getEnd(index)
            con.matchResult.setEnd(index, offset)
            val ret = this.matchSource(con, op.next, offset, dx, opts, target)
            if (ret < 0) con.matchResult.setEnd(index, save)
            return ret
          }
          op = op.next

        case Op.LOOKAHEAD =>
          if (0 > this.matchSource(con, op.getChild(), offset, 1, opts, target)) return -1
          op = op.next

        case Op.NEGATIVELOOKAHEAD =>
          if (0 <= this.matchSource(con, op.getChild(), offset, 1, opts, target)) return -1
          op = op.next

        case Op.LOOKBEHIND =>
          if (0 > this.matchSource(con, op.getChild(), offset, -1, opts, target)) return -1
          op = op.next

        case Op.NEGATIVELOOKBEHIND =>
          if (0 <= this.matchSource(con, op.getChild(), offset, -1, opts, target)) return -1
          op = op.next

        case Op.INDEPENDENT =>
          val ret = this.matchSource(con, op.getChild(), offset, dx, opts, target)
          if (ret < 0) return ret
          offset = ret
          op = op.next

        case Op.MODIFIER =>
          var localopts = opts
          localopts |= op.getData()
          localopts &= ~op.getData2()
          val ret = this.matchSource(con, op.getChild(), offset, dx, localopts, target)
          if (ret < 0) return ret
          offset = ret
          op = op.next

        case Op.CONDITION =>
          val cop = op.asInstanceOf[Op.ConditionOp]
          var matchp = false
          if (cop.refNumber > 0) {
            if (cop.refNumber >= this.nofparen)
              throw new RuntimeException("Internal Error: Reference number must be more than zero: " + cop.refNumber)
            matchp = con.matchResult.getBeginning(cop.refNumber) >= 0 && con.matchResult.getEnd(cop.refNumber) >= 0
          } else {
            matchp = 0 <= this.matchSource(con, cop.condition, offset, dx, opts, target)
          }

          if (matchp) {
            op = cop.yes
          } else if (cop.no != null) {
            op = cop.no
          } else {
            op = cop.next
          }

        case _ =>
          throw new RuntimeException("Unknown operation type: " + op.opType)
      }
    }
    -1 // unreachable
  }
}

object RegularExpression {
  final val DEBUG: Boolean = false

  final val IGNORE_CASE: Int = 1 << 1 // "i"
  final val SINGLE_LINE: Int = 1 << 2 // "s"
  final val MULTIPLE_LINES: Int = 1 << 3 // "m"
  final val EXTENDED_COMMENT: Int = 1 << 4 // "x"
  final val USE_UNICODE_CATEGORY: Int = 1 << 5 // "u"
  final val UNICODE_WORD_BOUNDARY: Int = 1 << 6 // "w"
  final val PROHIBIT_HEAD_CHARACTER_OPTIMIZATION: Int = 1 << 7 // "H"
  final val PROHIBIT_FIXED_STRING_OPTIMIZATION: Int = 1 << 8 // "F"
  final val XMLSCHEMA_MODE: Int = 1 << 9 // "X"
  final val SPECIAL_COMMA: Int = 1 << 10 // ","

  private def isSet(options: Int, flag: Int): Boolean = {
    (options & flag) == flag
  }

  // ================================================================
  // Context
  // ================================================================

  final class Context {
    var start: Int = 0
    var limit: Int = 0
    var length: Int = 0
    var matchResult: Match = null
    var inuse: Boolean = false
    var offsets: Array[Int] = null

    private def resetCommon(nofclosures: Int): Unit = {
      this.length = this.limit - this.start
      this.inuse = true
      this.matchResult = null
      if (this.offsets == null || this.offsets.length != nofclosures)
        this.offsets = new Array[Int](nofclosures)
      var i = 0
      while (i < nofclosures) {
        this.offsets(i) = -1
        i += 1
      }
    }

    def reset(start: Int, limit: Int, nofclosures: Int): Unit = {
      this.start = start
      this.limit = limit
      this.resetCommon(nofclosures)
    }
  }

  // ================================================================
  // CharSource abstraction
  // ================================================================

  sealed trait CharSource {
    def charAt(index: Int): Char
    def regionMatches(offset: Int, limit: Int, part: String, partlen: Int): Boolean
    def regionMatches(offset: Int, limit: Int, offset2: Int, partlen: Int): Boolean
    def regionMatchesIgnoreCase(offset: Int, limit: Int, part: String, partlen: Int): Boolean
    def regionMatchesIgnoreCase(offset: Int, limit: Int, offset2: Int, partlen: Int): Boolean
  }

  final class StringSource(val target: String) extends CharSource {
    def charAt(index: Int): Char = target.charAt(index)

    def regionMatches(offset: Int, limit: Int, part: String, partlen: Int): Boolean = {
      if (limit - offset < partlen) false
      else target.regionMatches(offset, part, 0, partlen)
    }

    def regionMatches(offset: Int, limit: Int, offset2: Int, partlen: Int): Boolean = {
      if (limit - offset < partlen) false
      else target.regionMatches(offset, target, offset2, partlen)
    }

    def regionMatchesIgnoreCase(offset: Int, limit: Int, part: String, partlen: Int): Boolean = {
      if (limit - offset < partlen) false
      else target.regionMatches(true, offset, part, 0, partlen)
    }

    def regionMatchesIgnoreCase(offset: Int, limit: Int, offset2: Int, partlen: Int): Boolean = {
      if (limit - offset < partlen) false
      else target.regionMatches(true, offset, target, offset2, partlen)
    }
  }

  final class CharArraySource(val target: Array[Char]) extends CharSource {
    def charAt(index: Int): Char = target(index)

    def regionMatches(offset: Int, limit: Int, part: String, partlen: Int): Boolean = {
      if (offset < 0 || limit - offset < partlen) return false
      var i = 0
      while (i < partlen) {
        if (target(offset + i) != part.charAt(i)) return false
        i += 1
      }
      true
    }

    def regionMatches(offset: Int, limit: Int, offset2: Int, partlen: Int): Boolean = {
      if (offset < 0 || limit - offset < partlen) return false
      var i = 0
      while (i < partlen) {
        if (target(offset + i) != target(offset2 + i)) return false
        i += 1
      }
      true
    }

    def regionMatchesIgnoreCase(offset: Int, limit: Int, part: String, partlen: Int): Boolean = {
      if (offset < 0 || limit - offset < partlen) return false
      var i = 0
      while (i < partlen) {
        val ch1 = target(offset + i)
        val ch2 = part.charAt(i)
        if (ch1 != ch2) {
          val uch1 = Character.toUpperCase(ch1)
          val uch2 = Character.toUpperCase(ch2)
          if (uch1 != uch2 && Character.toLowerCase(uch1) != Character.toLowerCase(uch2))
            return false
        }
        i += 1
      }
      true
    }

    def regionMatchesIgnoreCase(offset: Int, limit: Int, offset2: Int, partlen: Int): Boolean = {
      if (offset < 0 || limit - offset < partlen) return false
      var i = 0
      while (i < partlen) {
        val ch1 = target(offset + i)
        val ch2 = target(offset2 + i)
        if (ch1 != ch2) {
          val uch1 = Character.toUpperCase(ch1)
          val uch2 = Character.toUpperCase(ch2)
          if (uch1 != uch2 && Character.toLowerCase(uch1) != Character.toLowerCase(uch2))
            return false
        }
        i += 1
      }
      true
    }
  }

  // ================================================================
  // Word boundary helpers
  // ================================================================

  private final val WT_IGNORE: Int = 0
  private final val WT_LETTER: Int = 1
  private final val WT_OTHER: Int = 2

  private def getPreviousWordType(target: CharSource, begin: Int, end: Int, offset0: Int, opts: Int): Int = {
    var offset = offset0 - 1
    var ret = getWordType(target, begin, end, offset, opts)
    while (ret == WT_IGNORE) {
      offset -= 1
      ret = getWordType(target, begin, end, offset, opts)
    }
    ret
  }

  private def getWordType(target: CharSource, begin: Int, end: Int, offset: Int, opts: Int): Int = {
    if (offset < begin || offset >= end) return WT_OTHER
    getWordType0(target.charAt(offset), opts)
  }

  private def getWordType0(ch: Char, opts: Int): Int = {
    if (!isSet(opts, UNICODE_WORD_BOUNDARY)) {
      if (isSet(opts, USE_UNICODE_CATEGORY)) {
        return if (Token.getRange("IsWord", true).`match`(ch.toInt)) WT_LETTER else WT_OTHER
      }
      return if (isWordChar(ch.toInt)) WT_LETTER else WT_OTHER
    }

    Character.getType(ch) match {
      case Character.UPPERCASE_LETTER | Character.LOWERCASE_LETTER | Character.TITLECASE_LETTER |
           Character.MODIFIER_LETTER | Character.OTHER_LETTER | Character.LETTER_NUMBER |
           Character.DECIMAL_DIGIT_NUMBER | Character.OTHER_NUMBER | Character.COMBINING_SPACING_MARK =>
        WT_LETTER

      case Character.FORMAT | Character.NON_SPACING_MARK | Character.ENCLOSING_MARK =>
        WT_IGNORE

      case Character.CONTROL =>
        ch match {
          case '\t' | '\n' | '\u000B' | '\f' | '\r' => WT_OTHER
          case _ => WT_IGNORE
        }

      case _ =>
        WT_OTHER
    }
  }

  final val LINE_FEED: Int = 0x000A
  final val CARRIAGE_RETURN: Int = 0x000D
  final val LINE_SEPARATOR: Int = 0x2028
  final val PARAGRAPH_SEPARATOR: Int = 0x2029

  private def isEOLChar(ch: Int): Boolean = {
    ch == LINE_FEED || ch == CARRIAGE_RETURN || ch == LINE_SEPARATOR || ch == PARAGRAPH_SEPARATOR
  }

  private def isWordChar(ch: Int): Boolean = {
    if (ch == '_') return true
    if (ch < '0') return false
    if (ch > 'z') return false
    if (ch <= '9') return true
    if (ch < 'A') return false
    if (ch <= 'Z') return true
    if (ch < 'a') return false
    true
  }

  private def matchIgnoreCase(chardata: Int, ch: Int): Boolean = {
    if (chardata == ch) return true
    if (chardata > 0xffff || ch > 0xffff) return false
    val uch1 = Character.toUpperCase(chardata.toChar)
    val uch2 = Character.toUpperCase(ch.toChar)
    if (uch1 == uch2) return true
    Character.toLowerCase(uch1) == Character.toLowerCase(uch2)
  }
}
