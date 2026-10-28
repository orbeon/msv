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

import java.util.ArrayList

/**
 * @version Op.java,v 1.3 2002/08/09 15:18:17 neilg Exp
 */
class Op protected (val opType: Int) {
  var next: Op = null

  def size(): Int = 0 // for UNION

  def elementAt(index: Int): Op =
    throw new RuntimeException("Internal Error: type=" + this.opType)

  def getChild(): Op =
    throw new RuntimeException("Internal Error: type=" + this.opType)

  def getData(): Int =
    throw new RuntimeException("Internal Error: type=" + this.opType)

  def getData2(): Int =
    throw new RuntimeException("Internal Error: type=" + this.opType)

  def getToken(): RangeToken =
    throw new RuntimeException("Internal Error: type=" + this.opType)

  def getString(): String =
    throw new RuntimeException("Internal Error: type=" + this.opType)
}

object Op {
  final val DOT: Int = 0
  final val CHAR: Int = 1 // Single character
  final val RANGE: Int = 3 // [a-zA-Z]
  final val NRANGE: Int = 4 // [^a-zA-Z]
  final val ANCHOR: Int = 5 // ^ $ ...
  final val STRING: Int = 6 // literal String
  final val CLOSURE: Int = 7 // X*
  final val NONGREEDYCLOSURE: Int = 8 // X*?
  final val QUESTION: Int = 9 // X?
  final val NONGREEDYQUESTION: Int = 10 // X??
  final val UNION: Int = 11 // X|Y
  final val CAPTURE: Int = 15 // ( and )
  final val BACKREFERENCE: Int = 16 // \1 \2 ...
  final val LOOKAHEAD: Int = 20 // (?=...)
  final val NEGATIVELOOKAHEAD: Int = 21 // (?!...)
  final val LOOKBEHIND: Int = 22 // (?<=...)
  final val NEGATIVELOOKBEHIND: Int = 23 // (?<!...)
  final val INDEPENDENT: Int = 24 // (?>...)
  final val MODIFIER: Int = 25 // (?ims-ims:...)
  final val CONDITION: Int = 26 // (?(..)yes|no)

  var nofinstances: Int = 0
  final val COUNT: Boolean = false

  def createDot(): Op = {
    if (COUNT) nofinstances += 1
    new Op(DOT)
  }

  def createChar(data: Int): CharOp = {
    if (COUNT) nofinstances += 1
    new CharOp(CHAR, data)
  }

  def createAnchor(data: Int): CharOp = {
    if (COUNT) nofinstances += 1
    new CharOp(ANCHOR, data)
  }

  def createCapture(number: Int, next: Op): CharOp = {
    if (COUNT) nofinstances += 1
    val op = new CharOp(CAPTURE, number)
    op.next = next
    op
  }

  def createUnion(size: Int): UnionOp = {
    if (COUNT) nofinstances += 1
    new UnionOp(UNION, size)
  }

  def createClosure(id: Int): ChildOp = {
    if (COUNT) nofinstances += 1
    new ModifierOp(CLOSURE, id, -1)
  }

  def createNonGreedyClosure(): ChildOp = {
    if (COUNT) nofinstances += 1
    new ChildOp(NONGREEDYCLOSURE)
  }

  def createQuestion(nongreedy: Boolean): ChildOp = {
    if (COUNT) nofinstances += 1
    new ChildOp(if (nongreedy) NONGREEDYQUESTION else QUESTION)
  }

  def createRange(tok: Token): RangeOp = {
    if (COUNT) nofinstances += 1
    new RangeOp(RANGE, tok)
  }

  def createLook(opType: Int, next: Op, branch: Op): ChildOp = {
    if (COUNT) nofinstances += 1
    val op = new ChildOp(opType)
    op.setChild(branch)
    op.next = next
    op
  }

  def createBackReference(refno: Int): CharOp = {
    if (COUNT) nofinstances += 1
    new CharOp(BACKREFERENCE, refno)
  }

  def createString(literal: String): StringOp = {
    if (COUNT) nofinstances += 1
    new StringOp(STRING, literal)
  }

  def createIndependent(next: Op, branch: Op): ChildOp = {
    if (COUNT) nofinstances += 1
    val op = new ChildOp(INDEPENDENT)
    op.setChild(branch)
    op.next = next
    op
  }

  def createModifier(next: Op, branch: Op, add: Int, mask: Int): ModifierOp = {
    if (COUNT) nofinstances += 1
    val op = new ModifierOp(MODIFIER, add, mask)
    op.setChild(branch)
    op.next = next
    op
  }

  def createCondition(next: Op, ref: Int, conditionflow: Op, yesflow: Op, noflow: Op): ConditionOp = {
    if (COUNT) nofinstances += 1
    val op = new ConditionOp(CONDITION, ref, conditionflow, yesflow, noflow)
    op.next = next
    op
  }

  // ================================================================
  class CharOp(opType: Int, val charData: Int) extends Op(opType) {
    override def getData(): Int = this.charData
  }

  // ================================================================
  class UnionOp(opType: Int, initialSize: Int) extends Op(opType) {
    val branches: ArrayList[Op] = new ArrayList[Op](initialSize)

    def addElement(op: Op): Unit = {
      this.branches.add(op)
    }

    override def size(): Int = {
      this.branches.size()
    }

    override def elementAt(index: Int): Op = {
      this.branches.get(index)
    }
  }

  // ================================================================
  class ChildOp(opType: Int) extends Op(opType) {
    var child: Op = null

    def setChild(child: Op): Unit = {
      this.child = child
    }

    override def getChild(): Op = {
      this.child
    }
  }

  // ================================================================
  class ModifierOp(opType: Int, val v1: Int, val v2: Int) extends ChildOp(opType) {
    override def getData(): Int = this.v1
    override def getData2(): Int = this.v2
  }

  // ================================================================
  class RangeOp(opType: Int, val tok: Token) extends Op(opType) {
    override def getToken(): RangeToken = this.tok.asInstanceOf[RangeToken]
  }

  // ================================================================
  class StringOp(opType: Int, val string: String) extends Op(opType) {
    override def getString(): String = this.string
  }

  // ================================================================
  class ConditionOp(opType: Int, val refNumber: Int, val condition: Op, val yes: Op, val no: Op) extends Op(opType)
}
