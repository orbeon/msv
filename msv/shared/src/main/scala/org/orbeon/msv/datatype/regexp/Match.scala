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
 * An instance of this class has ranges captured in matching.
 *
 * @author TAMURA Kent &lt;kent@trl.ibm.co.jp&gt;
 * @version Match.java,v 1.4 2002/08/09 15:18:17 neilg Exp
 */
final class Match extends Cloneable {
  var beginpos: Array[Int] = null
  var endpos: Array[Int] = null
  var nofgroups: Int = 0

  var strSource: String = null
  var charSource: Array[Char] = null


  override def clone(): AnyRef = {
    val ma = new Match()
    if (this.nofgroups > 0) {
      ma.setNumberOfGroups(this.nofgroups)
      if (this.strSource != null) ma.setSource(this.strSource)
      else if (this.charSource != null) ma.setSource(this.charSource)
      var i = 0
      while (i < this.nofgroups) {
        ma.setBeginning(i, this.getBeginning(i))
        ma.setEnd(i, this.getEnd(i))
        i += 1
      }
    }
    ma
  }

  protected[regexp] def setNumberOfGroups(n: Int): Unit = {
    val oldn = this.nofgroups
    this.nofgroups = n
    if (oldn <= 0 || oldn < n || n * 2 < oldn) {
      this.beginpos = new Array[Int](n)
      this.endpos = new Array[Int](n)
    }
    var i = 0
    while (i < n) {
      this.beginpos(i) = -1
      this.endpos(i) = -1
      i += 1
    }
  }

  protected[regexp] def setSource(str: String): Unit = {
    this.strSource = str
    this.charSource = null
  }

  protected[regexp] def setSource(chars: Array[Char]): Unit = {
    this.strSource = null
    this.charSource = chars
  }

  protected[regexp] def setBeginning(index: Int, v: Int): Unit = {
    this.beginpos(index) = v
  }

  protected[regexp] def setEnd(index: Int, v: Int): Unit = {
    this.endpos(index) = v
  }

  /**
   * Return the number of regular expression groups.
   * This method returns 1 when the regular expression has no capturing-parenthesis.
   */
  def getNumberOfGroups: Int = {
    if (this.nofgroups <= 0)
      throw new IllegalStateException("A result is not set.")
    this.nofgroups
  }

  /**
   * Return a start position in the target text matched to specified regular expression group.
   *
   * @param index Less than `getNumberOfGroups`.
   */
  def getBeginning(index: Int): Int = {
    if (this.beginpos == null)
      throw new IllegalStateException("A result is not set.")
    if (index < 0 || this.nofgroups <= index)
      throw new IllegalArgumentException(s"The parameter must be less than ${this.nofgroups}: $index")
    this.beginpos(index)
  }

  /**
   * Return an end position in the target text matched to specified regular expression group.
   *
   * @param index Less than `getNumberOfGroups`.
   */
  def getEnd(index: Int): Int = {
    if (this.endpos == null)
      throw new IllegalStateException("A result is not set.")
    if (index < 0 || this.nofgroups <= index)
      throw new IllegalArgumentException(s"The parameter must be less than ${this.nofgroups}: $index")
    this.endpos(index)
  }

  /**
   * Return an substring of the target text matched to specified regular expression group.
   *
   * @param index Less than `getNumberOfGroups`.
   */
  def getCapturedText(index: Int): String = {
    if (this.beginpos == null)
      throw new IllegalStateException("match() has never been called.")
    if (index < 0 || this.nofgroups <= index)
      throw new IllegalArgumentException(s"The parameter must be less than ${this.nofgroups}: $index")
    val begin = this.beginpos(index)
    val end = this.endpos(index)
    if (begin < 0 || end < 0) return null
    if (this.strSource != null) {
      this.strSource.substring(begin, end)
    } else {
      new String(this.charSource, begin, end - begin)
    }
  }
}
