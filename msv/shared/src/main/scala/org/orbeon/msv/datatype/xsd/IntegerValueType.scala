/*
 * @(#)$Id: IntegerValueType.java,v 1.16 2003/06/09 20:49:22 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import java.io.Serializable
import java.math.BigInteger

/**
 * Value object of "integer" type.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IntegerValueType private (val value: String) extends Number with Comparable[AnyRef] with Serializable:

  private def this(v: Long) =
    this(java.lang.Long.toString(v))

  override def compareTo(o: AnyRef): Int =
    val rhs: IntegerValueType =
      o match
        case iv: IntegerValueType => iv
        case num: Number          => new IntegerValueType(num.longValue())
        case _                    => throw new ClassCastException()

    val lhsIsNegative = value.charAt(0) == '-'
    val rhsIsNegative = rhs.value.charAt(0) == '-'

    if lhsIsNegative && !rhsIsNegative then return -1
    if rhsIsNegative && !lhsIsNegative then return 1

    var lp = if lhsIsNegative && rhsIsNegative then 1 else 0
    var rp = lp

    var llen = value.length - lp
    val rlen = rhs.value.length - rp

    if llen > rlen then return if lhsIsNegative then -1 else 1
    if llen < rlen then return if lhsIsNegative then 1 else -1

    while llen > 0 do
      val lch = value.charAt(lp)
      val rch = rhs.value.charAt(rp)
      lp += 1
      rp += 1

      if lch > rch then return if lhsIsNegative then -1 else 1
      if lch < rch then return if lhsIsNegative then 1 else -1

      llen -= 1

    0

  override def equals(o: Any): Boolean =
    o match
      case iv: IntegerValueType => value == iv.value
      case _                    => false

  override def hashCode(): Int = value.hashCode()

  override def toString: String = value

  def precision(): Int =
    val len = value.length
    if value.charAt(0) == '-' then len - 1
    else len

  def isNonPositive: Boolean =
    val ch = value.charAt(0)
    ch == '-' || ch == '0'

  def isPositive: Boolean =
    val ch = value.charAt(0)
    ch != '-' && ch != '0'

  def isNegative: Boolean =
    value.charAt(0) == '-'

  def isNonNegative: Boolean =
    value.charAt(0) != '-'

  def toBigInteger(): BigInteger =
    new BigInteger(value)

  def doubleValue(): Double = toBigInteger().doubleValue()
  def floatValue(): Float = doubleValue().toFloat
  def intValue(): Int = toBigInteger().intValue()
  def longValue(): Long = toBigInteger().longValue()

object IntegerValueType:

  def create(bi: BigInteger): IntegerValueType =
    create(bi.toString)

  def create(nonCanonicalizedValue: String): IntegerValueType =
    var idx = 0
    var v = ""
    val len = nonCanonicalizedValue.length

    if len == 0 then return null

    nonCanonicalizedValue.charAt(idx) match
      case '+' => idx += 1
      case '-' =>
        v += '-'
        idx += 1
      case ch if '0' <= ch && ch <= '9' => ()
      case _ => return null

    if idx == len then return null // sign only

    // skip leading '0'
    while idx < len && nonCanonicalizedValue.charAt(idx) == '0' do
      idx += 1

    if idx == len then return new IntegerValueType("0")

    // adding digits
    while idx < len do
      val ch = nonCanonicalizedValue.charAt(idx)
      idx += 1
      if '0' <= ch && ch <= '9' then
        v += ch
      else
        return null

    new IntegerValueType(v)
