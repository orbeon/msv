/*
 * @(#)$Id: Base64BinaryType.java,v 1.25 2005/03/31 23:19:09 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * "base64Binary" type.
 * 
 * Type of the value object is {@link BinaryValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#base64Binary for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class Base64BinaryType private () extends BinaryBaseType("base64Binary"):

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    val buf = Base64BinaryType.load(lexicalValue)
    if buf == null then null
    else new BinaryValueType(buf)

  def checkFormat(lexicalValue: String, context: ValidationContext): Boolean =
    Base64BinaryType.calcLength(lexicalValue.toCharArray) != -1

  override def serializeJavaObject(value: AnyRef, context: SerializationContext): String =
    value match
      case bytes: Array[Byte] => Base64BinaryType.save(bytes)
      case _                  => throw new IllegalArgumentException()

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case bvt: BinaryValueType => serializeJavaObject(bvt.rawData, context)
      case _                    => throw new IllegalArgumentException()

object Base64BinaryType:
  val theInstance: Base64BinaryType = new Base64BinaryType()

  private val decodeMap: Array[Byte] = initDecodeMap()
  private val PADDING: Byte = 127

  private def initDecodeMap(): Array[Byte] =
    val map = new Array[Byte](256)
    var i = 0
    while i < 256 do { map(i) = -1; i += 1 }

    i = 'A'
    while i <= 'Z' do { map(i) = (i - 'A').toByte; i += 1 }
    i = 'a'
    while i <= 'z' do { map(i) = (i - 'a' + 26).toByte; i += 1 }
    i = '0'
    while i <= '9' do { map(i) = (i - '0' + 52).toByte; i += 1 }
    map('+') = 62
    map('/') = 63
    map('=') = PADDING

    map

  def load(lexicalValue: String): Array[Byte] =
    val buf = lexicalValue.toCharArray
    val outlen = calcLength(buf)
    if outlen == -1 then return null

    val out = new Array[Byte](outlen)
    var o = 0
    val len = buf.length
    val quadruplet = new Array[Byte](4)
    var q = 0

    var i = 0
    while i < len do
      val v = decodeMap(buf(i).toInt)
      if v != -1 then
        quadruplet(q) = v
        q += 1

      if q == 4 then
        out(o) = ((quadruplet(0) << 2) | ((quadruplet(1) & 0xff) >> 4)).toByte
        o += 1
        if quadruplet(2) != PADDING then
          out(o) = (((quadruplet(1) & 0xff) << 4) | ((quadruplet(2) & 0xff) >> 2)).toByte
          o += 1
        if quadruplet(3) != PADDING then
          out(o) = (((quadruplet(2) & 0xff) << 6) | (quadruplet(3) & 0xff)).toByte
          o += 1
        q = 0
      i += 1

    if q != 0 then throw new IllegalStateException()

    out

  def calcLength(buf: Array[Char]): Int =
    val len = buf.length
    var base64count = 0
    var paddingCount = 0
    var i = 0

    var broken = false
    while i < len && !broken do
      if buf(i) == '=' then
        broken = true
      else if buf(i) >= 256 then
        return -1
      else
        if decodeMap(buf(i).toInt) != -1 then
          base64count += 1
        i += 1

    while i < len do
      if buf(i) == '=' then
        paddingCount += 1
      else if buf(i) >= 256 then
        return -1
      else if decodeMap(buf(i).toInt) != -1 then
        return -1
      i += 1

    if paddingCount > 2 then return -1
    if (base64count + paddingCount) % 4 != 0 then return -1

    ((base64count + paddingCount) / 4) * 3 - paddingCount

  private val encodeMap: Array[Char] = initEncodeMap()

  private def initEncodeMap(): Array[Char] =
    val map = new Array[Char](64)
    var i = 0
    while i < 26 do { map(i) = ('A' + i).toChar; i += 1 }
    i = 26
    while i < 52 do { map(i) = ('a' + (i - 26)).toChar; i += 1 }
    i = 52
    while i < 62 do { map(i) = ('0' + (i - 52)).toChar; i += 1 }
    map(62) = '+'
    map(63) = '/'
    map

  protected def encode(i: Int): Char =
    encodeMap(i & 0x3f)

  def save(input: Array[Byte]): String =
    val r = new java.lang.StringBuilder(input.length * 4 / 3)
    var i = 0
    while i < input.length do
      input.length - i match
        case 1 =>
          val b0 = input(i) & 0xff
          r.append(encode(b0 >> 2))
          r.append(encode((b0 & 0x3) << 4))
          r.append("==")
        case 2 =>
          val b0 = input(i) & 0xff
          val b1 = input(i + 1) & 0xff
          r.append(encode(b0 >> 2))
          r.append(encode(((b0 & 0x3) << 4) | (b1 >> 4)))
          r.append(encode((b1 & 0xf) << 2))
          r.append("=")
        case _ =>
          val b0 = input(i) & 0xff
          val b1 = input(i + 1) & 0xff
          val b2 = input(i + 2) & 0xff
          r.append(encode(b0 >> 2))
          r.append(encode(((b0 & 0x3) << 4) | (b1 >> 4)))
          r.append(encode(((b1 & 0xf) << 2) | (b2 >> 6)))
          r.append(encode(b2 & 0x3f))
      i += 3
    r.toString
