/*
 * @(#)$Id: HexBinaryType.java,v 1.21 2005/03/31 23:19:10 kohsuke Exp $
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
 * "hexBinary" type.
 * 
 * Type of the value object is {@link BinaryValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#hexBinary for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class HexBinaryType private () extends BinaryBaseType("hexBinary"):

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    val buf = HexBinaryType.load(lexicalValue)
    if buf == null then null
    else new BinaryValueType(buf)

  def checkFormat(lexicalValue: String, context: ValidationContext): Boolean =
    val len = lexicalValue.length
    if len % 2 != 0 then return false

    var i = 0
    while i < len do
      if HexBinaryType.hexToBin(lexicalValue.charAt(i)) == -1 then
        return false
      i += 1
    true

  override def serializeJavaObject(value: AnyRef, context: SerializationContext): String =
    value match
      case bytes: Array[Byte] => HexBinaryType.save(bytes)
      case _                  => throw new IllegalArgumentException()

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case bvt: BinaryValueType => serializeJavaObject(bvt.rawData, context)
      case _                    => throw new IllegalArgumentException()

object HexBinaryType:
  val theInstance: HexBinaryType = new HexBinaryType()

  def hexToBin(ch: Char): Int =
    if '0' <= ch && ch <= '9' then ch - '0'
    else if 'A' <= ch && ch <= 'F' then ch - 'A' + 10
    else if 'a' <= ch && ch <= 'f' then ch - 'a' + 10
    else -1

  def load(s: String): Array[Byte] =
    val len = s.length
    if len % 2 != 0 then return null

    val out = new Array[Byte](len / 2)
    var i = 0
    while i < len do
      val h = hexToBin(s.charAt(i))
      val l = hexToBin(s.charAt(i + 1))
      if h == -1 || l == -1 then return null
      out(i / 2) = (h * 16 + l).toByte
      i += 2
    out

  def save(data: Array[Byte]): String =
    val r = new java.lang.StringBuilder(data.length * 2)
    var i = 0
    while i < data.length do
      r.append(encode(data(i) >> 4))
      r.append(encode(data(i) & 0xf))
      i += 1
    r.toString

  def encode(ch0: Int): Char =
    val ch = ch0 & 0xf
    if ch < 10 then ('0' + ch).toChar
    else ('A' + (ch - 10)).toChar
