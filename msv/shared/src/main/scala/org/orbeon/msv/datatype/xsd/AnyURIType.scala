/*
 * @(#)$Id: AnyURIType.java,v 1.30 2005/04/01 00:00:28 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.datatype.xsd.regex.{RegExp, RegExpFactory}
import org.orbeon.msv.relaxng.datatype.ValidationContext

import org.orbeon.msv.datatype.regexp.ParseException

/**
 * "anyURI" type.
 * 
 * See http://www.w3.org/TR/xmlschema-2/#anyURI for the spec.
 * Type of the value object is `java.lang.String`.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AnyURIType private () extends BuiltinAtomicType("anyURI") with Discrete:

  def checkFormat(content: String, context: ValidationContext): Boolean =
    AnyURIType.regexp.matches(AnyURIType.escape(content))

  def _createValue(content: String, context: ValidationContext): AnyRef =
    if !AnyURIType.regexp.matches(AnyURIType.escape(content)) then null
    else content

  def getJavaObjectType(): Class[?] = classOf[String]

  def getBaseType(): XSDatatype = SimpleURType.theInstance

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case s: String => s
      case _         => throw new IllegalArgumentException()

  final def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_LENGTH ||
       facetName == XSDatatype.FACET_MINLENGTH ||
       facetName == XSDatatype.FACET_MAXLENGTH ||
       facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_WHITESPACE ||
       facetName == XSDatatype.FACET_ENUMERATION then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

  final def countLength(value: AnyRef): Int =
    UnicodeUtil.countLength(value.asInstanceOf[String])

object AnyURIType:
  val theInstance: AnyURIType = new AnyURIType()

  private def appendHex(buf: java.lang.StringBuilder, hex: Int): Unit =
    if hex < 10 then buf.append((hex + '0').toChar)
    else buf.append((hex - 10 + 'A').toChar)

  private def appendByte(buf: java.lang.StringBuilder, ch: Int): Unit =
    buf.append('%')
    appendHex(buf, ch / 16)
    appendHex(buf, ch % 16)

  /** Convert one 'char' in BMP to UTF-8 encoding. */
  private def appendEscaped(buf: java.lang.StringBuilder, ch: Char): Unit =
    if ch < 0x7f then
      appendByte(buf, ch.toInt)
    else if ch < 0x7ff then
      appendByte(buf, 0xc0 + (ch >> 6))
      appendByte(buf, 0x80 + (ch % 64))
    else if ch <= 0xffff then
      appendByte(buf, 0xe0 + (ch >> 12))
      appendByte(buf, 0x80 + ((ch >> 6) % 64))
      appendByte(buf, 0x80 + (ch % 64))

  /** Convert one surrogate pair to UTF-8 encoding. */
  private def appendEscaped(buf: java.lang.StringBuilder, ch1: Char, ch2: Char): Unit =
    val ucs = (((ch1 & 0x3ff).toInt) << 10) + (ch2 & 0x3ff).toInt
    appendByte(buf, 0xf0 + (ucs >> 18))
    appendByte(buf, 0x80 + ((ucs >> 12) % 64))
    appendByte(buf, 0x80 + ((ucs >> 6) % 64))
    appendByte(buf, 0x80 + (ucs % 64))

  private val isUric: Array[Boolean] = createUricMap()

  private def createUricMap(): Array[Boolean] =
    val r = new Array[Boolean](128)
    var c = 'a'
    while c <= 'z' do { r(c) = true; c = (c + 1).toChar }
    c = 'A'
    while c <= 'Z' do { r(c) = true; c = (c + 1).toChar }
    c = '0'
    while c <= '9' do { r(c) = true; c = (c + 1).toChar }

    val mark = Array('-', '_', '.', '!', '~', '*', '\'', '(', ')', '#', '%', '[', ']')
    var i = 0
    while i < mark.length do { r(mark(i)) = true; i += 1 }

    val reserved = Array(';', '/', '?', ':', '@', '&', '=', '+', '$', ',')
    i = 0
    while i < reserved.length do { r(reserved(i)) = true; i += 1 }

    r

  /** Escape non-ASCII characters in URL */
  def escape(content: String): String =
    val escaped = new java.lang.StringBuilder(content.length)
    var i = 0
    while i < content.length do
      val ch = content.charAt(i)
      if ch < 128 && isUric(ch.toInt) then
        escaped.append(ch)
      else
        if 0xd800 <= ch && ch < 0xdc00 then
          i += 1
          appendEscaped(escaped, ch, content.charAt(i))
        else
          appendEscaped(escaped, ch)
      i += 1
    escaped.toString

  val regexp: RegExp = createRegExp()

  private def createRegExp(): RegExp =
    val alpha = "[a-zA-Z]"
    val alphanum = "[0-9a-zA-Z]"
    val hex = "[0-9a-fA-F]"
    val escaped = "%" + hex + "{2}"
    val mark = "[\\-_\\.!~\\*'\\(\\)]"
    val unreserved = "(" + alphanum + "|" + mark + ")"
    val reserved = "[;/\\?:@&=\\+$,\\[\\]]"
    val uric = "(" + reserved + "|" + unreserved + "|" + escaped + ")"
    val fragment = uric + "*"
    val query = uric + "*"
    val pchar = "(" + unreserved + "|" + escaped + "|[:@&=\\+$,])"
    val param = pchar + "*"
    val segment = "(" + param + "(;" + param + ")*)"
    val pathSegments = "(" + segment + "(/" + segment + ")*)"
    val port = "[0-9]*"
    val __upTo3digits = "[0-9]{1,3}"
    val IPv4address = __upTo3digits + "\\." + __upTo3digits + "\\." + __upTo3digits + "\\." + __upTo3digits
    val hex4 = hex + "{1,4}"
    val hexseq = hex4 + "(:" + hex4 + ")*"
    val hexpart = "((" + hexseq + "(::(" + hexseq + ")?)?)|(::(" + hexseq + ")?))"
    val IPv6address = "((" + hexpart + "(:" + IPv4address + ")?)|(::" + IPv4address + "))"
    val IPv6reference = "\\[" + IPv6address + "\\]"
    val domainlabel = alphanum + "([0-9A-Za-z\\-]*" + alphanum + ")?"
    val toplabel = alpha + "([0-9A-Za-z\\-]*" + alphanum + ")?"
    val hostname = "(" + domainlabel + "\\.)*" + toplabel + "(\\.)?"
    val host = "((" + hostname + ")|(" + IPv4address + ")|(" + IPv6reference + "))"
    val hostport = host + "(:" + port + ")?"
    val userinfo = "(" + unreserved + "|" + escaped + "|[;:&=\\+$,])*"
    val server = "((" + userinfo + "@)?" + hostport + ")?"
    val regName = "(" + unreserved + "|" + escaped + "|[$,;:@&=\\+])+"
    val authority = "((" + server + ")|(" + regName + "))"
    val scheme = alpha + "[A-Za-z0-9\\+\\-\\.]*"
    val relSegment = "(" + unreserved + "|" + escaped + "|[;@&=\\+$,])+"
    val absPath = "/" + pathSegments
    val relPath = relSegment + "(" + absPath + ")?"
    val netPath = "//" + authority + "(" + absPath + ")?"
    val uricNoSlash = "(" + unreserved + "|" + escaped + "|[;\\?:@&=\\+$,])"
    val opaquePart = uricNoSlash + "(" + uric + ")*"
    val hierPart = "((" + netPath + ")|(" + absPath + "))(\\?" + query + ")?"
    val relativeURI = "((" + netPath + ")|(" + absPath + ")|(" + relPath + "))(\\?" + query + ")?"
    val absoluteURI = scheme + ":((" + hierPart + ")|(" + opaquePart + "))"
    val uriRef = "(" + absoluteURI + "|" + relativeURI + ")?(#" + fragment + ")?"

    try
      RegExpFactory.createFactory().compile(uriRef)
    catch
      case _: ParseException =>
        throw new Error()
