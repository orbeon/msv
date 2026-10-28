/*
Copyright (c) 2001, 2002 Thai Open Source Software Center Ltd
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

    Neither the name of the Thai Open Source Software Center Ltd nor
    the names of its contributors may be used to endorse or promote
    products derived from this software without specific prior written
    permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package org.orbeon.msv.util

import java.net.URI
import java.nio.charset.StandardCharsets

object Uri {
  private val HEX_DIGITS = "0123456789abcdef"
  private val excluded   = "<>\"{}|\\^`"

  def isValid(s: String): Boolean =
    isValidPercent(s) && isValidFragment(s) && isValidScheme(s)

  def escapeDisallowedChars(s: String): String = {
    var buf: StringBuilder = null
    val len = s.length
    var done = 0

    while (done < len) {
      var i = done
      while (i < len && !isExcluded(s.charAt(i))) {
        i += 1
      }
      if (i == len && done == 0)
        return s

      if (buf == null)
        buf = new StringBuilder()
      if (i > done) {
        buf.append(s.substring(done, i))
        done = i
      }
      if (i == len) {
        // done
      } else {
        i += 1
        while (i < len && isExcluded(s.charAt(i))) {
          i += 1
        }
        val tem = s.substring(done, i)
        val bytes = tem.getBytes(StandardCharsets.UTF_8)
        var j = 0
        while (j < bytes.length) {
          buf.append('%')
          buf.append(HEX_DIGITS.charAt((bytes(j) & 0xff) >> 4))
          buf.append(HEX_DIGITS.charAt(bytes(j) & 0xf))
          j += 1
        }
        done = i
      }
    }

    if (buf != null) buf.toString else s
  }

  private def isExcluded(c: Char): Boolean =
    c <= 0x20 || c >= 0x7f || excluded.indexOf(c) >= 0

  private def isAlpha(c: Char): Boolean =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  private def isHexDigit(c: Char): Boolean =
    ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F') || isDigit(c)

  private def isDigit(c: Char): Boolean =
    '0' <= c && c <= '9'

  private def isSchemeChar(c: Char): Boolean =
    isAlpha(c) || isDigit(c) || c == '+' || c == '-' || c == '.'

  private def isValidPercent(s: String): Boolean = {
    val len = s.length
    var i = 0
    while (i < len) {
      if (s.charAt(i) == '%') {
        if (i + 2 >= len)
          return false
        else if (!isHexDigit(s.charAt(i + 1)) || !isHexDigit(s.charAt(i + 2)))
          return false
      }
      i += 1
    }
    true
  }

  private def isValidFragment(s: String): Boolean = {
    val i = s.indexOf('#')
    i < 0 || s.indexOf('#', i + 1) < 0
  }

  private def isValidScheme(s: String): Boolean = {
    if (!isAbsolute(s))
      return true
    var i = s.indexOf(':')
    if (i == 0 || i + 1 == s.length || !isAlpha(s.charAt(0)))
      return false
    i -= 1
    while (i > 0) {
      if (!isSchemeChar(s.charAt(i)))
        return false
      i -= 1
    }
    true
  }

  def resolve(baseUri: String, uriReference: String): String = {
    if (!isAbsolute(uriReference) && baseUri != null && isAbsolute(baseUri)) {
      try {
        new URI(baseUri).resolve(uriReference).toString
      } catch {
        case _: Exception =>
          uriReference
      }
    } else {
      uriReference
    }
  }

  def hasFragmentId(uri: String): Boolean =
    uri.indexOf('#') >= 0

  def isAbsolute(uri: String): Boolean = {
    var i = uri.indexOf(':')
    if (i < 0)
      return false
    i -= 1
    while (i >= 0) {
      uri.charAt(i) match {
        case '#' | '/' | '?' => return false
        case _               =>
      }
      i -= 1
    }
    true
  }
}
