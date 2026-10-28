/*
 * @(#)XmlChars.java    1.8 99/01/22
 * 
 * Copyright (c) 1998 Sun Microsystems, Inc. All Rights Reserved.
 */

package org.orbeon.msv.xml.util

/**
 * Methods in this class are used to determine whether characters may
 * appear in certain roles in XML documents. Such methods are used
 * both to parse and to create such documents.
 *
 * @version 1.8
 * @author David Brownell
 */
object XmlChars {

  /**
   * Returns true if the argument, a UCS-4 character code, is valid in
   * XML documents.
   *
   * @param ucs4char The 32-bit UCS-4 character being tested.
   */
  def isChar(ucs4char: Int): Boolean =
    (ucs4char >= 0x0020 && ucs4char <= 0xd7ff) ||
      ucs4char == 0x000a || ucs4char == 0x0009 ||
      ucs4char == 0x000d ||
      (ucs4char >= 0xe000 && ucs4char <= 0xfffd) ||
      (ucs4char >= 0x10000 && ucs4char <= 0x10ffff)

  /**
   * Returns true if the character is allowed to be a non-initial
   * character in names according to the XML recommendation.
   * @see #isNCNameChar(char)
   * @see #isLetter(char)
   */
  def isNameChar(c: Char): Boolean =
    if (isLetter2(c))
      true
    else if (c == '>')
      false
    else if (c == '.' || c == '-' || c == '_' || c == ':' || isExtender(c))
      true
    else
      false

  /**
   * Returns true if the character is allowed to be a non-initial
   * character in unscoped names according to the rules of the XML
   * Namespaces proposed recommendation.
   * @see #isNameChar(char)
   * @see #isLetter(char)
   */
  def isNCNameChar(c: Char): Boolean =
    c != ':' && isNameChar(c)

  /**
   * Returns true if the character is allowed where XML supports
   * whitespace characters, false otherwise.
   */
  def isSpace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\n' || c == '\r'

  /**
   * Returns true if the character is an XML "Letter". XML Names must start
   * with Letters or a few other characters, but other characters in names
   * must only satisfy the <em>isNameChar</em> predicate.
   *
   * @see #isNameChar(char)
   * @see #isNCNameChar(char)
   */
  def isLetter(c: Char): Boolean = {
    if (c >= 'a' && c <= 'z')
      return true
    if (c == '>')
      return false
    if (c >= 'A' && c <= 'Z')
      return true

    Character.getType(c) match {
      case Character.LOWERCASE_LETTER |
           Character.UPPERCASE_LETTER |
           Character.OTHER_LETTER |
           Character.TITLECASE_LETTER |
           Character.LETTER_NUMBER =>
        !isCompatibilityChar(c)
      case _ =>
        c == 0x0387 || c == 0x212e
    }
  }

  // Returns true for chars which are "compatibility" or "composition"
  private def isCompatibilityChar(c: Char): Boolean =
    (c >> 8) match {
      case 0x00 =>
        c == 0x00aa || c == 0x00ba || c == 0x00b5 ||
          (c >= 0x00c0 && c <= 0x00d6) ||
          (c >= 0x00d8 && c <= 0x00f6) ||
          (c >= 0x00f8 && c <= 0x00ff)

      case 0x01 =>
        c == 0x0132 || c == 0x0133 || c == 0x013f || c == 0x0140 || c == 0x0149 ||
          c == 0x017f ||
          (c >= 0x01c4 && c <= 0x01cc) ||
          (c >= 0x01f1 && c <= 0x01f3)

      case 0x02 =>
        (c >= 0x02b0 && c <= 0x02b8) || (c >= 0x02e0 && c <= 0x02e4)

      case 0x03 =>
        c == 0x037a

      case 0x05 =>
        c == 0x0587

      case 0x0e =>
        c >= 0x0edc && c <= 0x0edd

      case 0x11 =>
        c == 0x1101 || c == 0x1104 || c == 0x1108 || c == 0x110a || c == 0x110d ||
          (c >= 0x1113 && c <= 0x113b) || c == 0x113d || c == 0x113f ||
          (c >= 0x1141 && c <= 0x114b) || c == 0x114d || c == 0x114f ||
          (c >= 0x1151 && c <= 0x1153) || (c >= 0x1156 && c <= 0x1158) ||
          c == 0x1162 || c == 0x1164 || c == 0x1166 || c == 0x1168 ||
          (c >= 0x116a && c <= 0x116c) || (c >= 0x116f && c <= 0x1171) ||
          c == 0x1174 || (c >= 0x1176 && c <= 0x119d) ||
          (c >= 0x119f && c <= 0x11a2) || (c >= 0x11a9 && c <= 0x11aa) ||
          (c >= 0x11ac && c <= 0x11ad) || (c >= 0x11b0 && c <= 0x11b6) ||
          c == 0x11b9 || c == 0x11bb || (c >= 0x11c3 && c <= 0x11ea) ||
          (c >= 0x11ec && c <= 0x11ef) || (c >= 0x11f1 && c <= 0x11f8)

      case 0x20 =>
        c == 0x207f

      case 0x21 =>
        c == 0x2102 || c == 0x2107 ||
          (c >= 0x210a && c <= 0x2113) || c == 0x2115 ||
          (c >= 0x2118 && c <= 0x211d) || c == 0x2124 || c == 0x2128 ||
          (c >= 0x212c && c <= 0x212d) || (c >= 0x212f && c <= 0x2138) ||
          (c >= 0x2160 && c <= 0x217f)

      case 0x30 =>
        c >= 0x309b && c <= 0x309c

      case 0x31 =>
        c >= 0x3131 && c <= 0x318e

      case 0xf9 | 0xfa | 0xfb | 0xfc | 0xfd | 0xfe | 0xff =>
        true

      case _ =>
        false
    }

  private def isLetter2(c: Char): Boolean = {
    if (c >= 'a' && c <= 'z')
      return true
    if (c == '>')
      return false
    if (c >= 'A' && c <= 'Z')
      return true

    Character.getType(c) match {
      case Character.LOWERCASE_LETTER |
           Character.UPPERCASE_LETTER |
           Character.OTHER_LETTER |
           Character.TITLECASE_LETTER |
           Character.LETTER_NUMBER |
           Character.COMBINING_SPACING_MARK |
           Character.ENCLOSING_MARK |
           Character.NON_SPACING_MARK |
           Character.MODIFIER_LETTER |
           Character.DECIMAL_DIGIT_NUMBER =>
        !isCompatibilityChar(c) && !(c >= 0x20dd && c <= 0x20e0)

      case Character.FORMAT =>
        c == 0x06dd

      case _ =>
        c == 0x0387 || c == 0x212e
    }
  }

  private def isExtender(c: Char): Boolean =
    c == 0x00b7 || c == 0x02d0 || c == 0x02d1 || c == 0x0387 ||
      c == 0x0640 || c == 0x0e46 || c == 0x0ec6 ||
      c == 0x3005 || (c >= 0x3031 && c <= 0x3035) ||
      (c >= 0x309d && c <= 0x309e) ||
      (c >= 0x30fc && c <= 0x30fe)
}
