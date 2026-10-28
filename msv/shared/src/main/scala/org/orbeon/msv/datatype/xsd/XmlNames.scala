/*
 * @(#)XmlNames.java    1.4 99/01/22
 * 
 * Copyright (c) 1998 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the confidential and proprietary information of Sun
 * Microsystems, Inc. ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Sun.
 * 
 * SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.xml.util.XmlChars

/**
 * This class contains static methods used to determine whether identifiers
 * may appear in certain roles in XML documents. Such methods are used
 * both to parse and to create such documents.
 *
 * @version 1.4
 * @author David Brownell
 */
object XmlNames:

  /**
   * Returns true if the value is a legal XML name.
   *
   * @param value the string being tested
   */
  def isName(value: String): Boolean =
    if value == null || value.length == 0 then
      return false

    val c = value.charAt(0)
    if !XmlChars.isLetter(c) && c != '_' && c != ':' then
      return false
    var i = 1
    while i < value.length do
      if !XmlChars.isNameChar(value.charAt(i)) then
        return false
      i += 1
    true

  /**
   * Returns true if the value is a legal "unqualified" XML name, as
   * defined in the XML Namespaces proposed recommendation.
   * These are normal XML names, except that they may not contain
   * a "colon" character.
   *
   * @param value the string being tested
   */
  def isUnqualifiedName(value: String): Boolean =
    if value == null || value.length == 0 then
      return false

    val c = value.charAt(0)
    if !XmlChars.isLetter(c) && c != '_' then
      return false
    var i = 1
    while i < value.length do
      if !XmlChars.isNCNameChar(value.charAt(i)) then
        return false
      i += 1
    true

  /**
   * Returns true if the value is a legal "qualified" XML name, as defined
   * in the XML Namespaces proposed recommendation. Qualified names are
   * composed of an optional prefix (an unqualified name), followed by a
   * colon, and a required "local part" (an unqualified name). Prefixes are
   * declared, and correspond to particular URIs which scope the "local
   * part" of the name. (This method cannot check whether the prefix of a
   * name has been declared.)
   *
   * @param value the string being tested
   */
  def isQualifiedName(value: String): Boolean =
    if value == null || value.length == 0 then
      return false

    val first = value.indexOf(':')

    // no Prefix, only check LocalPart
    if first <= 0 then
      return isUnqualifiedName(value)

    // Prefix exists, check everything
    val last = value.lastIndexOf(':')
    if last != first then
      return false

    isUnqualifiedName(value.substring(0, first)) &&
      isUnqualifiedName(value.substring(first + 1))

  /**
   * This method returns true if the identifier is a "name token"
   * as defined in the XML specification. Like names, these
   * may only contain "name characters"; however, they do not need
   * to have letters as their initial characters. Attribute values
   * defined to be of type NMTOKEN(S) must satisfy this predicate.
   *
   * @param token the string being tested
   */
  def isNmtoken(token: String): Boolean =
    if token == null || token.length == 0 then
      return false

    val length = token.length
    var i = 0
    while i < length do
      if !XmlChars.isNameChar(token.charAt(i)) then
        return false
      i += 1
    true

  /**
   * This method returns true if the identifier is a "name token" as
   * defined by the XML Namespaces proposed recommendation.
   * These are like XML "name tokens" but they may not contain the
   * "colon" character.
   *
   * @see #isNmtoken(String)
   * @param token the string being tested
   */
  def isNCNmtoken(token: String): Boolean =
    isNmtoken(token) && token.indexOf(':') < 0

  def isNCName(token: String): Boolean =
    isName(token) && token.indexOf(':') < 0
