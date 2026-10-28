/*
 * @(#)$Id: WhiteSpaceProcessor.java,v 1.24 2005/03/31 23:19:11 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.DatatypeException

import java.io.InvalidObjectException
import java.io.Serializable

/**
 * Processes white space normalization.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class WhiteSpaceProcessor extends Serializable:

  /**
   * Returns whitespace normalized text.
   * Behavior varies on what normalization mode is used.
   */
  def process(text: String): String

  /** Higher return value indicates tighter constraint */
  def tightness(): Int

  /**
   * Gets the name of the white space processing mode.
   * It is one of "preserve", "collapse", or "replace".
   */
  def getName: String

  @throws(classOf[InvalidObjectException])
  protected def readResolve(): AnyRef =
    try
      WhiteSpaceProcessor.get(getName)
    catch
      case _: DatatypeException =>
        throw new InvalidObjectException("Unknown Processing Mode")

object WhiteSpaceProcessor:

  /**
   * Returns a WhiteSpaceProcessor object if "whiteSpace" facet is specified.
   * Otherwise returns null.
   */
  @throws(classOf[DatatypeException])
  def get(name: String): WhiteSpaceProcessor =
    val processed = theCollapse.process(name)
    processed match
      case "preserve" => thePreserve
      case "collapse" => theCollapse
      case "replace"  => theReplace
      case _ =>
        throw new DatatypeException(
          XSDatatypeImpl.localize(XSDatatypeImpl.ERR_INVALID_WHITESPACE_VALUE, processed)
        )

  /** Returns true if the specified char is a white space character. */
  def isWhiteSpace(ch: Char): Boolean =
    ch == 0x9 || ch == 0xa || ch == 0xd || ch == 0x20

  // short-cut methods
  def replace(str: String): String = theReplace.process(str)
  def collapse(str: String): String = theCollapse.process(str)

  val thePreserve: WhiteSpaceProcessor = new Preserve()
  val theReplace: WhiteSpaceProcessor = new Replace()
  val theCollapse: WhiteSpaceProcessor = new Collapse()

  private class Preserve extends WhiteSpaceProcessor:
    def process(text: String): String = text
    def tightness(): Int = 0
    def getName: String = "preserve"

  private class Replace extends WhiteSpaceProcessor:
    def process(text: String): String =
      val len = text.length
      val result = new java.lang.StringBuilder(len)
      var i = 0
      while i < len do
        val ch = text.charAt(i)
        if WhiteSpaceProcessor.isWhiteSpace(ch) then
          result.append(' ')
        else
          result.append(ch)
        i += 1
      result.toString

    def tightness(): Int = 1
    def getName: String = "replace"

  private class Collapse extends WhiteSpaceProcessor:
    def process(text: String): String =
      var len = text.length
      val result = new java.lang.StringBuilder(len)
      var inStripMode = true
      var i = 0
      while i < len do
        val ch = text.charAt(i)
        val b = WhiteSpaceProcessor.isWhiteSpace(ch)
        if !(inStripMode && b) then
          inStripMode = b
          if inStripMode then
            result.append(' ')
          else
            result.append(ch)
        i += 1

      // remove trailing whitespaces
      len = result.length
      if len > 0 && result.charAt(len - 1) == ' ' then
        result.setLength(len - 1)

      result.toString

    def tightness(): Int = 2
    def getName: String = "collapse"
