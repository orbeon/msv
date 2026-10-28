/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2002 The Apache Software Foundation.  All rights 
 * reserved.
 */

package org.orbeon.msv.datatype.regexp



/**
 * Boyer-Moore searcher.
 *
 * @version BMPattern.java,v 1.3 2002/08/09 15:18:17 neilg Exp
 */
final class BMPattern(
  pat: String,
  tableSize: Int,
  val ignoreCase: Boolean
) {

  def this(pat: String, ignoreCase: Boolean) =
    this(pat, 256, ignoreCase)

  val pattern: Array[Char] = pat.toCharArray
  val shiftTable: Array[Int] = new Array[Int](tableSize)

  private val length = pattern.length
  var i = 0
  while (i < shiftTable.length) {
    shiftTable(i) = length
    i += 1
  }

  i = 0
  while (i < length) {
    var ch = pattern(i)
    val diff = length - i - 1
    var index = ch % shiftTable.length
    if (diff < shiftTable(index))
      shiftTable(index) = diff
    if (ignoreCase) {
      ch = Character.toUpperCase(ch)
      index = ch % shiftTable.length
      if (diff < shiftTable(index))
        shiftTable(index) = diff
      ch = Character.toLowerCase(ch)
      index = ch % shiftTable.length
      if (diff < shiftTable(index))
        shiftTable(index) = diff
    }
    i += 1
  }


  /**
   * @return -1 if str does not contain this pattern.
   */
  def matches(str: String, start: Int, limit: Int): Int = {
    if (ignoreCase) return matchesIgnoreCase(str, start, limit)
    val plength = pattern.length
    if (plength == 0) return start
    var index = start + plength
    while (index <= limit) {
      var pindex = plength
      val nindex = index + 1
      var ch: Char = 0
      var matched = false
      var breakLoop = false
      while (!breakLoop && pindex > 0) {
        index -= 1
        pindex -= 1
        ch = str.charAt(index)
        if (ch != pattern(pindex)) {
          breakLoop = true
        } else if (pindex == 0) {
          matched = true
          breakLoop = true
        }
      }
      if (matched) return index
      index += shiftTable(ch % shiftTable.length) + 1
      if (index < nindex) index = nindex
    }
    -1
  }

  /**
   * @return -1 if chars does not contain this pattern.
   */
  def matches(chars: Array[Char], start: Int, limit: Int): Int = {
    if (ignoreCase) return matchesIgnoreCase(chars, start, limit)
    val plength = pattern.length
    if (plength == 0) return start
    var index = start + plength
    while (index <= limit) {
      var pindex = plength
      val nindex = index + 1
      var ch: Char = 0
      var matched = false
      var breakLoop = false
      while (!breakLoop && pindex > 0) {
        index -= 1
        pindex -= 1
        ch = chars(index)
        if (ch != pattern(pindex)) {
          breakLoop = true
        } else if (pindex == 0) {
          matched = true
          breakLoop = true
        }
      }
      if (matched) return index
      index += shiftTable(ch % shiftTable.length) + 1
      if (index < nindex) index = nindex
    }
    -1
  }


  def matchesIgnoreCase(text: String, start: Int, limit: Int): Int = {
    val plength = pattern.length
    if (plength == 0) return start
    var index = start + plength
    while (index <= limit) {
      var pindex = plength
      val nindex = index + 1
      var ch: Char = 0
      var matched = false
      var breakLoop = false
      while (!breakLoop && pindex > 0) {
        index -= 1
        pindex -= 1
        var ch1 = text.charAt(index)
        ch = ch1
        val ch2 = pattern(pindex)
        if (ch1 != ch2) {
          ch1 = Character.toUpperCase(ch1)
          val u2 = Character.toUpperCase(ch2)
          if (ch1 != u2 && Character.toLowerCase(ch1) != Character.toLowerCase(ch2)) {
            breakLoop = true
          }
        }
        if (!breakLoop && pindex == 0) {
          matched = true
          breakLoop = true
        }
      }
      if (matched) return index
      index += shiftTable(ch % shiftTable.length) + 1
      if (index < nindex) index = nindex
    }
    -1
  }

  def matchesIgnoreCase(chars: Array[Char], start: Int, limit: Int): Int = {
    val plength = pattern.length
    if (plength == 0) return start
    var index = start + plength
    while (index <= limit) {
      var pindex = plength
      val nindex = index + 1
      var ch: Char = 0
      var matched = false
      var breakLoop = false
      while (!breakLoop && pindex > 0) {
        index -= 1
        pindex -= 1
        var ch1 = chars(index)
        ch = ch1
        val ch2 = pattern(pindex)
        if (ch1 != ch2) {
          ch1 = Character.toUpperCase(ch1)
          val u2 = Character.toUpperCase(ch2)
          if (ch1 != u2 && Character.toLowerCase(ch1) != Character.toLowerCase(ch2)) {
            breakLoop = true
          }
        }
        if (!breakLoop && pindex == 0) {
          matched = true
          breakLoop = true
        }
      }
      if (matched) return index
      index += shiftTable(ch % shiftTable.length) + 1
      if (index < nindex) index = nindex
    }
    -1
  }
}
