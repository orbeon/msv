/*
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

import java.io.Serializable

/**
 * Self-contained XML Schema timezone representation (fixed minute offset from UTC).
 */
final case class TimeZone(minutes: Int) extends Serializable

object TimeZone:
  val ZERO: TimeZone = TimeZone(0)
  val MISSING: TimeZone = null

  def fromMinutes(minutes: Int): TimeZone = TimeZone(minutes)
  def create(rawOffsetMillis: Int, id: String = ""): TimeZone = TimeZone(rawOffsetMillis / (60 * 1000))
