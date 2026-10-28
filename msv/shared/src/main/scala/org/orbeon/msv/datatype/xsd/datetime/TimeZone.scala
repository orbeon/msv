/*
 * @(#)$Id: TimeZone.java,v 1.10 2003/12/16 21:39:13 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

import java.io.Serializable
import java.util.TimeZone as JTimeZone

/**
 * Simple time zone component.
 * 
 * @author Kohsuke KAWAGUCHI
 */
class TimeZone extends Serializable {
  /**
   * Difference from GMT in terms of minutes.
   * @deprecated here just for the serialization backward compatibility.
   */
  var minutes: Int = 0

  private def readResolve(): AnyRef =
    TimeZone.create(minutes * 60 * 1000, "")
}

object TimeZone {
  class CustomTimeZone(rawOffset: Int, id: String) extends JTimeZone with Serializable {
    setID(id)
    def getOffset(era: Int, year: Int, month: Int, day: Int, dayOfWeek: Int, milliseconds: Int): Int = rawOffset
    def getRawOffset: Int = rawOffset
    def setRawOffset(offsetMillis: Int): Unit = ()
    override def useDaylightTime(): Boolean = false
    override def inDaylightTime(date: java.util.Date): Boolean = false
    override def getDisplayName(daylight: Boolean, style: Int, locale: java.util.Locale): String = getID
  }

  def create(rawOffset: Int, id: String): JTimeZone =
    new CustomTimeZone(rawOffset, id)

  private class JavaZeroTimeZone extends CustomTimeZone(0, "XSD 'Z' timezone") {
    protected def readResolve(): AnyRef = ZERO
  }

  private class JavaMissingTimeZone extends CustomTimeZone(0, "XSD missing timezone") {
    protected def readResolve(): AnyRef = MISSING
  }

  /**
   * The {@link java.util.TimeZone} representation that corresponds
   * to the ZERO singleton instance. Once again, using a special
   * instance is a hack to make the round-tripping work OK.
   */
  val ZERO: JTimeZone = new JavaZeroTimeZone()

  /**
   * The {@link java.util.TimeZone} representation that corresponds
   * to the missing time zone.
   */
  val MISSING: JTimeZone = new JavaMissingTimeZone()

  /**
   * @deprecated exists just for the backward serialization compatibility.
   */
  class ZeroTimeZone extends TimeZone {
    protected def readResolve(): AnyRef = ZERO
  }
}
