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
import java.util.SimpleTimeZone

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
    new SimpleTimeZone(minutes * 60 * 1000, "")
}

object TimeZone {
  private class JavaZeroTimeZone extends SimpleTimeZone(0, "XSD 'Z' timezone") with Serializable {
    protected def readResolve(): AnyRef = ZERO
  }

  private class JavaMissingTimeZone extends SimpleTimeZone(0, "XSD missing timezone") with Serializable {
    protected def readResolve(): AnyRef = MISSING
  }

  /**
   * The {@link java.util.TimeZone} representation that corresponds
   * to the ZERO singleton instance. Once again, using a special
   * instance is a hack to make the round-tripping work OK.
   */
  val ZERO: java.util.TimeZone = new JavaZeroTimeZone()

  /**
   * The {@link java.util.TimeZone} representation that corresponds
   * to the missing time zone.
   */
  val MISSING: java.util.TimeZone = new JavaMissingTimeZone()

  /**
   * @deprecated exists just for the backward serialization compatibility.
   */
  class ZeroTimeZone extends TimeZone {
    protected def readResolve(): AnyRef = ZERO
  }
}
