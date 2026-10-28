/*
 * @(#)$Id: GMonthDayType.java,v 1.15 2004/04/05 17:41:49 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * "gMonthDay" type.
 * 
 * Type of the value object is {@link org.orbeon.msv.datatype.xsd.datetime.IDateTimeValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#gMonthDay for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class GMonthDayType private () extends DateTimeBaseType("gMonthDay"):

  protected final def getFormat(): String = "--%M-%D%z"

object GMonthDayType:
  val theInstance: GMonthDayType = new GMonthDayType()
