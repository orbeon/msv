/*
 * @(#)$Id: GYearMonthType.java,v 1.15 2004/04/05 17:41:49 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * "gYearMonth" type.
 * 
 * Type of the value object is {@link org.orbeon.msv.datatype.xsd.datetime.IDateTimeValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#gYearMonth for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class GYearMonthType private () extends DateTimeBaseType("gYearMonth"):

  protected final def getFormat(): String = "%Y-%M%z"

object GYearMonthType:
  val theInstance: GYearMonthType = new GYearMonthType()
