/*
 * @(#)$Id: DateTimeType.java,v 1.20 2004/04/05 17:41:48 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * "dateTime" type.
 * 
 * Type of the value object is {@link org.orbeon.msv.datatype.xsd.datetime.IDateTimeValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#dateTime for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DateTimeType private () extends DateTimeBaseType("dateTime"):

  protected final def getFormat(): String = "%Y-%M-%DT%h:%m:%s%z"

object DateTimeType:
  val theInstance: DateTimeType = new DateTimeType()
