/*
 * @(#)$Id: TimeType.java,v 1.18 2004/04/05 17:41:50 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * "time" type.
 * 
 * Type of the value object is {@link org.orbeon.msv.datatype.xsd.datetime.IDateTimeValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#time for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TimeType private () extends DateTimeBaseType("time"):

  protected final def getFormat(): String = "%h:%m:%s%z"

object TimeType:
  val theInstance: TimeType = new TimeType()
