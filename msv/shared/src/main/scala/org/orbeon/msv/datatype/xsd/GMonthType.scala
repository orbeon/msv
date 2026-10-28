/*
 * @(#)$Id: GMonthType.java,v 1.16 2004/04/05 17:41:49 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * "gMonth" type.
 * 
 * Type of the value object is {@link org.orbeon.msv.datatype.xsd.datetime.IDateTimeValueType}.
 * See http://www.w3.org/TR/xmlschema-2/#gMonth for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class GMonthType private () extends DateTimeBaseType("gMonth"):

  protected final def getFormat(): String = "--%M--%z"

object GMonthType:
  val theInstance: GMonthType = new GMonthType()
