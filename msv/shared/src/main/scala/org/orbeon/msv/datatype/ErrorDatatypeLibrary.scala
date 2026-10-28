/*
 * @(#)$Id: ErrorDatatypeLibrary.java,v 1.4 2005/03/31 23:19:11 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype

import org.orbeon.msv.datatype.xsd.StringType
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeBuilder, DatatypeLibrary, ValidationContext}

/**
 * Dummy `DatatypeLibrary` implementation which is used
 * to recover from "unknown datatype library" error.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class ErrorDatatypeLibrary private () extends DatatypeLibrary with DatatypeBuilder:

  def createDatatype(name: String): Datatype = StringType.theInstance

  def createDatatypeBuilder(name: String): DatatypeBuilder = this

  def createDatatype(): Datatype = StringType.theInstance

  def addParameter(name: String, value: String, context: ValidationContext): Unit = ()

object ErrorDatatypeLibrary:
  val theInstance: ErrorDatatypeLibrary = new ErrorDatatypeLibrary()
