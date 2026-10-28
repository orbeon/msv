/*
 * @(#)$Id: DataTypeLibraryImpl.java,v 1.9 2005/03/31 23:19:08 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.ngimpl

import org.orbeon.msv.datatype.xsd.{DatatypeFactory, XSDatatype}
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeBuilder, DatatypeException, DatatypeLibrary, DatatypeLibraryFactory}

/**
 * DatatypeLibrary implementation for Sun XML Datatypes Library.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DataTypeLibraryImpl extends DatatypeLibrary with DatatypeLibraryFactory:

  @throws(classOf[DatatypeException])
  def createDatatype(typeName: String): Datatype =
    getType(typeName)

  @throws(classOf[DatatypeException])
  private def getType(typeName: String): XSDatatype =
    DatatypeFactory.getTypeByName(typeName)

  @throws(classOf[DatatypeException])
  def createDatatypeBuilder(typeName: String): DatatypeBuilder =
    new DatatypeBuilderImpl(getType(typeName))

  def createDatatypeLibrary(uri: String): DatatypeLibrary =
    if uri == "http://www.w3.org/2001/XMLSchema" ||
       uri == "http://www.w3.org/2001/XMLSchema-datatypes" then
      this
    else
      null
