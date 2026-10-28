/*
 * @(#)$Id: IDType.java,v 1.6 2004/04/05 17:41:50 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.Datatype

/**
 * Very limited 'ID' type of XML Schema Part 2.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IDType protected () extends NcnameType("ID"):

  override def getIdType(): Int = Datatype.ID_TYPE_ID

  override protected def readResolve(): AnyRef = IDType.theInstance

object IDType:
  val theInstance: IDType = new IDType()
