/*
 * @(#)$Id: IDREFType.java,v 1.7 2004/04/05 17:41:49 kohsuke Exp $
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
 * Very limited 'IDREF' type of XML Schema Part 2.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IDREFType protected () extends NcnameType("IDREF"):

  override def getIdType(): Int = Datatype.ID_TYPE_IDREF

  override protected def readResolve(): AnyRef = IDREFType.theInstance

object IDREFType:
  val theInstance: IDREFType = new IDREFType()
