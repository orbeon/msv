/*
 * @(#)$Id: BinaryValueType.java,v 1.10 2003/06/09 20:49:20 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import java.io.Serializable
import java.util.Arrays

/**
 * Value type of "base64Binary" and "hexBinary" type.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class BinaryValueType(val rawData: Array[Byte]) extends Serializable:

  override def equals(o: Any): Boolean =
    o match
      case rhs: BinaryValueType =>
        Arrays.equals(rawData, rhs.rawData)
      case _ => false

  override def hashCode(): Int =
    if rawData.length == 0 then 293
    else if rawData.length == 1 then rawData(0).toInt
    else rawData.length * rawData(0) * rawData(1)
