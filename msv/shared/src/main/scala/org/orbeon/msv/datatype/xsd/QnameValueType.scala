/*
 * @(#)$Id: QnameValueType.java,v 1.12 2003/06/09 20:49:24 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import java.io.Serializable

/**
 * Value object of QName.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class QnameValueType(val namespaceURI: String, val localPart: String) extends Serializable:

  override def equals(o: Any): Boolean =
    o match
      case rhs: QnameValueType =>
        namespaceURI == rhs.namespaceURI && localPart == rhs.localPart
      case _ => false

  override def hashCode(): Int =
    namespaceURI.hashCode + localPart.hashCode

  override def toString: String =
    "{" + namespaceURI + "}:" + localPart
