/*
 * @(#)$Id: ListValueType.java,v 1.11 2003/06/09 20:49:22 kk122374 Exp $
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
 * Value object of ListType.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ListValueType(val values: Array[AnyRef]) extends Serializable:

  /**
   * Two ListValueType are equal if and only if all the array members
   * are equal respectively.
   */
  override def equals(o: Any): Boolean =
    o match
      case rhs: ListValueType =>
        val len = values.length
        if len != rhs.values.length then false
        else
          var i = 0
          while i < len do
            if values(i) != rhs.values(i) then return false
            i += 1
          true
      case _ => false

  override def hashCode(): Int =
    var h = 1
    val len = values.length
    var i = 0
    while i < len do
      h += (if values(i) != null then values(i).hashCode() else 0)
      i += 1
    h
