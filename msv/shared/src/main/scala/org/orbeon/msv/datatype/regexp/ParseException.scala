/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2002 The Apache Software Foundation.  All rights 
 * reserved.
 */

package org.orbeon.msv.datatype.regexp

/**
 * @author TAMURA Kent &lt;kent@trl.ibm.co.jp&gt;
 * @version ParseException.java,v 1.3 2002/08/09 15:18:17 neilg Exp
 */
class ParseException(mes: String, val location: Int) extends RuntimeException(mes) {

  def getLocation: Int = location
}
