/*
 * @(#)$Id: BuiltinAtomicType.java,v 1.5 2003/06/09 20:49:20 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.DatatypeException

import java.io.ObjectStreamException

/**
 * Base class for atomic built-in types; those types which can be used by itself
 * (int, uriReference, string, etc.).
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class BuiltinAtomicType(
  typeName: String,
  whiteSpace: WhiteSpaceProcessor
) extends ConcreteType(XSDatatype.XMLSCHEMA_NSURI, typeName, whiteSpace):

  def this(typeName: String) =
    this(typeName, WhiteSpaceProcessor.theCollapse)

  final def getVariety(): Int = XSDatatype.VARIETY_ATOMIC

  final def displayName(): String = getName

  @throws(classOf[ObjectStreamException])
  protected def readResolve(): AnyRef =
    val name = getName
    if name != null then
      try
        return DatatypeFactory.getTypeByName(name)
      catch
        case _: DatatypeException => ()
    this
