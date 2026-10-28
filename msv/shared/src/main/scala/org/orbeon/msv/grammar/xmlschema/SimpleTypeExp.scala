package org.orbeon.msv.grammar.xmlschema

import org.orbeon.msv.datatype.xsd.XSDatatype
import org.orbeon.msv.reader.datatype.xsd.XSDatatypeExp

/**
 * Simple type declaration.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SimpleTypeExp(typeName: String) extends XMLSchemaTypeExp(typeName):

  protected var `type`: XSDatatypeExp = scala.compiletime.uninitialized

  def set(exp: XSDatatypeExp): Unit =
    this.exp = exp
    this.`type` = exp

  def getType: XSDatatypeExp = `type`

  def getDatatype: XSDatatype = `type`.getCreatedType()

  def getBlock: Int = 0

  def getClone(): RedefinableExp =
    val exp = new SimpleTypeExp(this.name)
    exp.redefine(this)
    exp

  override def redefine(_rhs: RedefinableExp): Unit =
    super.redefine(_rhs)
    val rhs = _rhs.asInstanceOf[SimpleTypeExp]
    if `type` == null then
      `type` = rhs.getType.getClone()
    else
      if rhs.getType == null then throw new InternalError()
      `type`.redefine(rhs.getType)
