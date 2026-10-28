package org.orbeon.msv.grammar.xmlschema

/**
 * attribute group declaration.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeGroupExp(typeLocalName: String) extends RedefinableExp(typeLocalName) with AttWildcardExp:

  var wildcard: AttributeWildcard = scala.compiletime.uninitialized

  def getAttributeWildcard: AttributeWildcard = wildcard
  def setAttributeWildcard(local: AttributeWildcard): Unit = wildcard = local

  def getClone(): RedefinableExp =
    val exp = new AttributeGroupExp(this.name)
    exp.redefine(this)
    exp

  override def redefine(_rhs: RedefinableExp): Unit =
    super.redefine(_rhs)
    val rhs = _rhs.asInstanceOf[AttributeGroupExp]
    if rhs.wildcard == null then wildcard = null
    else wildcard = rhs.wildcard.copy()
