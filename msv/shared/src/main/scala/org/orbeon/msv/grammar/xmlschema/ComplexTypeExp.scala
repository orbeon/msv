package org.orbeon.msv.grammar.xmlschema

import org.orbeon.msv.datatype.xsd.XSDatatype
import org.orbeon.msv.grammar.{Expression, ReferenceExp}
import org.orbeon.msv.reader.datatype.xsd.XSDatatypeExp

/**
 * ComplexType definition.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ComplexTypeExp(val parent: XMLSchemaSchema, localName: String) extends XMLSchemaTypeExp(localName):

  /** actual content model definition + attribute uses. */
  val body: ReferenceExp = new ReferenceExp(null)

  /** attribute wildcard as an expression. */
  val attWildcard: ReferenceExp = new ReferenceExp(null, Expression.epsilon)

  var wildcard: AttributeWildcard = scala.compiletime.uninitialized

  def getAttributeWildcard: AttributeWildcard = wildcard
  def setAttributeWildcard(local: AttributeWildcard): Unit = wildcard = local

  final def getTargetNamespace: String = parent.targetNamespace

  var complexBaseType: ComplexTypeExp = scala.compiletime.uninitialized
  var simpleBaseType: XSDatatypeExp = scala.compiletime.uninitialized
  var derivationMethod: Int = -1

  setAbstract(false)

  def isAbstract: Boolean = exp eq Expression.nullSet

  def setAbstract(isAbstractVal: Boolean): Unit =
    if isAbstractVal then exp = Expression.nullSet
    else exp = parent.pool.createSequence(body, attWildcard)

  def isDerivedTypeOf(baseType: ComplexTypeExp, constraint: Int): Boolean =
    var derived: ComplexTypeExp = this
    while derived != null do
      if derived eq baseType then return true
      if (derived.derivationMethod & constraint) != 0 then
        return false
      derived = derived.complexBaseType
    false

  def isDerivedTypeOf(baseType: XSDatatype, constraint: Int): Boolean =
    var derived: ComplexTypeExp = this
    while true do
      if derived.complexBaseType == null then
        if derived.simpleBaseType != null then
          return derived.simpleBaseType.getCreatedType().isDerivedTypeOf(
            baseType,
            (constraint & XMLSchemaTypeExp.RESTRICTION) == 0
          )
        else
          return false

      if (derived.derivationMethod & constraint) != 0 then
        return false

      derived = derived.complexBaseType
    false

  def isDerivedTypeOf(exp: XMLSchemaTypeExp, constraint: Int): Boolean =
    if exp.isInstanceOf[ComplexTypeExp] then
      isDerivedTypeOf(exp.asInstanceOf[ComplexTypeExp], constraint)
    else
      isDerivedTypeOf(exp.asInstanceOf[SimpleTypeExp].getDatatype, constraint)

  var finalValue: Int = 0
  var block: Int = 0

  def getBlock: Int = block

  def getClone(): RedefinableExp =
    val exp = new ComplexTypeExp(parent, this.name)
    exp.redefine(this)
    exp

  override def redefine(_rhs: RedefinableExp): Unit =
    super.redefine(_rhs)
    val rhs = _rhs.asInstanceOf[ComplexTypeExp]
    body.exp = rhs.body.exp
    attWildcard.exp = rhs.attWildcard.exp
    complexBaseType = rhs.complexBaseType
    simpleBaseType = rhs.simpleBaseType
    derivationMethod = rhs.derivationMethod
    finalValue = rhs.finalValue
    block = rhs.block
    if rhs.wildcard == null then wildcard = null
    else wildcard = rhs.wildcard.copy()

    if this.parent ne rhs.parent then throw new IllegalArgumentException()

  override def isDefined: Boolean = body.isDefined

object ComplexTypeExp:
  val RESTRICTION: Int = XMLSchemaTypeExp.RESTRICTION
  val EXTENSION: Int = XMLSchemaTypeExp.EXTENSION
