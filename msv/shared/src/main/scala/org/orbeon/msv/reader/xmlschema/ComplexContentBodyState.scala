package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.xmlschema.{AttributeWildcard, ComplexTypeExp}
import org.orbeon.msv.reader.{SequenceState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse restriction/extension element as a child of complexContent element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ComplexContentBodyState(
  protected val parentDecl: ComplexTypeExp,
  protected val extension: Boolean
) extends SequenceState with AnyAttributeOwner:

  protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if super.exp == Expression.epsilon then
      // model group must be the first expression child.
      val s = reader.createModelGroupState(this, tag)
      if s != null then return s

    // attribute, attributeGroup, and anyAttribute can be specified
    // after content model is given.
    reader.createAttributeState(this, tag)

  def setAttributeWildcard(local: AttributeWildcard): Unit =
    parentDecl.wildcard = local

  override protected def initialExpression(): Expression =
    Expression.epsilon

  override protected def annealExpression(expParam: Expression): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val exp = super.annealExpression(expParam)

    val refQName = startTag.getAttribute("base")
    if refQName == null then
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, startTag.qName, "base")
      return exp

    val r = reader.splitQName(refQName)
    if r == null then
      reader.reportError(XMLSchemaReader.ERR_UNDECLARED_PREFIX, refQName)
      return exp

    if reader.isSchemaNamespace(r(0)) && r(1) == "anyType" then
      return exp

    val baseType = reader.getOrCreateSchema(r(0)).complexTypes.getOrCreate(r(1))
    if baseType == null then return exp

    reader.backwardReference.memorizeLink(baseType)

    parentDecl.derivationMethod = if extension then ComplexTypeExp.EXTENSION else ComplexTypeExp.RESTRICTION
    parentDecl.complexBaseType = baseType

    combineToBaseType(baseType, exp)

  /** Combines the base type content model and this content model. */
  protected def combineToBaseType(baseType: ComplexTypeExp, addedExp: Expression): Expression =
    if extension then
      this.reader.asInstanceOf[XMLSchemaReader].pool.createSequence(baseType.body, addedExp)
    else
      addedExp
