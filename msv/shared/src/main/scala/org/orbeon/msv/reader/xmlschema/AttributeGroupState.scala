package org.orbeon.msv.reader.xmlschema

import org.xml.sax.Locator
import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.ReferenceContainer
import org.orbeon.msv.grammar.xmlschema.AttributeGroupExp
import org.orbeon.msv.grammar.xmlschema.AttributeWildcard
import org.orbeon.msv.grammar.xmlschema.XMLSchemaSchema
import org.orbeon.msv.reader.State
import org.orbeon.msv.util.StartTagInfo

/**
 * Parses &lt;attributeGroup /&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeGroupState extends RedefinableDeclState with AnyAttributeOwner:

  override protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    reader.createAttributeState(this, tag)

  override protected def getContainer(): ReferenceContainer =
    this.reader.asInstanceOf[XMLSchemaReader].currentSchema.attributeGroups

  override protected def initialExpression(): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    val refQName = startTag.getAttribute("ref")
    if refQName == null then
      // child expressions are expected. (although it's optional)
      Expression.epsilon
    else
      val exp = reader.resolveQNameRef(
        startTag,
        "ref",
        new XMLSchemaReader.RefResolver:
          def get(g: XMLSchemaSchema): ReferenceContainer = g.attributeGroups
      )
      if exp == null then Expression.epsilon // couldn't resolve QName.
      else exp

  private var wildcard: AttributeWildcard = null

  def setAttributeWildcard(local: AttributeWildcard): Unit =
    this.wildcard = local

  override protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression =
    if startTag.containsAttribute("ref") then
      reader.reportError(XMLSchemaReader.ERR_MORE_THAN_ONE_CHILD_EXPRESSION)
    if halfCastedExpression == null then
      newChildExpression // the first one.
    else
      reader.pool.createSequence(newChildExpression, halfCastedExpression)

  override protected def annealExpression(contentType: Expression): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if !isGlobal() then
      return contentType

    // if this is a global declaration register it.
    val name = startTag.getAttribute("name")
    if name == null then
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "attributeGroup", "name")
      return Expression.epsilon

    val exp: AttributeGroupExp =
      if isRedefine() then
        oldDecl.asInstanceOf[AttributeGroupExp]
      else
        val created = reader.currentSchema.attributeGroups.getOrCreate(name)
        if created.exp != null then
          reader.reportError(
            Array[Locator](this.location, reader.getDeclaredLocationOf(created)),
            XMLSchemaReader.ERR_DUPLICATE_ATTRIBUTE_GROUP_DEFINITION,
            Array[AnyRef](name)
          )
        created

    reader.setDeclaredLocationOf(exp)
    exp.exp = contentType
    exp.wildcard = this.wildcard
    exp
