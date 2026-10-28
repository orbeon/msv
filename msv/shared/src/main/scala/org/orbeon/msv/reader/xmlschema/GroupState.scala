package org.orbeon.msv.reader.xmlschema

import org.xml.sax.Locator
import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.ReferenceContainer
import org.orbeon.msv.grammar.xmlschema.GroupDeclExp
import org.orbeon.msv.grammar.xmlschema.XMLSchemaSchema
import org.orbeon.msv.reader.State
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse &lt;group&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class GroupState extends RedefinableDeclState:

  protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    reader.createModelGroupState(this, tag)

  protected def getContainer(): ReferenceContainer =
    this.reader.asInstanceOf[XMLSchemaReader].currentSchema.groupDecls

  protected def initialExpression(): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if startTag.containsAttribute("ref") then
      val exp = reader.resolveQNameRef(
        startTag,
        "ref",
        new XMLSchemaReader.RefResolver:
          def get(g: XMLSchemaSchema): ReferenceContainer = g.groupDecls
      )
      if exp == null then Expression.epsilon // couldn't resolve QName.
      else exp
    else
      null // use null to indicate that there is no child expression.

  protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression =
    if halfCastedExpression != null then
      reader.reportError(XMLSchemaReader.ERR_MORE_THAN_ONE_CHILD_EXPRESSION)
      halfCastedExpression
    else
      newChildExpression // the first one.

  override protected def annealExpression(contentType: Expression): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if !isGlobal() then
      return contentType

    // this <group> element is global.
    // register this as a global group definition.
    val name = startTag.getAttribute("name")
    if name == null then
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "group", "name")
      return Expression.nullSet

    if contentType == null then
      reader.reportError(XMLSchemaReader.ERR_MISSING_CHILD_EXPRESSION)
      return Expression.nullSet

    val decl: GroupDeclExp =
      if isRedefine() then
        super.oldDecl.asInstanceOf[GroupDeclExp]
      else
        val created = reader.currentSchema.groupDecls.getOrCreate(name)
        if created.exp != null then
          reader.reportError(
            Array[Locator](this.location, reader.getDeclaredLocationOf(created)),
            XMLSchemaReader.ERR_DUPLICATE_GROUP_DEFINITION,
            Array[AnyRef](name)
          )
        created

    reader.setDeclaredLocationOf(decl)
    decl.exp = contentType
    decl
