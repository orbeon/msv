package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.xmlschema.SimpleTypeExp
import org.orbeon.msv.reader.{ExpressionOwner, SimpleState, State}
import org.orbeon.msv.reader.datatype.xsd.{XSDatatypeExp, XSTypeOwner}
import org.orbeon.msv.util.StartTagInfo

/**
 * State that parses global declarations.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class GlobalDeclState extends SimpleState with ExpressionOwner with XSTypeOwner:

  protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    tag.localName match
      case "include"        => reader.sfactory.include(this, tag)
      case "import"         => reader.sfactory.import_(this, tag)
      case "redefine"       => reader.sfactory.redefine(this, tag)
      case "simpleType"     => reader.sfactory.simpleType(this, tag)
      case "complexType"    => reader.sfactory.complexTypeDecl(this, tag)
      case "group"          => reader.sfactory.group(this, tag)
      case "attributeGroup" => reader.sfactory.attributeGroup(this, tag)
      case "element"        => reader.sfactory.elementDecl(this, tag)
      case "attribute"      => reader.sfactory.attribute(this, tag)
      case "notation"       => reader.sfactory.notation(this, tag)
      case _                => null

  def getTargetNamespaceUri(): String =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    reader.currentSchema.targetNamespace

  def onEndChild(exp: Expression): Unit = ()

  def onEndChild(dataType: XSDatatypeExp): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val typeName = reader.getCurrentState.getStartTag.getAttribute("name")

    if typeName == null then
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "simpleType", "name")
      return

    val exp: SimpleTypeExp = reader.currentSchema.simpleTypes.getOrCreate(typeName)
    if exp.getType != null then
      reader.reportError(XMLSchemaReader.ERR_DATATYPE_ALREADY_DEFINED, typeName)
      return

    exp.set(dataType)
    reader.setDeclaredLocationOf(exp)
