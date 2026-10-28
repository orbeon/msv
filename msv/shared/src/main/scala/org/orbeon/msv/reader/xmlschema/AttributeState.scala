package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.xml.sax.Locator
import org.orbeon.msv.datatype.xsd.TokenType
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.relax.NoneType
import org.orbeon.msv.grammar.xmlschema.AttributeDeclExp
import org.orbeon.msv.grammar.xmlschema.XMLSchemaSchema
import org.orbeon.msv.reader.ExpressionWithChildState
import org.orbeon.msv.reader.State
import org.orbeon.msv.reader.datatype.xsd.{XSDatatypeExp, XSTypeIncubator, XSTypeOwner}
import org.orbeon.msv.util.StartTagInfo
import org.orbeon.msv.util.StringPair

/**
 * Used to parse &lt;attribute &gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeState extends ExpressionWithChildState with XSTypeOwner:

  override protected def createChildState(tag: StartTagInfo): State =
    if tag.localName == "simpleType" then
      this.reader.asInstanceOf[XMLSchemaReader].sfactory.simpleType(this, tag)
    else
      super.createChildState(tag)

  override protected def initialExpression(): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if startTag.containsAttribute("ref") then
      if isGlobal() then
        reader.reportError(XMLSchemaReader.ERR_DISALLOWED_ATTRIBUTE, startTag.qName, "ref")
        return Expression.epsilon

      // this tag has @ref.
      val exp = reader.resolveQNameRef(
        startTag,
        "ref",
        new XMLSchemaReader.RefResolver:
          def get(g: XMLSchemaSchema): ReferenceContainer = g.attributeDecls
      )
      if exp == null then Expression.epsilon // couldn't resolve QName.
      else exp
    else
      val typeAttr = startTag.getAttribute("type")
      if typeAttr == null then
        // return null to indicate that no type definition is given.
        null
      else
        // if <attribute> element has @type, then it shall be used as content type.
        reader.resolveXSDatatype(typeAttr)

  override protected def defaultExpression(): Expression =
    // if no type definition is given, assume ur-type.
    Expression.anyString

  override protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression =
    if halfCastedExpression != null then
      // only one child is allowed.
      // recover by ignoring previously found child expressions.
      reader.reportError(XMLSchemaReader.ERR_MORE_THAN_ONE_CHILD_EXPRESSION)

    newChildExpression

  override protected def annealExpression(contentTypeParam: Expression): Expression =
    var contentType = contentTypeParam
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val fixed = startTag.getAttribute("fixed")
    val name = startTag.getAttribute("name")
    val use = startTag.getAttribute("use")
    val defaultValue = startTag.getAttribute("default")

    var exp: Expression = null

    if startTag.containsAttribute("ref") then
      if fixed != null then
        reader.reportWarning(
          XMLSchemaReader.ERR_UNIMPLEMENTED_FEATURE,
          "<attribute> element with both 'ref' and 'fixed' attributes"
        )
      exp = contentType
    else
      val targetNamespace =
        if name == null then
          reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "attribute", "name")
          return Expression.nullSet
        else if isGlobal() then
          reader.currentSchema.targetNamespace
        else
          // in local attribute declaration,
          // targetNamespace is affected by @form and schema's @attributeFormDefault.
          reader.resolveNamespaceOfAttributeDecl(startTag.getAttribute("form"))

      if fixed != null then
        if defaultValue != null then
          reader.reportError(
            Array[Locator](this.location),
            XMLSchemaReader.ERR_FIXED_AND_DEFAULT,
            null
          )
        contentType match
          case baseType: XSDatatypeExp =>
            try
              val inc = baseType.createIncubator()
              inc.addFacet("enumeration", fixed, false, reader)
              contentType = inc.derive(null, null)
            catch
              case e: DatatypeException =>
                reader.reportError(e, XMLSchemaReader.ERR_BAD_TYPE, e.getMessage)
                return Expression.nullSet
          case _ =>
            contentType = reader.pool.createValue(
              TokenType.theInstance,
              new StringPair("", "token"),
              fixed
            )

      if "prohibited" == use then
        // use='prohibited' is implemented through NoneType
        contentType = reader.pool.createData(NoneType.theInstance)

      exp = createAttribute(new SimpleNameClass(targetNamespace, name), contentType, defaultValue)

    if isGlobal() then
      // register this expression as a global attribute declaration.
      val decl = reader.currentSchema.attributeDecls.getOrCreate(name)
      if decl.exp != null then
        reader.reportError(
          Array[Locator](this.location, reader.getDeclaredLocationOf(decl)),
          XMLSchemaReader.ERR_DUPLICATE_ATTRIBUTE_DEFINITION,
          Array[AnyRef](name)
        )
      reader.setDeclaredLocationOf(decl)
      exp match
        case attExp: AttributeExp =>
          decl.set(attExp)
        case _ =>
          if !reader.controller.hadError then throw new Error()
    else
      // handle @use
      if "optional" == use || use == null || "prohibited" == use then
        exp = reader.pool.createOptional(exp)
      else if "required" != use then
        reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "use", use)

    exp

  protected def createAttribute(nc: NameClass, exp: Expression, defaultValue: String = null): Expression =
    reader.pool.createAttribute(nc, exp, defaultValue)

  def getTargetNamespaceUri: String =
    this.reader.asInstanceOf[XMLSchemaReader].currentSchema.targetNamespace

  def onEndChild(dataType: XSDatatypeExp): Unit =
    onEndChild(dataType.asInstanceOf[Expression])

  protected def isGlobal(): Boolean =
    parentState.isInstanceOf[GlobalDeclState]
