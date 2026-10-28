package org.orbeon.msv.reader.xmlschema

import org.xml.sax.Locator
import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.ReferenceContainer
import org.orbeon.msv.grammar.xmlschema.{AttributeWildcard, ComplexTypeExp}
import org.orbeon.msv.reader.State
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse &lt;complexType&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ComplexTypeDeclState extends RedefinableDeclState with AnyAttributeOwner:

  /** ComplexType object that we are now constructing. */
  protected var decl: ComplexTypeExp = null

  protected def getContainer(): ReferenceContainer =
    this.reader.asInstanceOf[XMLSchemaReader].currentSchema.complexTypes

  override protected def startSelf(): Unit =
    super.startSelf()

    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    val name = startTag.getAttribute("name")
    if name == null then
      if isGlobal() then
        reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "complexType", "name")
      decl = new ComplexTypeExp(reader.currentSchema, null)
    else
      if isRedefine() then
        // in redefine mode, use temporary object.
        // parsed complexType will be copied into the original one.
        decl = new ComplexTypeExp(reader.currentSchema, name)
      else
        decl = reader.currentSchema.complexTypes.getOrCreate(name)
        // MSV has pre-defined types in xsd namespace (such as xs:anyType)
        // this causes a problem when we are parsing schema4schema.
        // to avoid this problem, we won't issue this error when we are
        // parsing schema4schema.
        if decl.body.exp != null && (reader.currentSchema ne reader.xsdSchema) then
          reader.reportError(
            Array[Locator](this.location, reader.getDeclaredLocationOf(decl)),
            XMLSchemaReader.ERR_DUPLICATE_COMPLEXTYPE_DEFINITION,
            Array[AnyRef](name)
          )

    // set the final attribute to ComplexTypeExp.
    decl.finalValue = parseFinalValue("final", reader.finalDefault)
    decl.block = parseFinalValue("block", reader.blockDefault)

  /** Parses the value of the block/final attribute. */
  private def parseFinalValue(attName: String, defaultValue: String): Int =
    var r = 0
    var value = startTag.getAttribute(attName)
    if value == null then value = defaultValue
    if value != null then
      if value.indexOf("#all") >= 0 then
        r |= ComplexTypeExp.RESTRICTION | ComplexTypeExp.EXTENSION
      if value.indexOf("extension") >= 0 then
        r |= ComplexTypeExp.EXTENSION
      if value.indexOf("restriction") >= 0 then
        r |= ComplexTypeExp.RESTRICTION
    r

  def setAttributeWildcard(local: AttributeWildcard): Unit =
    decl.wildcard = local

  protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    // simpleContent, ComplexContent, group, all, choice, and sequence
    // are allowed only when we haven't seen type definition.
    if tag.localName == "simpleContent" then
      return reader.sfactory.simpleContent(this, tag, decl)
    if tag.localName == "complexContent" then
      return reader.sfactory.complexContent(this, tag, decl)

    val s = reader.createModelGroupState(this, tag)
    if s != null then return s

    if super.exp == null then
      // no content model was given.
      // I couldn't "decipher" what should we do in this case.
      // I assume "empty" just because it's most likely.
      exp = Expression.epsilon

    // attribute, attributeGroup, and anyAttribute can be specified
    // after content model is given.
    reader.createAttributeState(this, tag)

  protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression =
    if halfCastedExpression == null then
      newChildExpression // the first one
    else
      // only the first one contains element.
      // the rest consists of attributes.
      // so this order of parameters is fine.
      reader.pool.createSequence(newChildExpression, halfCastedExpression)

  override protected def defaultExpression(): Expression =
    // if no content model is given, then this complex type is empty
    Expression.epsilon

  override protected def annealExpression(contentTypeParam: Expression): Expression =
    var contentType = contentTypeParam
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    val abstract_ = startTag.getAttribute("abstract")
    if "false" == abstract_ || abstract_ == null then
      // allow the content model to directly appear as this type.
      decl.setAbstract(false)
    else
      decl.setAbstract(true)
      if "true" != abstract_ then
        reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "abstract", abstract_)

    val mixed = startTag.getAttribute("mixed")
    if "true" == mixed then
      contentType = reader.pool.createMixed(contentType)
    else if mixed != null && "false" != mixed then
      reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "mixed", mixed)

    decl.body.exp = contentType

    if isRedefine() then
      // copy new definition back into the original definition.
      oldDecl.redefine(decl)
      decl = oldDecl.asInstanceOf[ComplexTypeExp]

    reader.setDeclaredLocationOf(decl)
    reader.setDeclaredLocationOf(decl.body)
    decl
