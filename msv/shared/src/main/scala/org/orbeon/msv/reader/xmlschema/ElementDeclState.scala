package org.orbeon.msv.reader.xmlschema

import java.util.{ArrayList as JArrayList, List as JList}
import org.xml.sax.Locator
import org.orbeon.msv.datatype.xsd.BooleanType
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.xmlschema.{ElementDeclExp, IdentityConstraint, XMLSchemaSchema}
import org.orbeon.msv.reader.{ExpressionWithChildState, GrammarReader, State}
import org.orbeon.msv.util.{StartTagInfo, StringPair}

/**
 * Used to parse &lt;element &gt; element without ref attribute.
 *
 * This state uses ExpressionWithChildState to collect content model
 * of this element declaration.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementDeclState extends ExpressionWithChildState:

  override protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    // type declaration is allowed only when we haven't seen type definition.
    if this.exp == null then
      if tag.localName == "simpleType" then return reader.sfactory.simpleType(this, tag)
      if tag.localName == "complexType" then return reader.sfactory.complexTypeDecl(this, tag)

    // unique/key/keyref are ignored.
    if tag.localName == "unique" then return reader.sfactory.unique(this, tag)
    if tag.localName == "key" then return reader.sfactory.key(this, tag)
    if tag.localName == "keyref" then return reader.sfactory.keyref(this, tag)

    null

  override protected def initialExpression(): Expression =
    val typeQName = startTag.getAttribute("type")
    if typeQName == null then null
    else resolveTypeRef(typeQName)

  /**
   * If this element declaration has @type, then this method
   * is called to resolve it.
   */
  protected def resolveTypeRef(typeQName: String): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    val ref = new ReferenceExp(s"elementType($typeQName)")
    val s = reader.splitQName(typeQName)
    if s == null then
      reader.reportError(XMLSchemaReader.ERR_UNDECLARED_PREFIX, typeQName)
      ref.exp = Expression.nullSet
      return ref

    reader.addBackPatchJob(new GrammarReader.BackPatch:
      def getOwnerState: State = ElementDeclState.this
      def patch(): Unit =
        var e: Expression = null

        if reader.isSchemaNamespace(s(0)) then
          e = reader.resolveBuiltinSimpleType(s(1))

        if e == null then
          val g = reader.getOrCreateSchema(s(0))
          e = g.simpleTypes.get(s(1))
          if e == null then e = g.complexTypes.get(s(1))
          if e == null then
            reader.reportError(XMLSchemaReader.ERR_UNDEFINED_ELEMENTTYPE, typeQName)
            e = Expression.nullSet

        ref.exp = e
    )

    ref

  override protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression =
    if halfCastedExpression != null then
      throw new Error()

    newChildExpression

  override protected def defaultExpression(): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    if startTag.containsAttribute("substitutionGroup") then
      reader.reportError(
        XMLSchemaReader.ERR_UNIMPLEMENTED_FEATURE,
        "omitting type attribute in <element> element with substitutionGroup attribute"
      )

    reader.reportWarning(XMLSchemaReader.WRN_IMPLICIT_URTYPE_FOR_ELEMENT, null)
    reader.complexUrType

  override protected def annealExpression(contentTypeParam: Expression): Expression =
    var contentType = contentTypeParam
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    val name = startTag.getAttribute("name")
    if name == null then
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "element", "name")
      return Expression.nullSet

    val targetNamespace =
      if isGlobal() then
        reader.currentSchema.targetNamespace
      else
        reader.resolveNamespaceOfElementDecl(startTag.getAttribute("form"))

    var prefix = ""
    val typeAttr = startTag.getAttribute("type")
    if typeAttr != null then
      val idx = typeAttr.indexOf(':')
      if idx > 0 then
        prefix = typeAttr.substring(0, idx)
    else
      val refAttr = startTag.getAttribute("ref")
      if refAttr != null then
        val idx = refAttr.indexOf(':')
        if idx > 0 then
          prefix = refAttr.substring(0, idx)

    val fixed = startTag.getAttribute("fixed")
    val defaultValue = startTag.getAttribute("default")
    if fixed != null then
      if defaultValue != null then
        reader.reportError(
          Array[Locator](this.location),
          XMLSchemaReader.ERR_FIXED_AND_DEFAULT,
          null
        )
      contentType = reader.pool.createValue(
        org.orbeon.msv.datatype.xsd.TokenType.theInstance,
        new StringPair("", "token"),
        fixed
      )

    val decl: ElementDeclExp =
      if isGlobal() then
        val d = reader.currentSchema.elementDecls.getOrCreate(name)
        if d.getElementExp != null then
          reader.reportError(
            Array[Locator](this.location, reader.getDeclaredLocationOf(d)),
            XMLSchemaReader.ERR_DUPLICATE_ELEMENT_DEFINITION,
            Array[AnyRef](name)
          )
        d
      else
        new ElementDeclExp(reader.currentSchema, null)

    reader.setDeclaredLocationOf(decl)

    val exp = new decl.XSElementExp(
      new SimpleNameClass(targetNamespace, name, prefix),
      contentType,
      defaultValue
    )

    decl.setElementExp(exp)
    exp.identityConstraints.addAll(idcs)

    val nillable = startTag.getAttribute("nillable")
    if nillable != null then
      decl.isNillable = nillable == "true" || nillable == "1"

    val abstract_ = startTag.getAttribute("abstract")
    decl.setAbstract("true" == abstract_ || "1" == abstract_)
    if abstract_ != null && !BooleanType.theInstance.isValid(abstract_, null) then
      reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "abstract", abstract_)

    val substitutionGroupQName = startTag.getAttribute("substitutionGroup")
    if substitutionGroupQName != null then
      val r = reader.splitQName(substitutionGroupQName)
      if r == null then
        reader.reportError(XMLSchemaReader.ERR_UNDECLARED_PREFIX, substitutionGroupQName)
      else
        val head = reader.getOrCreateSchema(r(0)).elementDecls.getOrCreate(r(1))
        decl.substitutionAffiliation = head

    var block = startTag.getAttribute("block")
    if block == null then block = reader.blockDefault
    if block != null then
      if block.indexOf("#all") >= 0 then
        decl.block |= ElementDeclExp.ALL
      if block.indexOf("extension") >= 0 then
        decl.block |= ElementDeclExp.EXTENSION
      if block.indexOf("restriction") >= 0 then
        decl.block |= ElementDeclExp.RESTRICTION
      if block.indexOf("substitution") >= 0 then
        decl.block |= ElementDeclExp.SUBSTITUTION

    var finalValue = startTag.getAttribute("final")
    if finalValue == null then finalValue = reader.finalDefault
    if finalValue != null then
      if finalValue.indexOf("#all") >= 0 then
        decl.finalValue |= ElementDeclExp.ALL
      if finalValue.indexOf("extension") >= 0 then
        decl.finalValue |= ElementDeclExp.EXTENSION
      if finalValue.indexOf("restriction") >= 0 then
        decl.finalValue |= ElementDeclExp.RESTRICTION

    annealDeclaration(decl)

  protected def annealDeclaration(exp: ElementDeclExp): Expression = exp

  def isGlobal(): Boolean =
    parentState.isInstanceOf[GlobalDeclState]

  /** Identity constraints found in this element. */
  protected val idcs: JList[IdentityConstraint] = new JArrayList[IdentityConstraint]()

  /** This method is called when an identity constraint declaration is found. */
  protected[xmlschema] def onIdentityConstraint(idc: IdentityConstraint): Unit =
    idcs.add(idc)
