package org.orbeon.msv.verifier.regexp.xmlschema

import org.orbeon.msv.datatype.xsd.{DatatypeFactory, QnameType}
import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.{ElementExp, Expression}
import org.orbeon.msv.grammar.xmlschema.{ComplexTypeExp, ElementDeclExp, SimpleTypeExp, XMLSchemaSchema, XMLSchemaTypeExp}
import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.orbeon.msv.util.{StartTagInfo, StringRef}
import org.orbeon.msv.verifier.Acceptor
import org.orbeon.msv.verifier.regexp.{AttributeToken, SimpleAcceptor}

/**
 * Acceptor implementation for XSREDocDecl.
 *
 * <p>
 * This class should be considered as a "quick-hack" to
 * better accomodate W3C XML Schema.
 */
class XSAcceptor(
  private val _docDecl: XSREDocDecl,
  combined: Expression,
  owner: ElementExp,
  continuation: Expression
) extends SimpleAcceptor(_docDecl, combined, owner, continuation):

  override def createClone(): Acceptor =
    new XSAcceptor(_docDecl, getExpression, owner, continuation)

  /**
   * creates an XSAcceptor instead of default acceptor implementations.
   */
  override protected def createAcceptor(
    combined: Expression,
    continuation: Expression,
    primitives: Array[ElementExp],
    numPrimitives: Int
  ): Acceptor =
    if primitives == null || numPrimitives <= 1 then
      new XSAcceptor(
        docDecl.asInstanceOf[XSREDocDecl],
        combined,
        if primitives == null then null else primitives(0),
        continuation
      )
    else
      // we don't want to use complex acceptor because it doesn't implement
      // XML Schema semantics.
      new XSAcceptor(
        docDecl.asInstanceOf[XSREDocDecl],
        primitives(0).contentModel.getExpandedExp(docDecl.pool),
        primitives(0),
        null
      )

  override protected def onAttribute(token: AttributeToken, refErr: StringRef): Boolean =
    // xsi:*** attribute is ignored.
    if token.namespaceURI == XSAcceptor.XSINamespace then
      token.`match`(_docDecl.xsiAttExp)
      true
    else
      super.onAttribute(token, refErr)

  override def createChildAcceptor(sti: StartTagInfo, refErr: StringRef): Acceptor =
    val `type` = sti.getAttribute(XSAcceptor.XSINamespace, "type")
    val nil = sti.getAttribute(XSAcceptor.XSINamespace, "nil")

    if `type` == null && nil == null then
      return super.createChildAcceptor(sti, refErr)

    _docDecl.getCCCEC.get(getExpression, sti, true)

    _docDecl.getCCCEC.numMatchedElements() match
      case 0 =>
        return super.createChildAcceptor(sti, refErr)
      case 1 =>
      case _ =>
        return super.createChildAcceptor(sti, refErr)

    val element = _docDecl.getCCCEC.getMatchedElements()(0)

    if !element.isInstanceOf[ElementDeclExp.XSElementExp] then
      return super.createChildAcceptor(sti, refErr)

    val xe = element.asInstanceOf[ElementDeclExp.XSElementExp]

    if nil != null then
      if !xe.parent.isNillable then
        if refErr == null then return null
        refErr.str = _docDecl.localizeMessage(XSREDocDecl.ERR_NON_NILLABLE_ELEMENT, sti.qName)
        return new XSAcceptor(_docDecl, Expression.epsilon, xe, null)

      if nil.trim == "true" then
        if Debug.debug then
          println("xsi:nil is found")
        return new XSAcceptor(_docDecl, Expression.epsilon, xe, null)

    if `type` == null then
      return super.createChildAcceptor(sti, refErr)

    val typeName = QnameType.theInstance.createJavaObject(`type`, sti.context).asInstanceOf[Array[String]]
    if typeName == null then
      return onTypeResolutionFailure(sti, `type`, refErr)

    var contentModel: Expression = null

    if typeName(0) == XSAcceptor.XMLSchemaNamespace then
      try
        contentModel = _docDecl.grammar.getPool.createData(
          DatatypeFactory.getTypeByName(typeName(1))
        )
      catch
        case e: DatatypeException =>
          return onTypeResolutionFailure(sti, `type`, refErr)
    else
      val schema = _docDecl.grammar.getByNamespace(typeName(0))
      if schema == null then
        return onTypeResolutionFailure(sti, `type`, refErr)

      val currentType = xe.parent.getTypeDefinition
      val cexp = schema.complexTypes.get(typeName(1))
      if cexp != null then
        if cexp.isDerivedTypeOf(currentType, xe.parent.block | currentType.getBlock) then
          contentModel = cexp
        else
          return onNotSubstitutableType(sti, `type`, refErr)
      else
        val sexp = schema.simpleTypes.get(typeName(1))
        if sexp == null then
          return onTypeResolutionFailure(sti, `type`, refErr)

        if !currentType.isInstanceOf[SimpleTypeExp] then
          return onNotSubstitutableType(sti, `type`, refErr)

        val curT = currentType.asInstanceOf[SimpleTypeExp]
        if sexp.getDatatype.isDerivedTypeOf(curT.getDatatype, !xe.parent.isRestrictionBlocked) then
          contentModel = sexp
        else
          return onNotSubstitutableType(sti, `type`, refErr)

    new XSAcceptor(_docDecl, contentModel, xe, null)

  private def onNotSubstitutableType(sti: StartTagInfo, `type`: String, refErr: StringRef): Acceptor =
    if refErr == null then return null
    refErr.str = _docDecl.localizeMessage(XSREDocDecl.ERR_NOT_SUBSTITUTABLE_TYPE, `type`)
    super.createChildAcceptor(sti, refErr)

  private def onTypeResolutionFailure(sti: StartTagInfo, `type`: String, refErr: StringRef): Acceptor =
    if refErr == null then return null
    refErr.str = _docDecl.localizeMessage(XSREDocDecl.ERR_UNDEFINED_TYPE, `type`)
    super.createChildAcceptor(sti, refErr)

object XSAcceptor:
  val XSINamespace: String       = "http://www.w3.org/2001/XMLSchema-instance"
  val XMLSchemaNamespace: String = "http://www.w3.org/2001/XMLSchema"
