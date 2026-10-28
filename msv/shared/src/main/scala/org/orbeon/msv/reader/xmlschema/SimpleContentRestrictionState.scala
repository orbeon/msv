package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.orbeon.msv.datatype.xsd.{StringType, XSDatatype}
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.util.ExpressionWalker
import org.orbeon.msv.grammar.xmlschema.{AttributeWildcard, ComplexTypeExp, SimpleTypeExp, XMLSchemaSchema}
import org.orbeon.msv.reader.{GrammarReader, SequenceState, State}
import org.orbeon.msv.reader.datatype.xsd.{FacetStateParent, XSDatatypeExp, XSTypeIncubator, XSTypeOwner}
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse restriction/extension element as a child of &lt;simpleContent&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SimpleContentRestrictionState(protected val parentDecl: ComplexTypeExp)
  extends SequenceState with FacetStateParent with XSTypeOwner with AnyAttributeOwner:

  def setAttributeWildcard(local: AttributeWildcard): Unit =
    parentDecl.wildcard = local

  /** Used to restrict simpleType. */
  protected var incubator: XSTypeIncubator = null

  def getIncubator: XSTypeIncubator =
    if incubator == null then
      val reader = this.reader.asInstanceOf[XMLSchemaReader]

      if baseTypeName == null then
        incubator = new XSDatatypeExp(StringType.theInstance, reader.pool).createIncubator()
      else
        if reader.isSchemaNamespace(baseTypeName(0)) then
          val dt = reader.resolveBuiltinDataType(baseTypeName(1))
          if dt != null then
            incubator = new XSDatatypeExp(dt, reader.pool).createIncubator()

        if incubator == null then
          incubator = new XSDatatypeExp(
            baseTypeName(0),
            baseTypeName(1),
            reader,
            new BaseContentTypeRenderer()
          ).createIncubator()

    incubator

  private class BaseContentTypeRenderer extends XSDatatypeExp.Renderer:
    def render(context: XSDatatypeExp.RenderingContext): XSDatatype =
      val reader = SimpleContentRestrictionState.this.reader.asInstanceOf[XMLSchemaReader]

      val sexp = baseSchema.simpleTypes.get(baseTypeName(1))
      if sexp != null then
        return sexp.getType.getType(context)

      val cexp = baseSchema.complexTypes.get(baseTypeName(1))
      if cexp != null then
        val dexp = new Array[XSDatatypeExp](1)
        cexp.body.visit(new ExpressionWalker:
          override def onAttribute(exp: AttributeExp): Unit = ()
          override def onRef(exp: ReferenceExp): Unit =
            if exp.isInstanceOf[XSDatatypeExp] then
              dexp(0) = exp.asInstanceOf[XSDatatypeExp]
              return
            super.onRef(exp)
        )
        if dexp(0) == null then
          reader.reportError(XMLSchemaReader.ERR_INVALID_BASETYPE_FOR_SIMPLECONTENT, base)
          return StringType.theInstance
        return dexp(0).getType(context)

      StringType.theInstance

  def getTargetNamespaceUri: String =
    this.reader.asInstanceOf[XMLSchemaReader].currentSchema.targetNamespace

  def onEndChild(child: XSDatatypeExp): Unit =
    if incubator != null then
      throw new Error()

    incubator = child.createIncubator()

  override protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if incubator == null && tag.localName == "simpleType" then
      return reader.sfactory.simpleType(this, tag)

    val s = reader.createAttributeState(this, tag)
    if s != null then return s

    reader.createFacetState(this, tag) // facets

  override protected def initialExpression(): Expression =
    Expression.epsilon

  /** Value of the base attribute. null in case of an error. */
  private var base: String = null

  /**
   * Namespace URI and local name of the base attribute.
   * null in case of an error.
   */
  private var baseTypeName: Array[String] = null

  /**
   * XMLSchemaSchema object of the base type.
   * We should have this reference so that
   * XMLSchemaReader can detect an error if this schema is undefined.
   *
   * Null in case of an error.
   */
  private var baseSchema: XMLSchemaSchema = null

  override protected def startSelf(): Unit =
    super.startSelf()
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    base = startTag.getAttribute("base")
    if base == null then
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, startTag.localName, "base")
      return

    baseTypeName = reader.splitQName(base)
    if baseTypeName == null then
      reader.reportError(XMLSchemaReader.ERR_UNDECLARED_PREFIX, base)
      return

    baseSchema = reader.grammar.getByNamespace(baseTypeName(0)).asInstanceOf[XMLSchemaSchema]

  override protected def annealExpression(exp: Expression): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    parentDecl.derivationMethod = ComplexTypeExp.RESTRICTION

    val annealedSeq =
      try
        val p = getIncubator.derive(null, null)
        reader.pool.createSequence(
          super.annealExpression(exp),
          p
        )
      catch
        case e: DatatypeException =>
          reader.reportError(e, XMLSchemaReader.ERR_BAD_TYPE, e.getMessage)
          return Expression.nullSet

    // we need a special handling for built-in types
    if reader.isSchemaNamespace(baseTypeName(0)) then
      val dt = reader.resolveBuiltinDataType(baseTypeName(1))
      if dt != null then
        parentDecl.simpleBaseType = new XSDatatypeExp(dt, reader.pool)
        return annealedSeq

    // in other cases, we don't know the base type yet.
    // let back patch it
    reader.addBackPatchJob(new GrammarReader.BackPatch:
      def getOwnerState: State = SimpleContentRestrictionState.this
      def patch(): Unit =
        val sexp = baseSchema.simpleTypes.get(baseTypeName(1))
        if sexp != null then
          parentDecl.simpleBaseType = sexp.getType
          State._assert(parentDecl.simpleBaseType != null)
          return

        val cexp = baseSchema.complexTypes.get(baseTypeName(1))
        if cexp != null then
          parentDecl.complexBaseType = cexp
          return

        reader.reportError(XMLSchemaReader.ERR_UNDEFINED_COMPLEX_OR_SIMPLE_TYPE, base)
    )

    annealedSeq
