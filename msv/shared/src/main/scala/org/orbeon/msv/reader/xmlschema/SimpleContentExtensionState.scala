package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.datatype.xsd.XSDatatype
import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.ReferenceExp
import org.orbeon.msv.grammar.xmlschema.{AttributeWildcard, ComplexTypeExp, SimpleTypeExp, XMLSchemaSchema}
import org.orbeon.msv.reader.{GrammarReader, SequenceState, State}
import org.orbeon.msv.reader.datatype.xsd.XSDatatypeExp
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse extension element as a child of &lt;simpleContent&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SimpleContentExtensionState(protected val parentDecl: ComplexTypeExp)
  extends SequenceState with AnyAttributeOwner:

  def setAttributeWildcard(local: AttributeWildcard): Unit =
    parentDecl.wildcard = local

  override protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    reader.createAttributeState(this, tag)

  override protected def initialExpression(): Expression =
    // without this statement,
    // <extension> without any attribute will be prohibited.
    Expression.epsilon

  override protected def annealExpression(exp: Expression): Expression =
    parentDecl.derivationMethod = ComplexTypeExp.EXTENSION
    reader.pool.createSequence(
      super.annealExpression(exp),
      getBody()
    )

  /**
   * Gets the expression for the base type.
   */
  private def getBody(): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    val base = startTag.getAttribute("base")
    if base == null then
      // in extension, base attribute is mandatory.
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, startTag.localName, "base")
      return Expression.nullSet

    val baseTypeName = reader.splitQName(base)
    if baseTypeName == null then
      reader.reportError(XMLSchemaReader.ERR_UNDECLARED_PREFIX, base)
      return Expression.nullSet

    // we need a special handling for built-in types
    if reader.isSchemaNamespace(baseTypeName(0)) then
      val dt = reader.resolveBuiltinDataType(baseTypeName(1))
      if dt != null then
        val dtexp = new XSDatatypeExp(dt, reader.pool)
        parentDecl.simpleBaseType = dtexp
        return dtexp

      // maybe we are parsing the schema for schema.
      // consult externally specified schema.

    val schema = reader.grammar.getByNamespace(baseTypeName(0)).asInstanceOf[XMLSchemaSchema]

    // we don't know whether it's a complex type or a simple type.
    // so back patch it
    val ref = new ReferenceExp(null)
    reader.addBackPatchJob(new GrammarReader.BackPatch:
      def getOwnerState: State = SimpleContentExtensionState.this
      def patch(): Unit =
        val sexp = schema.simpleTypes.get(baseTypeName(1))
        if sexp != null then
          // we've found the simple type
          ref.exp = sexp
          parentDecl.simpleBaseType = sexp.getType
          State._assert(parentDecl.simpleBaseType != null)
          return

        val cexp = schema.complexTypes.get(baseTypeName(1))
        if cexp != null then
          // we've found the complex type as the base type
          ref.exp = cexp.body
          parentDecl.complexBaseType = cexp
          return

        // there is no base type.
        reader.reportError(XMLSchemaReader.ERR_UNDEFINED_COMPLEX_OR_SIMPLE_TYPE, base)
    )

    ref
