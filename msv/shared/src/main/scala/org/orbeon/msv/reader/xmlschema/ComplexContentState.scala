package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.xmlschema.ComplexTypeExp
import org.orbeon.msv.reader.ExpressionWithChildState
import org.orbeon.msv.reader.State
import org.orbeon.msv.util.StartTagInfo

/**
 * Parses &lt;complexContent&gt; element.
 *
 * The expression created by this state is used as ComplexTypeExp.self field.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ComplexContentState(protected val parentDecl: ComplexTypeExp) extends ExpressionWithChildState:

  override protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if this.exp != null then
      // we have already parsed restriction/extension.
      return null

    if tag.localName == "restriction" then
      reader.sfactory.complexRst(this, tag, parentDecl)
    else if tag.localName == "extension" then
      reader.sfactory.complexExt(this, tag, parentDecl)
    else
      super.createChildState(tag)

  override protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression =
    if halfCastedExpression != null then
      throw new Error()

    newChildExpression

  override protected def annealExpression(contentType: Expression): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val mixed = startTag.getAttribute("mixed")

    var content = contentType
    if "true" == mixed then
      content = reader.pool.createMixed(content)
    else if mixed != null && "false" != mixed then
      reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "mixed", mixed)

    content
