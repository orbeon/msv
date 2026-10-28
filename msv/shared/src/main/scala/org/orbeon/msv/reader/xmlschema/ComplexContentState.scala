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

  protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if super.exp != null then
      // we have already parsed restriction/extension.
      return null

    if tag.localName == "restriction" then
      reader.sfactory.complexRst(this, tag, parentDecl)
    else if tag.localName == "extension" then
      reader.sfactory.complexExt(this, tag, parentDecl)
    else
      super.createChildState(tag)

  protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression =
    if halfCastedExpression != null then
      throw new Error()

    newChildExpression
