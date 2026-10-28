package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.xmlschema.ComplexTypeExp
import org.orbeon.msv.reader.ExpressionWithChildState
import org.orbeon.msv.reader.State
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse &lt;simpleContent&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SimpleContentState(protected val parentDecl: ComplexTypeExp) extends ExpressionWithChildState:

  protected def createChildState(tag: StartTagInfo): State =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if super.exp != null then
      return null

    if tag.localName == "restriction" then
      reader.sfactory.simpleRst(this, tag, parentDecl)
    else if tag.localName == "extension" then
      reader.sfactory.simpleExt(this, tag, parentDecl)
    else
      super.createChildState(tag)

  protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression =
    if halfCastedExpression != null then
      throw new Error()

    newChildExpression
