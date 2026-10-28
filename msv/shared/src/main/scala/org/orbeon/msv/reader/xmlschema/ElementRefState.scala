package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.ReferenceContainer
import org.orbeon.msv.grammar.xmlschema.XMLSchemaSchema
import org.orbeon.msv.reader.ExpressionWithoutChildState

/**
 * Used to parse &lt;element &gt; element with ref attribute.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementRefState extends ExpressionWithoutChildState:

  protected def makeExpression(): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    if !startTag.containsAttribute("ref") then
      throw new Error()

    val exp = reader.resolveQNameRef(
      startTag,
      "ref",
      new XMLSchemaReader.RefResolver:
        def get(g: XMLSchemaSchema): ReferenceContainer = g.elementDecls
    )
    if exp == null then Expression.epsilon // couldn't resolve QName.
    else exp
