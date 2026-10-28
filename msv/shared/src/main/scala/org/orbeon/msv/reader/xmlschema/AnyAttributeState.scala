package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.NameClass
import org.orbeon.msv.grammar.ReferenceExp
import org.orbeon.msv.grammar.xmlschema.AttributeDeclExp
import org.orbeon.msv.grammar.xmlschema.AttributeWildcard
import org.orbeon.msv.grammar.xmlschema.XMLSchemaSchema

/**
 * Used to parse &lt;anyAttribute &gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AnyAttributeState extends AnyState:

  protected def createExpression(namespace: String, process: String): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val currentSchema = reader.currentSchema

    val mode: Int =
      if process == "skip" then AttributeWildcard.SKIP
      else if process == "lax" then AttributeWildcard.LAX
      else if process == "strict" then AttributeWildcard.STRICT
      else
        reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "processContents", process)
        AttributeWildcard.SKIP

    parentState.asInstanceOf[AnyAttributeOwner].setAttributeWildcard(
      new AttributeWildcard(getNameClass(namespace, currentSchema), mode)
    )

    Expression.epsilon

  protected def getNameClassFrom(exp: ReferenceExp): NameClass =
    exp.asInstanceOf[AttributeDeclExp].self.nameClass
