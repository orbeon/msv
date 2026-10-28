package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.trex.ElementPattern

/**
 * parses &lt;element&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementState extends NameClassAndExpressionState:
  protected override def annealExpression(contentModel: Expression): Expression =
    val e = new ElementPattern(nameClass, contentModel)
    reader.setDeclaredLocationOf(e)
    e
