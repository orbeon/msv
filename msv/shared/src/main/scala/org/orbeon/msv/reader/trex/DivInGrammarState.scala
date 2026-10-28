package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.{ExpressionOwner, ExpressionState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse merged grammars. Also &lt;div&gt; element in the grammar element
 * (of RELAX NG).
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DivInGrammarState extends ExpressionState with ExpressionOwner:
  protected final def getReader(): TREXBaseReader = reader.asInstanceOf[TREXBaseReader]

  protected def makeExpression(): Expression = null

  protected override def createChildState(tag: StartTagInfo): State =
    if tag.localName == "start" then getReader().sfactory.start(this, tag)
    else if tag.localName == "define" then getReader().sfactory.define(this, tag)
    else if tag.localName == "include" then getReader().sfactory.includeGrammar(this, tag)
    else if tag.localName == "div" then getReader().sfactory.divInGrammar(this, tag)
    else null

  def onEndChild(exp: Expression): Unit = ()
