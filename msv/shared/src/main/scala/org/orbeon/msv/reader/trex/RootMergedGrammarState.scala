package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.{ExpressionOwner, SimpleState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * parses root state of a merged grammar.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RootMergedGrammarState extends SimpleState with ExpressionOwner:
  protected override def createChildState(tag: StartTagInfo): State =
    if tag.localName == "grammar" then new DivInGrammarState()
    else null

  def onEndChild(exp: Expression): Unit = ()
