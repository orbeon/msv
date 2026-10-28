package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.{Expression, ReferenceExp}
import org.orbeon.msv.grammar.trex.TREXGrammar
import org.orbeon.msv.reader.ExpressionWithoutChildState

/**
 * parses &lt;ref&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RefState(protected val parentRef: Boolean) extends ExpressionWithoutChildState:
  protected def makeExpression(): Expression =
    val name = startTag.getCollapsedAttribute("name")

    if name == null then
      reader.reportError(org.orbeon.msv.reader.GrammarReader.ERR_MISSING_ATTRIBUTE, "ref", "name")
      return Expression.nullSet

    var grammar: TREXGrammar = reader.asInstanceOf[TREXBaseReader].grammar

    if parentRef then
      grammar = grammar.getParentGrammar
      if grammar == null then
        reader.reportError(TREXBaseReader.ERR_NONEXISTENT_PARENT_GRAMMAR)
        return Expression.nullSet

    val r = grammar.namedPatterns.getOrCreate(name)
    wrapUp(r)
    r

  protected def wrapUp(r: ReferenceExp): Unit =
    reader.backwardReference.memorizeLink(r)
