package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.grammar.trex.TREXGrammar

/**
 * parses &lt;grammar&gt; element.
 * 
 * this state is used to parse top-level grammars and nested grammars.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class GrammarState extends DivInGrammarState:
  protected var previousGrammar: TREXGrammar = null
  protected var newGrammar: TREXGrammar = null

  protected override def makeExpression(): Expression = newGrammar

  protected override def startSelf(): Unit =
    super.startSelf()
    previousGrammar = getReader().grammar
    newGrammar = getReader().sfactory.createGrammar(reader.pool, previousGrammar)
    getReader().grammar = newGrammar

  override def endSelf(): Unit =
    val grammar = getReader().grammar

    // detect references to undefined patterns
    reader.detectUndefinedOnes(grammar.namedPatterns, TREXBaseReader.ERR_UNDEFINED_PATTERN)

    // is start pattern defined?
    if grammar.exp == null then
      reader.reportError(org.orbeon.msv.reader.GrammarReader.ERR_MISSING_TOPLEVEL)
      grammar.exp = Expression.nullSet

    if previousGrammar != null then
      getReader().grammar = previousGrammar

    super.endSelf()
