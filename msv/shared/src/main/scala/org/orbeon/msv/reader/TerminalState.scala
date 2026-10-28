package org.orbeon.msv.reader

import org.orbeon.msv.grammar.Expression

/**
 * State that always returns the same expression.
 * 
 * Typically used for &lt;empty/&gt;, &lt;notAllowed/&gt; and &lt;text/> of RELAX NG.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TerminalState(private val exp: Expression) extends ExpressionWithoutChildState:
  protected def makeExpression(): Expression = exp
