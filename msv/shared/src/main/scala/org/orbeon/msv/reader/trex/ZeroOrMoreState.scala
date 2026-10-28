package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.SequenceState

/**
 * parses &lt;zeroOrMore&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ZeroOrMoreState extends SequenceState:
  protected override def annealExpression(exp: Expression): Expression =
    reader.pool.createZeroOrMore(exp)
