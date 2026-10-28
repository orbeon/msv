package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.SequenceState

/**
 * parses &lt;optional&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class OptionalState extends SequenceState:
  protected override def annealExpression(exp: Expression): Expression =
    reader.pool.createOptional(exp)
