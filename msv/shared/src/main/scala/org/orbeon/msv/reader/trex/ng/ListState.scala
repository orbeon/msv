package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.SequenceState

/**
 * state that parses &lt;list&gt; pattern of RELAX NG.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ListState extends SequenceState:
  protected override def annealExpression(exp: Expression): Expression =
    reader.pool.createList(exp)
