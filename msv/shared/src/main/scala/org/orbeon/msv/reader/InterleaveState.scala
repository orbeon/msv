package org.orbeon.msv.reader

import org.orbeon.msv.grammar.Expression

/**
 * state that creates an InterleaveExp.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class InterleaveState extends ExpressionWithChildState:
  protected def castExpression(exp: Expression, child: Expression): Expression =
    if exp == null then child
    else reader.pool.createInterleave(exp, child)
