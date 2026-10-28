package org.orbeon.msv.reader

import org.orbeon.msv.grammar.Expression

/**
 * interface that must be implemented by the parent state of ExpressionState.
 *
 * ExpressionState notifies its parent by using this interface.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait ExpressionOwner:
  def onEndChild(exp: Expression): Unit
