package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.{ExpressionOwner, SimpleState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * parses the root state of a grammar included as a pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RootIncludedPatternState(protected val grandParent: IncludePatternState)
  extends SimpleState with ExpressionOwner:

  def this() = this(null)

  protected override def createChildState(tag: StartTagInfo): State =
    reader.createExpressionChildState(this, tag)

  def onEndChild(exp: Expression): Unit =
    if grandParent != null then
      grandParent.onEndChild(exp)
