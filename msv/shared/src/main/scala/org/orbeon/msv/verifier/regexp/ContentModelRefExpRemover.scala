package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.*

/**
 * Non-recursive ReferenceExpRemover with a cache.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class ContentModelRefExpRemover

object ContentModelRefExpRemover:
  private class Remover(pool: ExpressionPool) extends ExpressionCloner(pool):
    override def onElement(exp: ElementExp): Expression = exp

    override def onAttribute(exp: AttributeExp): Expression =
      val content = exp.exp.visit(this)
      if content == Expression.nullSet then
        Expression.nullSet
      else
        pool.createAttribute(exp.nameClass, content)

    override def onRef(exp: ReferenceExp): Expression = exp.exp.visit(this)
    override def onOther(exp: OtherExp): Expression = exp.exp.visit(this)
