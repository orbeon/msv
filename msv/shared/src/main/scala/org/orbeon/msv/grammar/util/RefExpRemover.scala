package org.orbeon.msv.grammar.util

import java.util.{HashSet, Set => JSet}
import org.orbeon.msv.grammar.{AttributeExp, ElementExp, Expression, ExpressionCloner, ExpressionPool, OtherExp, ReferenceExp}

/**
 * removes all ReferenceExp from AGM.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RefExpRemover(pool: ExpressionPool, private val recursive: Boolean) extends ExpressionCloner(pool) {
  private val visitedElements: JSet[ElementExp] = new HashSet[ElementExp]()

  def onElement(exp: ElementExp): Expression = {
    if (!recursive)
      return exp

    if (!visitedElements.contains(exp)) {
      visitedElements.add(exp)
      exp.contentModel = exp.contentModel.visit(this)
    }
    if (exp.contentModel eq Expression.nullSet)
      Expression.nullSet
    else
      exp
  }

  def onAttribute(exp: AttributeExp): Expression = {
    val content = exp.exp.visit(this)
    if (content eq Expression.nullSet)
      Expression.nullSet
    else
      pool.createAttribute(exp.nameClass, content)
  }

  def onRef(exp: ReferenceExp): Expression =
    exp.exp.visit(this)

  def onOther(exp: OtherExp): Expression =
    exp.exp.visit(this)
}
