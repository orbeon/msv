package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{AttributeExp, ElementExp, Expression, ExpressionCloner, ExpressionPool, OtherExp, ReferenceExp}

/**
 * Creates an expression whose AttributeExp is completely replaced by epsilon.
 *
 * This step is used to erase all unconsumed AttributeExp from the expression.
 * This class is used for error recovery. Usually, unconsumed attributes
 * indicates a violation of the validity.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeRemover(pool: ExpressionPool) extends ExpressionCloner(pool):

  override def onAttribute(exp: AttributeExp): Expression = Expression.epsilon
  override def onRef(exp: ReferenceExp): Expression = exp.exp.visit(this)
  override def onOther(exp: OtherExp): Expression = exp.exp.visit(this)
  override def onElement(exp: ElementExp): Expression = exp
