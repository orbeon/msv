package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.{Expression, NameClass, ReferenceExp}

/**
 * 'tag'  of RELAX module.
 *
 * exp field contains a sequence of AttributeExp.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TagClause protected[relax] (role: String) extends ReferenceExp(role):

  /**
   * tag name constraint.
   * This should be SimpleNameClass. The only exception is for stub module.
   */
  var nameClass: NameClass = scala.compiletime.uninitialized

  /** constructor for inline tag. creatable directly from outside */
  def this() = this(null.asInstanceOf[String])

  def visit(visitor: RELAXExpressionVisitor): AnyRef =
    visitor.onTag(this)

  def visit(visitor: RELAXExpressionVisitorExpression): Expression =
    visitor.onTag(this)

  def visit(visitor: RELAXExpressionVisitorBoolean): Boolean =
    visitor.onTag(this)

  def visit(visitor: RELAXExpressionVisitorVoid): Unit =
    visitor.onTag(this)
