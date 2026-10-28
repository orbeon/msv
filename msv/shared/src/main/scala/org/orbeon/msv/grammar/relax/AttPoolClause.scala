package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.{Expression, ReferenceExp}

/**
 * 'attPool'  of RELAX module.
 *
 * ReferenceExp.exp contains a sequence of AttributeExp.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttPoolClause protected[relax] (role: String) extends ReferenceExp(role) with Exportable:

  /**
   * a flag that indicates this elementRule is exported and
   * therefore accessible from other modules.
   */
  var exported: Boolean = false
  def isExported(): Boolean = exported

  def visit(visitor: RELAXExpressionVisitor): AnyRef =
    visitor.onAttPool(this)

  def visit(visitor: RELAXExpressionVisitorExpression): Expression =
    visitor.onAttPool(this)

  def visit(visitor: RELAXExpressionVisitorBoolean): Boolean =
    visitor.onAttPool(this)

  def visit(visitor: RELAXExpressionVisitorVoid): Unit =
    visitor.onAttPool(this)
