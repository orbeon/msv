package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.{ElementExp, Expression, ExpressionPool, NameClass}

/**
 * ElementRule declaration of RELAX.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementRule(
  pool: ExpressionPool,
  val clause: TagClause,
  contentModel: Expression
) extends ElementExp(pool.createSequence(clause, contentModel), true):

  /** Attribute-free content model */
  val attributeFreeContentModel: Expression = contentModel

  protected[relax] var parent: ElementRules = scala.compiletime.uninitialized

  /** gets the parent ElementRules object.
   * 
   * when this object is used as a named, no-inline elementRule,
   * this variable holds a reference to the parent ElementRules object.
   * otherwise, null
   */
  def getParent(): ElementRules = parent

  final def getNameClass: NameClass = clause.nameClass
