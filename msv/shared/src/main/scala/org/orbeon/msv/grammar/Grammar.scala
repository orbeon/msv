package org.orbeon.msv.grammar

/**
 * base interface of the "grammar".
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait Grammar extends Serializable {
  /**
   * gets top-level expression.
   * This expression shall be the constraint over the document element.
   * Never return null.
   */
  def getTopLevel: Expression

  /**
   * gets ExpressionPool object which was used to construct this grammar.
   * Never return null.
   */
  def getPool: ExpressionPool
}
