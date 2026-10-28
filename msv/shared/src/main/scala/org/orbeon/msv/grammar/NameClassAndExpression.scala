package org.orbeon.msv.grammar

/**
 * Common interface of [[AttributeExp]] and [[ElementExp]].
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait NameClassAndExpression {
  def getNameClass: NameClass
  def getContentModel: Expression
}
