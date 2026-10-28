package org.orbeon.msv.grammar

import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.StringPair

/**
 * Expression that matches a particular value of a [[Datatype]].
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class ValueExp(val dt: Datatype, val name: StringPair, val value: AnyRef)
  extends Expression(dt.hashCode + dt.valueHashCode(value)) with DataOrValueExp {

  def getType: Datatype = dt
  def getName: StringPair = name

  protected final def calcHashCode(): Int =
    dt.hashCode + dt.valueHashCode(value)

  override def equals(o: Any): Boolean = {
    if (o.getClass != this.getClass) return false
    val rhs = o.asInstanceOf[ValueExp]
    if (!rhs.dt.equals(dt)) return false
    dt.sameValue(value, rhs.value)
  }

  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onValue(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onValue(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onValue(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onValue(this)

  protected def calcEpsilonReducibility(): Boolean = false
}
