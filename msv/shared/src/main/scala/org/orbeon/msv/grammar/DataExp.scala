package org.orbeon.msv.grammar

import org.orbeon.msv.datatype.xsd.XSDatatype
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.StringPair

/**
 * Expression that matches characters of the particular [[Datatype]].
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class DataExp(val dt: Datatype, val name: StringPair, val except: Expression)
  extends Expression(dt.hashCode + except.hashCode) with DataOrValueExp {

  def getType: Datatype = dt
  def getName: StringPair = name

  protected final def calcHashCode(): Int =
    dt.hashCode + except.hashCode

  override def equals(o: Any): Boolean = {
    if (o.getClass != this.getClass) return false
    val rhs = o.asInstanceOf[DataExp]
    (this.except eq rhs.except) && rhs.dt.equals(dt)
  }

  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onData(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onData(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onData(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onData(this)

  protected def calcEpsilonReducibility(): Boolean = {
    // ORBEON: This can fail, see https://forge.objectweb.org/tracker/index.php?func=detail&aid=307143&group_id=168&atid=350207
    if ((except eq Expression.nullSet) && dt.isInstanceOf[XSDatatype] && dt.asInstanceOf[XSDatatype].isAlwaysValid())
      return true

    false
  }
}
