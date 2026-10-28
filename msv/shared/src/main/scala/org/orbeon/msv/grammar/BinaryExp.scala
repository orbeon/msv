package org.orbeon.msv.grammar

import java.util.Iterator

/**
 * Base implementation for those expression which has two child expressions.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class BinaryExp(val exp1: Expression, val exp2: Expression)
  extends Expression(exp1.hashCode + exp2.hashCode) {

  protected final def calcHashCode(): Int =
    exp1.hashCode + exp2.hashCode

  override def equals(o: Any): Boolean = {
    if (this.getClass != o.getClass) return false
    val rhs = o.asInstanceOf[BinaryExp]
    (rhs.exp1 eq exp1) && (rhs.exp2 eq exp2)
  }

  /**
   * returns all child expressions in one array.
   */
  def getChildren: Array[Expression] = {
    var cnt = 1
    var exp: Expression = this
    while (exp.getClass == this.getClass) {
      cnt += 1
      exp = exp.asInstanceOf[BinaryExp].exp1
    }

    val r = new Array[Expression](cnt)
    exp = this
    while (exp.getClass == this.getClass) {
      cnt -= 1
      r(cnt) = exp.asInstanceOf[BinaryExp].exp2
      exp = exp.asInstanceOf[BinaryExp].exp1
    }
    r(0) = exp

    r
  }

  /**
   * iterates all child expressions.
   */
  def children(): Iterator[Expression] = {
    val items = getChildren
    new Iterator[Expression] {
      private var idx = 0
      def next(): Expression = {
        val r = items(idx)
        idx += 1
        r
      }
      def hasNext: Boolean = idx != items.length
      override def remove(): Unit = throw new UnsupportedOperationException()
    }
  }
}
