package org.orbeon.msv.grammar

import org.orbeon.msv.grammar.util.RefExpRemover
import scala.compiletime.uninitialized

/**
 * Primitive of the tree regular expression.
 *
 * Most of the derived classes are immutable (except ReferenceExp, ElementExp and OtherExp).
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class Expression extends Serializable {
  private var epsilonReducibility: java.lang.Boolean = uninitialized
  private var expandedExp: Expression = uninitialized
  private var cachedHashCode: Int = uninitialized

  @transient
  var verifierTag: AnyRef = uninitialized

  protected def this(hashCode: Int) = {
    this()
    this.cachedHashCode = hashCode
  }

  this.cachedHashCode = System.identityHashCode(this)

  final override def hashCode(): Int = cachedHashCode

  protected def calcHashCode(): Int

  def isEpsilonReducible: Boolean = {
    if (epsilonReducibility == null)
      epsilonReducibility = java.lang.Boolean.valueOf(calcEpsilonReducibility())
    epsilonReducibility.booleanValue()
  }

  protected def calcEpsilonReducibility(): Boolean

  def getExpandedExp(pool: ExpressionPool): Expression = {
    if (expandedExp == null) {
      expandedExp = this.visit(new RefExpRemover(pool, false))
    }
    expandedExp
  }

  final def peelOccurence(): Expression = {
    if (this.isInstanceOf[ChoiceExp]) {
      val cexp = this.asInstanceOf[ChoiceExp]
      if (cexp.exp1 eq Expression.epsilon)
        return cexp.exp2.peelOccurence()
      if (cexp.exp2 eq Expression.epsilon)
        return cexp.exp1.peelOccurence()
    }
    if (this.isInstanceOf[OneOrMoreExp])
      return this.asInstanceOf[OneOrMoreExp].exp.peelOccurence()

    this
  }

  def visit(visitor: ExpressionVisitor): AnyRef
  def visit(visitor: ExpressionVisitorExpression): Expression
  def visit(visitor: ExpressionVisitorBoolean): Boolean
  def visit(visitor: ExpressionVisitorVoid): Unit

  protected def readResolve(): AnyRef = {
    cachedHashCode = calcHashCode()
    this
  }
}

object Expression {
  private class EpsilonExpression extends Expression {
    protected final def calcHashCode(): Int = System.identityHashCode(this)
    def visit(visitor: ExpressionVisitor): AnyRef = visitor.onEpsilon()
    def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onEpsilon()
    def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onEpsilon()
    def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onEpsilon()
    protected def calcEpsilonReducibility(): Boolean = true
    override def equals(o: Any): Boolean = this eq o.asInstanceOf[AnyRef]
    override protected def readResolve(): AnyRef = Expression.epsilon
  }

  val epsilon: Expression = new EpsilonExpression()

  private class NullSetExpression extends Expression {
    protected final def calcHashCode(): Int = System.identityHashCode(this)
    def visit(visitor: ExpressionVisitor): AnyRef = visitor.onNullSet()
    def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onNullSet()
    def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onNullSet()
    def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onNullSet()
    protected def calcEpsilonReducibility(): Boolean = false
    override def equals(o: Any): Boolean = this eq o.asInstanceOf[AnyRef]
    override protected def readResolve(): AnyRef = Expression.nullSet
  }

  val nullSet: Expression = new NullSetExpression()

  private class AnyStringExpression extends Expression {
    protected final def calcHashCode(): Int = System.identityHashCode(this)
    def visit(visitor: ExpressionVisitor): AnyRef = visitor.onAnyString()
    def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onAnyString()
    def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onAnyString()
    def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onAnyString()
    protected def calcEpsilonReducibility(): Boolean = true
    override def equals(o: Any): Boolean = this eq o.asInstanceOf[AnyRef]
    override protected def readResolve(): AnyRef = Expression.anyString
  }

  val anyString: Expression = new AnyStringExpression()

  protected[grammar] def hashCode(o1: AnyRef, o2: AnyRef, hashKey: Int): Int =
    o1.hashCode + o2.hashCode + hashKey

  protected[grammar] def hashCode(o: AnyRef, hashKey: Int): Int =
    o.hashCode + hashKey
}
