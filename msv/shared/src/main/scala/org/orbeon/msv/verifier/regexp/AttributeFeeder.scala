package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.*
import scala.compiletime.uninitialized

/**
 * Feeds AttributeToken to the expression and obtains the residual (content model).
 *
 * AttributeTokens are fed in order-less fashion.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeFeeder(protected val docDecl: REDocumentDeclaration) extends ExpressionVisitorExpression:

  protected val pool: ExpressionPool = docDecl.pool
  private var token: Token = uninitialized

  final def feed(exp: Expression, token: AttributeToken, ignoreUndeclaredAttribute: Boolean): Expression =
    this.token = token
    var r = exp.visit(this)

    if r != Expression.nullSet || !ignoreUndeclaredAttribute then
      return r

    // if ignoreUndeclaredAttribute==true and expression is nullSet,
    // we have to check which of the following is the case.
    //   (1) attribute is undefined
    //   (2) value of the attribute was rejected.

    this.token = token.createRecoveryAttToken()
    r = exp.visit(this)

    // if wild card token is rejected, then it must be the absence of declaration.
    if r == Expression.nullSet then exp
    else Expression.nullSet

  def onAttribute(exp: AttributeExp): Expression =
    if token.`match`(exp) then Expression.epsilon
    else Expression.nullSet

  def onChoice(exp: ChoiceExp): Expression =
    pool.createChoice(exp.exp1.visit(this), exp.exp2.visit(this))

  def onElement(exp: ElementExp): Expression = Expression.nullSet

  def onOneOrMore(exp: OneOrMoreExp): Expression =
    pool.createSequence(
      exp.exp.visit(this),
      pool.createZeroOrMore(exp.exp)
    )

  def onMixed(exp: MixedExp): Expression =
    pool.createMixed(exp.exp.visit(this))

  def onList(exp: ListExp): Expression = Expression.nullSet
  def onEpsilon(): Expression = Expression.nullSet
  def onNullSet(): Expression = Expression.nullSet
  def onAnyString(): Expression = Expression.nullSet

  def onRef(exp: ReferenceExp): Expression = exp.exp.visit(this)
  def onOther(exp: OtherExp): Expression = exp.exp.visit(this)

  def onSequence(exp: SequenceExp): Expression =
    // for attributes only, sequence acts as orderless
    pool.createChoice(
      pool.createSequence(exp.exp1.visit(this), exp.exp2),
      pool.createSequence(exp.exp1, exp.exp2.visit(this))
    )

  def onData(exp: DataExp): Expression = Expression.nullSet
  def onValue(exp: ValueExp): Expression = Expression.nullSet

  def onConcur(exp: ConcurExp): Expression =
    pool.createConcur(exp.exp1.visit(this), exp.exp2.visit(this))

  def onInterleave(exp: InterleaveExp): Expression =
    pool.createChoice(
      pool.createInterleave(exp.exp1.visit(this), exp.exp2),
      pool.createInterleave(exp.exp1, exp.exp2.visit(this))
    )
