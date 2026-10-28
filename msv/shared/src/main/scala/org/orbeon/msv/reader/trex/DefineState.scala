package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.{Expression, ReferenceExp}
import org.orbeon.msv.reader.{ExpressionOwner, GrammarReader, SimpleState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * parses &lt;define&gt; declaration.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class DefineState extends SimpleState with ExpressionOwner:
  protected var exp: Expression = null
  private var ref: ReferenceExp = null

  final def getRef(): ReferenceExp = ref

  final def onEndChild(child: Expression): Unit =
    if exp == null then
      exp = child
    else
      exp = reader.pool.createSequence(exp, child)

  protected override def startSelf(): Unit =
    super.startSelf()
    ref = getReference()

  protected override def endSelf(): Unit =
    if exp == null then
      reader.reportError(GrammarReader.ERR_MISSING_CHILD_EXPRESSION)
      exp = Expression.nullSet

    if ref != null then
      val trexReader = reader.asInstanceOf[TREXBaseReader]
      val combine = startTag.getCollapsedAttribute("combine")

      exp = callInterceptExpression(exp)

      val newexp = doCombine(ref, exp, combine)
      if newexp == null then
        reader.reportError(TREXBaseReader.ERR_BAD_COMBINE, combine)
      else
        ref.exp = newexp

      reader.setDeclaredLocationOf(ref)
      parentState.asInstanceOf[ExpressionOwner].onEndChild(ref)

  protected override def createChildState(tag: StartTagInfo): State =
    reader.createExpressionChildState(this, tag)

  protected def getReference(): ReferenceExp =
    val name = startTag.getCollapsedAttribute("name")
    if name == null then
      reader.reportError(GrammarReader.ERR_MISSING_ATTRIBUTE, "ref", "name")
      null
    else
      val trexReader = reader.asInstanceOf[TREXBaseReader]
      trexReader.grammar.namedPatterns.getOrCreate(name)

  protected def doCombine(baseExp: ReferenceExp, newExp: Expression, combine: String): Expression
