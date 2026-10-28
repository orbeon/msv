package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{ElementExp, Expression, ExpressionPool}
import org.orbeon.msv.verifier.Acceptor

/**
 * Accept that is used when more than one pattern can be applicable to the current context.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class ComplexAcceptor(
  docDecl: REDocumentDeclaration,
  combined: Expression,
  contentModels: Array[Expression],
  val owners: Array[ElementExp]
) extends ComplexAcceptorBaseImpl(docDecl, combined, contentModels, owners(0).ignoreUndeclaredAttributes):

  def this(docDecl: REDocumentDeclaration, combined: Expression, primitives: Array[ElementExp]) =
    this(docDecl, combined, ComplexAcceptor.createDefaultContentModels(primitives, docDecl.pool), primitives)

  def createClone(): Acceptor =
    val models = new Array[Expression](contents.length)
    System.arraycopy(contents, 0, models, 0, contents.length)
    new ComplexAcceptor(docDecl, getExpression, models, owners)

  /**
   * collects satisfied ElementExps.
   *
   * "satisfied ElementExps" are ElementExps whose
   * contents is now epsilon reducible.
   */
  def getSatisfiedOwners(): Array[ElementExp] =
    var i = 0
    var cnt = 0
    while i < contents.length do
      if contents(i).isEpsilonReducible then cnt += 1
      i += 1

    if cnt == 0 then return new Array[ElementExp](0)

    val satisfied = new Array[ElementExp](cnt)
    i = 0
    cnt = 0
    while i < contents.length do
      if contents(i).isEpsilonReducible then
        satisfied(cnt) = owners(i)
        cnt += 1
      i += 1

    satisfied

object ComplexAcceptor:
  private def createDefaultContentModels(owners: Array[ElementExp], pool: ExpressionPool): Array[Expression] =
    val r = new Array[Expression](owners.length)
    var i = 0
    while i < owners.length do
      r(i) = owners(i).contentModel.getExpandedExp(pool)
      i += 1
    r
