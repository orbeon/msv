package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{ElementExp, Expression}
import org.orbeon.msv.verifier.Acceptor

/**
 * Acceptor that will be used when only one ElementExp matches
 * the start tag.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SimpleAcceptor(
  docDecl: REDocumentDeclaration,
  combined: Expression,
  val owner: ElementExp,
  val continuation: Expression
) extends ContentModelAcceptor(
  docDecl,
  combined,
  if owner == null then true else owner.ignoreUndeclaredAttributes
):

  override def getOwnerType: AnyRef = owner

  def createClone(): Acceptor =
    new SimpleAcceptor(docDecl, getExpression, owner, continuation)
