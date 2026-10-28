package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{ElementExp, Expression, IDContextProvider2}
import org.orbeon.msv.util.{DatatypeRef, StartTagInfo, StringRef}
import org.orbeon.msv.verifier.Acceptor

/**
 * base implementation of ComplexAcceptor.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ComplexAcceptorBaseImpl(
  docDecl: REDocumentDeclaration,
  combined: Expression,
  protected val contents: Array[Expression],
  ignoreUndeclaredAttributes: Boolean
) extends ContentModelAcceptor(docDecl, combined, ignoreUndeclaredAttributes):

  /** eats string literal */
  override def onText2(literal: String, context: IDContextProvider2, refErr: StringRef, refType: DatatypeRef): Boolean =
    if !super.onText2(literal, context, refErr, refType) then
      return false

    val token = new StringToken(docDecl, literal, context)
    val res = docDecl.resCalc

    var i = 0
    while i < contents.length do
      contents(i) = res.calcResidual(contents(i), token)
      i += 1

    true

  override def stepForward(child: Acceptor, errRef: StringRef): Boolean =
    if !super.stepForward(child, errRef) then
      return false

    val res = docDecl.resCalc
    val token: Token = child match
      case sa: SimpleAcceptor =>
        val cowner = sa.owner
        if cowner == null then AnyElementToken.theInstance
        else new ElementToken(Array(cowner))
      case ca: ComplexAcceptor =>
        if errRef != null then new ElementToken(ca.owners)
        else new ElementToken(ca.getSatisfiedOwners())
      case _ =>
        throw new Error("unexpected acceptor")

    var i = 0
    while i < contents.length do
      contents(i) = res.calcResidual(contents(i), token)
      i += 1

    true

  override protected def onAttribute(token: AttributeToken, refErr: StringRef): Boolean =
    if !super.onAttribute(token, refErr) then
      return false

    var i = 0
    while i < contents.length do
      contents(i) = docDecl.attFeeder.feed(contents(i), token, ignoreUndeclaredAttributes)
      i += 1

    true

  override def onEndAttributes(sti: StartTagInfo, refErr: StringRef): Boolean =
    if !super.onEndAttributes(sti, refErr) then
      return false

    var i = 0
    while i < contents.length do
      contents(i) = docDecl.attPruner.prune(contents(i))
      i += 1

    true
