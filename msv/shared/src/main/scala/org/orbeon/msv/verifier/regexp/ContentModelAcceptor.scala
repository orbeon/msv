package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.{ElementExp, Expression}
import org.orbeon.msv.util.StringRef
import org.orbeon.msv.verifier.Acceptor

/**
 * base implementation for SimpleAcceptor and ComplexAcceptor
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ContentModelAcceptor(
  docDecl: REDocumentDeclaration,
  exp: Expression,
  ignoreUndeclaredAttributes: Boolean
) extends ExpressionAcceptor(docDecl, exp, ignoreUndeclaredAttributes):

  def stepForward(child: Acceptor, errRef: StringRef): Boolean =
    child match
      case sa: SimpleAcceptor =>
        if sa.continuation != null then
          stepForwardByContinuation(sa.continuation, errRef)
        else
          stepForward(new ElementToken(Array(sa.owner)), errRef)

      case ca: ComplexAcceptor =>
        val tokenOwners =
          if errRef != null then ca.owners
          else ca.getSatisfiedOwners()
        stepForward(new ElementToken(tokenOwners), errRef)

      case _ =>
        throw new Error("child must be either Simple or Complex.")

  protected def createAcceptor(
    combined: Expression,
    continuation: Expression,
    primitives: Array[ElementExp],
    numPrimitives: Int
  ): Acceptor =
    if primitives == null || numPrimitives <= 1 then
      new SimpleAcceptor(
        docDecl,
        combined,
        if primitives == null then null else primitives(0),
        continuation
      )
    else
      if Debug.debug then
        println("ComplexAcceptor is used")

      val owners = new Array[ElementExp](numPrimitives)
      System.arraycopy(primitives, 0, owners, 0, numPrimitives)
      new ComplexAcceptor(docDecl, combined, owners)

  def getOwnerType: AnyRef = null
