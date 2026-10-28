package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{AttributeExp, Expression}

/**
 * special AttributeToken that memorizes {@link AttributeExp} that fails to
 * validate itself.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class AttributeRecoveryToken(
  docDecl: REDocumentDeclaration,
  namespaceURI: String,
  localName: String,
  qName: String,
  value: StringToken
) extends AttributeToken(docDecl, namespaceURI, localName, qName, value):

  private var failedExp: Expression = Expression.nullSet

  override def `match`(exp: AttributeExp): Boolean =
    // Attribute name must meet the constraint of NameClass
    if !exp.nameClass.accepts(namespaceURI, localName) then
      return false

    // content model of the attribute must consume the value
    if !docDecl.resCalc.calcResidual(exp.exp, value).isEpsilonReducible then
      failedExp = docDecl.pool.createChoice(failedExp, exp.exp)

    // accept AttributeExp regardless of its content restriction
    true

  def getFailedExp: Expression = failedExp
