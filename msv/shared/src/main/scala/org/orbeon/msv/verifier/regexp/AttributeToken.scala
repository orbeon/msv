package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{AttributeExp, IDContextProvider2}
import org.orbeon.msv.util.DatatypeRef
import scala.compiletime.uninitialized

/**
 * represents attribute and its value.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeToken(protected var docDecl: REDocumentDeclaration) extends Token:

  var namespaceURI: String = uninitialized
  var localName: String = uninitialized
  var qName: String = uninitialized
  var value: StringToken = uninitialized

  /**
   * holds a reference to the assigned type.
   *
   * If this AttributeToken is successfully consumed, then this field
   * contains the AttributeExp which consumed this token.
   *
   * If this token is not consumed or several different AttributeExps
   * consumed this token, then null.
   */
  var matchedExp: AttributeExp = uninitialized

  /**
   * If this value is false, the "matched" field must always null. This indicates
   * that no AttributeExp has consumed this token yet.
   * If this value is true and the "matched" field is non-null, then it means
   * that AttributeExp has consumed this token.
   * If this value is true and the "matched" field is null, then more than
   * one AttributeExps have consumed this token.
   */
  private var saturated: Boolean = false

  def this(
    docDecl: REDocumentDeclaration,
    namespaceURI: String,
    localName: String,
    qName: String,
    value: StringToken
  ) =
    this(docDecl)
    reinit(namespaceURI, localName, qName, value)

  def this(
    docDecl: REDocumentDeclaration,
    namespaceURI: String,
    localName: String,
    qName: String,
    value: String,
    context: IDContextProvider2
  ) =
    this(
      docDecl,
      namespaceURI,
      localName,
      qName,
      new StringToken(docDecl, value, context, new DatatypeRef())
    )

  def reinit(namespaceURI: String, localName: String, qName: String, value: StringToken): Unit =
    this.namespaceURI = namespaceURI
    this.localName = localName
    this.qName = qName
    this.value = value
    this.matchedExp = null
    this.saturated = false

  /**
   * creates a special AttributeToken which matchs any content restrictions.
   *
   * This token acts like a wild card for the attribute. This method is
   * used for error recovery.
   */
  def createRecoveryAttToken(): AttributeRecoveryToken =
    new AttributeRecoveryToken(docDecl, namespaceURI, localName, qName, value)

  override def `match`(exp: AttributeExp): Boolean =
    // Attribute name must meet the constraint of NameClass
    if !exp.nameClass.accepts(namespaceURI, localName) then
      return false

    // content model of the attribute must consume the value
    var satisfied = false
    if value.literal.trim.length == 0 && exp.exp.isEpsilonReducible then
      satisfied = true
    else if docDecl.resCalc.calcResidual(exp.exp, value).isEpsilonReducible then
      satisfied = true

    if satisfied then
      // store the expression who consumed this token.
      if !saturated || exp == matchedExp then
        matchedExp = exp
      else
        matchedExp = null
      saturated = true
      true
    else
      false
