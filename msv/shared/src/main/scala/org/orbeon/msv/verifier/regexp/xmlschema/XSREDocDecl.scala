package org.orbeon.msv.verifier.regexp.xmlschema

import org.orbeon.msv.grammar.{AttributeExp, Expression, NamespaceNameClass}
import org.orbeon.msv.grammar.xmlschema.XMLSchemaGrammar
import org.orbeon.msv.verifier.Acceptor
import org.orbeon.msv.verifier.regexp.{AttributeFeeder, CombinedChildContentExpCreator, REDocumentDeclaration}

/**
 * {@link REDocumentDeclaration} that supports several W3C XML Schema
 * specific semantics.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class XSREDocDecl(val grammar: XMLSchemaGrammar) extends REDocumentDeclaration(grammar):

  override def createAcceptor(): Acceptor =
    // use XSAcceptor instead
    new XSAcceptor(this, topLevel, null, Expression.epsilon)

  def getCCCEC: CombinedChildContentExpCreator = cccec
  def getAttFeeder: AttributeFeeder = attFeeder

  /**
   * AttributeExp that matches to "xsi:***" attributes.
   */
  val xsiAttExp: AttributeExp =
    new AttributeExp(
      new NamespaceNameClass(XSAcceptor.XSINamespace),
      Expression.anyString
    )

  override def localizeMessage(propertyName: String, args: Array[AnyRef]): String =
    XSREDocDecl.messages.get(propertyName) match
      case Some(template) => REDocumentDeclaration.format(template, args)
      case None           => super.localizeMessage(propertyName, args)

object XSREDocDecl:
  val ERR_NON_NILLABLE_ELEMENT: String   = "XMLSchemaVerifier.NonNillableElement"
  val ERR_NOT_SUBSTITUTABLE_TYPE: String = "XMLSchemaVerifier.NotSubstitutableType"
  val ERR_UNDEFINED_TYPE: String         = "XMLSchemaVerifier.UndefinedType"

  private val messages: Map[String, String] = Map(
    ERR_NON_NILLABLE_ELEMENT   -> "element \"{0}\" is not nillable",
    ERR_NOT_SUBSTITUTABLE_TYPE -> "specified type \"{0}\" cannot substitute the current type",
    ERR_UNDEFINED_TYPE         -> "undefined type \"{0}\""
  )
