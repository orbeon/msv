package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{Expression, ExpressionPool, Grammar}
import org.orbeon.msv.verifier.{Acceptor, DocumentDeclaration}

/**
 * {@link DocumentDeclaration} implementation
 *
 * Grammar object can be shared among multiple threads, but this object
 * cannot be shared.
 *
 * <p>
 * REDocumentDeclaration always returns subclasses of {@link ExpressionAcceptor}
 * as the Acceptor implementation.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class REDocumentDeclaration(
  protected val topLevel: Expression,
  val pool: ExpressionPool
) extends DocumentDeclaration:

  def this(grammar: Grammar) =
    this(grammar.getTopLevel, grammar.getPool)

  val resCalc: ResidualCalculator = new ResidualCalculator(pool)
  val attFeeder: AttributeFeeder = new AttributeFeeder(this)
  val attPicker: AttributePicker = new AttributePicker(pool)
  val attPruner: AttributePruner = new AttributePruner(pool)
  val attRemover: AttributeRemover = new AttributeRemover(pool)
  val cccec: CombinedChildContentExpCreator = new CombinedChildContentExpCreator(pool)
  val ecc: ElementsOfConcernCollector = new ElementsOfConcernCollector()

  val attToken: AttributeToken = new AttributeToken(this, null, null, null, null.asInstanceOf[StringToken])

  def createAcceptor(): Acceptor =
    // top-level Acceptor cannot have continuation.
    new SimpleAcceptor(this, topLevel, null, Expression.epsilon)

  def localizeMessage(propertyName: String, args: Array[AnyRef]): String =
    val template = REDocumentDeclaration.messages.getOrElse(propertyName, propertyName)
    REDocumentDeclaration.format(template, args)

  def localizeMessage(propName: String, arg1: AnyRef): String =
    localizeMessage(propName, Array(arg1))

  def localizeMessage(propName: String, arg1: AnyRef, arg2: AnyRef): String =
    localizeMessage(propName, Array(arg1, arg2))

object REDocumentDeclaration:
  val DIAG_ELEMENT_NOT_ALLOWED: String = "Diagnosis.ElementNotAllowed"
  val DIAG_CONTENT_MODEL_IS_NULLSET: String = "Diagnosis.ContentModelIsNullset"
  val DIAG_BAD_TAGNAME_GENERIC: String = "Diagnosis.BadTagName.Generic"
  val DIAG_BAD_TAGNAME_WRAPUP: String = "Diagnosis.BadTagName.WrapUp"
  val DIAG_BAD_TAGNAME_SEPARATOR: String = "Diagnosis.BadTagName.Separator"
  val DIAG_BAD_TAGNAME_MORE: String = "Diagnosis.BadTagName.More"
  val DIAG_BAD_TAGNAME_WRONG_NAMESPACE: String = "Diagnosis.BadTagName.WrongNamespace"
  val DIAG_BAD_TAGNAME_PROBABLY_WRONG_NAMESPACE: String = "Diagnosis.BadTagName.ProbablyWrongNamespace"
  val DIAG_UNDECLARED_ATTRIBUTE: String = "Diagnosis.UndeclaredAttribute"
  val DIAG_BAD_ATTRIBUTE_VALUE_GENERIC: String = "Diagnosis.BadAttributeValue.Generic"
  val DIAG_BAD_ATTRIBUTE_VALUE_DATATYPE: String = "Diagnosis.BadAttributeValue.DataType"
  val DIAG_BAD_ATTRIBUTE_VALUE_WRAPUP: String = "Diagnosis.BadAttributeValue.WrapUp"
  val DIAG_BAD_ATTRIBUTE_VALUE_SEPARATOR: String = "Diagnosis.BadAttributeValue.Separator"
  val DIAG_BAD_ATTRIBUTE_VALUE_MORE: String = "Diagnosis.BadAttributeValue.More"
  val DIAG_MISSING_ATTRIBUTE_SIMPLE: String = "Diagnosis.MissingAttribute.Simple"
  val DIAG_MISSING_ATTRIBUTE_GENERIC: String = "Diagnosis.MissingAttribute.Generic"
  val DIAG_MISSING_ATTRIBUTE_WRAPUP: String = "Diagnosis.MissingAttribute.WrapUp"
  val DIAG_MISSING_ATTRIBUTE_SEPARATOR: String = "Diagnosis.MissingAttribute.Separator"
  val DIAG_MISSING_ATTRIBUTE_MORE: String = "Diagnosis.MissingAttribute.More"
  val DIAG_UNCOMPLETED_CONTENT_WRAPUP: String = "Diagnosis.UncompletedContent.WrapUp"
  val DIAG_UNCOMPLETED_CONTENT_SEPARATOR: String = "Diagnosis.UncompletedContent.Separator"
  val DIAG_UNCOMPLETED_CONTENT_MORE: String = "Diagnosis.UncompletedContent.More"
  val DIAG_BAD_LITERAL_WRAPUP: String = "Diagnosis.BadLiteral.WrapUp"
  val DIAG_BAD_LITERAL_SEPARATOR: String = "Diagnosis.BadLiteral.Separator"
  val DIAG_BAD_LITERAL_MORE: String = "Diagnosis.BadLiteral.More"
  val DIAG_BAD_LITERAL_GENERIC: String = "Diagnosis.BadLiteral.Generic"
  val DIAG_BAD_LITERAL_INCORRECT_VALUE: String = "Diagnosis.BadLiteral.IncorrectValue"
  val DIAG_SIMPLE_NAMECLASS: String = "Diagnosis.SimpleNameClass"
  val DIAG_NAMESPACE_NAMECLASS: String = "Diagnosis.NamespaceNameClass"
  val DIAG_NOT_NAMESPACE_NAMECLASS: String = "Diagnosis.NotNamespaceNameClass"
  val DIAG_STRING_NOT_ALLOWED: String = "Diagnosis.StringNotAllowed"
  val DIAG_BAD_KEY_VALUE: String = "Diagnosis.BadKeyValue"
  val DIAG_BAD_KEY_VALUE2: String = "Diagnosis.BadKeyValue2"

  private[regexp] val messages: Map[String, String] = Map(
    DIAG_ELEMENT_NOT_ALLOWED -> "element \"{0}\" was found where no element may occur",
    DIAG_CONTENT_MODEL_IS_NULLSET -> "The content model of this element can never be satisfied as declared. It may be a bug of the schema.",
    DIAG_BAD_TAGNAME_GENERIC -> "tag name \"{0}\" is not allowed",
    DIAG_BAD_TAGNAME_WRAPUP -> "tag name \"{0}\" is not allowed. Possible tag names are: {1}",
    DIAG_BAD_TAGNAME_SEPARATOR -> ", ",
    DIAG_BAD_TAGNAME_MORE -> ", and more",
    DIAG_BAD_TAGNAME_WRONG_NAMESPACE -> "namespace URI of tag \"{0}\" is wrong. It must be \"{1}\"",
    DIAG_BAD_TAGNAME_PROBABLY_WRONG_NAMESPACE -> "Probably namespace URI of tag \"{0}\" is wrong (correct one is \"{1}\")",
    DIAG_UNDECLARED_ATTRIBUTE -> "unexpected attribute \"{0}\"",
    DIAG_BAD_ATTRIBUTE_VALUE_GENERIC -> "attribute \"{0}\" has a bad value",
    DIAG_BAD_ATTRIBUTE_VALUE_DATATYPE -> "attribute \"{0}\" has a bad value: {1}",
    DIAG_BAD_ATTRIBUTE_VALUE_WRAPUP -> "attribute \"{0}\" has a bad value. Possible values are: {1}",
    DIAG_BAD_ATTRIBUTE_VALUE_SEPARATOR -> ", ",
    DIAG_BAD_ATTRIBUTE_VALUE_MORE -> ", and more",
    DIAG_MISSING_ATTRIBUTE_SIMPLE -> "element \"{0}\" is missing \"{1}\" attribute",
    DIAG_MISSING_ATTRIBUTE_GENERIC -> "required attribute(s) is/are missing",
    DIAG_MISSING_ATTRIBUTE_WRAPUP -> "at least one of the following attributes is required for element \"{0}\": {1}",
    DIAG_MISSING_ATTRIBUTE_SEPARATOR -> ", ",
    DIAG_MISSING_ATTRIBUTE_MORE -> ", or more",
    DIAG_UNCOMPLETED_CONTENT_WRAPUP -> "uncompleted content model. expecting: {1}",
    DIAG_UNCOMPLETED_CONTENT_SEPARATOR -> ", ",
    DIAG_UNCOMPLETED_CONTENT_MORE -> ", etc.",
    DIAG_BAD_LITERAL_WRAPUP -> "unexpected character literal. expecting: {0}  found: {1}",
    DIAG_BAD_LITERAL_SEPARATOR -> ", ",
    DIAG_BAD_LITERAL_MORE -> ", etc.",
    DIAG_BAD_LITERAL_GENERIC -> "bad character literal: {0}",
    DIAG_BAD_LITERAL_INCORRECT_VALUE -> "the value must be \"{0}\", but found \"{1}\"",
    DIAG_SIMPLE_NAMECLASS -> "<{0}>",
    DIAG_NAMESPACE_NAMECLASS -> "elements of \"{0}\" namespace",
    DIAG_NOT_NAMESPACE_NAMECLASS -> "elements of any namespace other than \"{0}\"",
    DIAG_STRING_NOT_ALLOWED -> "No character data is allowed here, but found \"{0}\"",
    DIAG_BAD_KEY_VALUE -> "ID \"{0}\" is already defined.",
    DIAG_BAD_KEY_VALUE2 -> "duplicate key value \"{0}\" for the symbol space \"{1}\""
  )

  def format(formatStr: String, args: Array[AnyRef]): String =
    if args == null || args.isEmpty then formatStr
    else
      var res = formatStr
      var i = 0
      while i < args.length do
        val s = if args(i) != null then args(i).toString else "null"
        res = res.replace("{" + i + "}", s)
        i += 1
      res
