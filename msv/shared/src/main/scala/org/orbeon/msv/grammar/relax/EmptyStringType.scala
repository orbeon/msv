package org.orbeon.msv.grammar.relax

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.datatype.xsd.{BuiltinAtomicType, SimpleURType, XSDatatype}
import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * 'emptyString' type of RELAX.
 *
 * this type accepts nothing but "".
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class EmptyStringType private () extends BuiltinAtomicType("emptyString"):

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  def isFacetApplicable(facetName: String): Int = XSDatatype.NOT_ALLOWED

  def checkFormat(literal: String, context: ValidationContext): Boolean =
    literal == ""

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    if lexicalValue == "" then lexicalValue
    else null

  def convertToLexicalValue(o: AnyRef, context: SerializationContext): String =
    if "" == o then ""
    else throw new IllegalArgumentException()

  def getJavaObjectType(): Class[?] = classOf[String]

object EmptyStringType:
  val theInstance: EmptyStringType = new EmptyStringType()
