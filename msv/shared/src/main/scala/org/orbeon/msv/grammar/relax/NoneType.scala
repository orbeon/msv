package org.orbeon.msv.grammar.relax

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.datatype.xsd.{BuiltinAtomicType, SimpleURType, XSDatatype}
import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * 'none' datatype of RELAX.
 *
 * this type accepts nothing.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NoneType private () extends BuiltinAtomicType("none"):

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  def isFacetApplicable(facetName: String): Int = XSDatatype.NOT_ALLOWED

  def checkFormat(literal: String, context: ValidationContext): Boolean = false

  def _createValue(lexicalValue: String, context: ValidationContext): AnyRef = null

  def convertToLexicalValue(o: AnyRef, context: SerializationContext): String =
    throw new IllegalArgumentException()

  def getJavaObjectType(): Class[?] = classOf[AnyRef]

object NoneType:
  val theInstance: NoneType = new NoneType()
