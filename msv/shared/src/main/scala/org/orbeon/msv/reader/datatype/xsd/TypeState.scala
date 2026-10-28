package org.orbeon.msv.reader.datatype.xsd

import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl
import org.orbeon.msv.datatype.xsd.StringType
import org.orbeon.msv.reader.{ExpressionOwner, GrammarReader, IgnoreState, SimpleState, State}
import org.orbeon.msv.reader.datatype.TypeOwner
import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.orbeon.msv.util.StartTagInfo

/**
 * Base implementation for those states which produce a type object
 * as its parsing result.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class TypeState extends SimpleState:

  /** Gets the parent state as TypeOwner. */
  private def getParent: XSTypeOwner =
    if parentState.isInstanceOf[XSTypeOwner] then
      parentState.asInstanceOf[XSTypeOwner]
    else null

  def getTargetNamespaceUri: String =
    val p = getParent
    if p != null then p.getTargetNamespaceUri
    else ""

  override def endSelf(): Unit =
    super.endSelf()
    val tpe = _makeType()

    if parentState.isInstanceOf[XSTypeOwner] then
      parentState.asInstanceOf[XSTypeOwner].onEndChild(tpe)
      return

    if parentState.isInstanceOf[TypeOwner] then
      parentState.asInstanceOf[TypeOwner].onEndChildType(tpe, tpe.name)
      return

    if parentState.isInstanceOf[ExpressionOwner] then
      parentState.asInstanceOf[ExpressionOwner].onEndChild(tpe)
      return

    throw new Error(parentState.getClass.getName + " doesn't implement any of TypeOwner")

  def _makeType(): XSDatatypeExp =
    try
      makeType()
    catch
      case be: DatatypeException =>
        reader.reportError(be, GrammarReader.ERR_BAD_TYPE)
        new XSDatatypeExp(StringType.theInstance, reader.pool)

  @throws(classOf[DatatypeException])
  protected def makeType(): XSDatatypeExp

  override def startElement(namespaceURI: String, localName: String, qName: String, atts: Attributes): Unit =
    val tag = new StartTagInfo(namespaceURI, localName, qName, new AttributesImpl(atts))
    val nextState = createChildState(tag)
    if nextState != null then
      reader.pushState(nextState, this, tag)
      return

    reader.reportError(GrammarReader.ERR_MALPLACED_ELEMENT, tag.qName)
    reader.pushState(new IgnoreState(), this, tag)
