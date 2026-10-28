package org.orbeon.msv.reader.datatype.xsd

import org.orbeon.msv.reader.{IgnoreState, State}
import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.orbeon.msv.util.StartTagInfo

/**
 * state that parses &lt;list&gt; element of XSD.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ListState protected[xsd] (val newTypeUri: String, val newTypeName: String)
  extends TypeWithOneChildState:

  override protected def annealType(itemType: XSDatatypeExp): XSDatatypeExp =
    XSDatatypeExp.makeList(newTypeUri, newTypeName, itemType, reader)

  override protected def startSelf(): Unit =
    super.startSelf()
    val itemType = startTag.getAttribute("itemType")
    if itemType != null then
      onEndChild(reader.asInstanceOf[XSDatatypeResolver].resolveXSDatatype(itemType))

  protected def createChildState(tag: StartTagInfo): State =
    if startTag.namespaceURI != tag.namespaceURI then return null

    tag.localName match
      case "annotation" => new IgnoreState()
      case "simpleType" => new SimpleTypeState()
      case _            => null
