package org.orbeon.msv.reader.datatype.xsd

import java.util.ArrayList as JArrayList
import org.orbeon.msv.reader.{IgnoreState, State}
import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.orbeon.msv.util.StartTagInfo

/**
 * State that parses &lt;union&gt; element and its children.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class UnionState protected[xsd] (val newTypeUri: String, val newTypeName: String)
  extends TypeState with XSTypeOwner:

  private val memberTypes: JArrayList[XSDatatypeExp] = new JArrayList[XSDatatypeExp]()

  protected def createChildState(tag: StartTagInfo): State =
    if startTag.namespaceURI != tag.namespaceURI then return null

    tag.localName match
      case "annotation" => new IgnoreState()
      case "simpleType" => new SimpleTypeState()
      case _            => null

  override protected def startSelf(): Unit =
    super.startSelf()
    val memberTypesAttr = startTag.getAttribute("memberTypes")
    if memberTypesAttr != null then
      val tokens = memberTypesAttr.split("\\s+").filter(_.nonEmpty)
      var i = 0
      while i < tokens.length do
        onEndChild(reader.asInstanceOf[XSDatatypeResolver].resolveXSDatatype(tokens(i)))
        i += 1

  def onEndChild(`type`: XSDatatypeExp): Unit =
    memberTypes.add(`type`)

  protected def makeType(): XSDatatypeExp =
    XSDatatypeExp.makeUnion(newTypeUri, newTypeName, memberTypes, reader)
