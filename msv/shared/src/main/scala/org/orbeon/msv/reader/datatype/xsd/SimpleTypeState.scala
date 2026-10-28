package org.orbeon.msv.reader.datatype.xsd

import org.orbeon.msv.datatype.xsd.XSDatatype
import org.orbeon.msv.reader.{GrammarReader, IgnoreState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * State that parses &lt;simpleType&gt; element and its children.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SimpleTypeState extends TypeWithOneChildState:

  protected def createChildState(tag: StartTagInfo): State =
    if startTag.namespaceURI != tag.namespaceURI then return null

    val name = startTag.getAttribute("name")
    val uri = getTargetNamespaceUri

    tag.localName match
      case "annotation"  => new IgnoreState()
      case "restriction" => new RestrictionState(uri, name)
      case "list"        => new ListState(uri, name)
      case "union"       => new UnionState(uri, name)
      case _             => null

  override protected def annealType(dt: XSDatatypeExp): XSDatatypeExp =
    val finalValueStr = startTag.getAttribute("final")
    if finalValueStr != null then
      val finalValue = getFinalValue(finalValueStr)
      dt.createFinalizedType(finalValue, reader)
    else
      dt

  def getFinalValue(list: String): Int =
    var finalValue = 0
    val tokens = list.split("\\s+").filter(_.nonEmpty)
    var i = 0
    while i < tokens.length do
      val token = tokens(i)
      token match
        case "#all" =>
          finalValue |= (XSDatatype.DERIVATION_BY_LIST |
            XSDatatype.DERIVATION_BY_RESTRICTION |
            XSDatatype.DERIVATION_BY_UNION)
        case "restriction" =>
          finalValue |= XSDatatype.DERIVATION_BY_RESTRICTION
        case "list" =>
          finalValue |= XSDatatype.DERIVATION_BY_LIST
        case "union" =>
          finalValue |= XSDatatype.DERIVATION_BY_UNION
        case _ =>
          reader.reportError(GrammarReader.ERR_ILLEGAL_FINAL_VALUE, token)
          return 0
      i += 1
    finalValue
