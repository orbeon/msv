package org.orbeon.msv.reader.datatype.xsd

import org.orbeon.msv.datatype.xsd.StringType
import org.orbeon.msv.reader.{GrammarReader, IgnoreState, State}
import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.orbeon.msv.util.StartTagInfo

/**
 * state that parses &lt;restriction&gt; element.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RestrictionState protected[xsd] (val newTypeUri: String, val newTypeName: String)
  extends TypeWithOneChildState with FacetStateParent:

  protected var incubator: XSTypeIncubator = scala.compiletime.uninitialized
  final def getIncubator: XSTypeIncubator = incubator

  override protected def annealType(baseType: XSDatatypeExp): XSDatatypeExp =
    incubator.derive(newTypeUri, newTypeName)

  override def onEndChild(child: XSDatatypeExp): Unit =
    super.onEndChild(child)
    createTypeIncubator()

  private def createTypeIncubator(): Unit =
    incubator = `type`.createIncubator()

  override protected def startSelf(): Unit =
    super.startSelf()
    val base = startTag.getAttribute("base")
    if base != null then
      onEndChild(reader.asInstanceOf[XSDatatypeResolver].resolveXSDatatype(base))

  protected def createChildState(tag: StartTagInfo): State =
    if startTag.namespaceURI != tag.namespaceURI then return null

    tag.localName match
      case "annotation" => new IgnoreState()
      case "simpleType" => new SimpleTypeState()
      case name if FacetState.facetNames.contains(name) =>
        if incubator == null then
          reader.reportError(GrammarReader.ERR_MISSING_ATTRIBUTE, "restriction", "base")
          onEndChild(new XSDatatypeExp(StringType.theInstance, reader.pool))
        new FacetState()
      case _ => null
