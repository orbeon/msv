package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.grammar.*

/**
 * parses &lt;attribute&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeState extends org.orbeon.msv.reader.trex.AttributeState:
  protected override def endSelf(): Unit =
    super.endSelf()

    val rngReader = reader.asInstanceOf[RELAXNGReader]
    rngReader.restrictionChecker.checkNameClass(nameClass)

    nameClass.visit(new NameClassVisitor {
      def onAnyName(nc: AnyNameClass): AnyRef = null
      def onSimple(nc: SimpleNameClass): AnyRef =
        if nc.namespaceURI == AttributeState.infosetURI then
          rngReader.reportError(RELAXNGReader.ERR_INFOSET_URI_ATTRIBUTE)
        if nc.namespaceURI.isEmpty && nc.localName == "xmlns" then
          rngReader.reportError(RELAXNGReader.ERR_XMLNS_ATTRIBUTE)
        null

      def onNsName(nc: NamespaceNameClass): AnyRef =
        if nc.namespaceURI == AttributeState.infosetURI then
          rngReader.reportError(RELAXNGReader.ERR_INFOSET_URI_ATTRIBUTE)
        null

      def onNot(nc: NotNameClass): AnyRef =
        nc.child.visit(this)
        null

      def onDifference(nc: DifferenceNameClass): AnyRef =
        nc.nc1.visit(this)
        nc.nc2.visit(this)
        null

      def onChoice(nc: ChoiceNameClass): AnyRef =
        nc.nc1.visit(this)
        nc.nc2.visit(this)
        null
    })

object AttributeState:
  val infosetURI = "http://www.w3.org/2000/xmlns"
