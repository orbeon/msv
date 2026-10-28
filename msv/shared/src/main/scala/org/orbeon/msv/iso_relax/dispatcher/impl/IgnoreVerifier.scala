package org.orbeon.msv.iso_relax.dispatcher.impl

import org.orbeon.msv.iso_relax.dispatcher.{Dispatcher, ElementDecl, IslandVerifier}
import org.xml.sax.Attributes
import org.xml.sax.SAXException
import org.xml.sax.helpers.DefaultHandler
import scala.compiletime.uninitialized

final class IgnoreVerifier(
  private val namespaceToIgnore: String,
  private val rules: Array[ElementDecl]
) extends DefaultHandler with IslandVerifier {

  private var dispatcher: Dispatcher = uninitialized

  def endIsland(): Array[ElementDecl] = rules

  def endChildIsland(uri: String, assignedLabels: Array[ElementDecl]): Unit = ()

  def setDispatcher(disp: Dispatcher): Unit = {
    this.dispatcher = disp
  }

  @throws(classOf[SAXException])
  override def startElement(
    namespaceURI: String,
    localName: String,
    qName: String,
    attributes: Attributes
  ): Unit = {
    if (namespaceToIgnore == namespaceURI)
      return

    val is = dispatcher.getSchemaProvider.getSchemaByNamespace(namespaceURI)
    if (is == null)
      return

    val iv = is.createNewVerifier(namespaceURI, is.getElementDecls)
    dispatcher.switchVerifier(iv)
    iv.startElement(namespaceURI, localName, qName, attributes)
  }
}
