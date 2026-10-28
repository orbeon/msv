package org.orbeon.msv.iso_relax.dispatcher

import org.xml.sax.{ContentHandler, SAXException}

trait IslandVerifier extends ContentHandler {
  def setDispatcher(disp: Dispatcher): Unit

  @throws(classOf[SAXException])
  def endIsland(): Array[ElementDecl]

  @throws(classOf[SAXException])
  def endChildIsland(uri: String, assignedLabels: Array[ElementDecl]): Unit
}
