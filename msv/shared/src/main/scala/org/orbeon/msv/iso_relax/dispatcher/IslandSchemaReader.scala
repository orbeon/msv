package org.orbeon.msv.iso_relax.dispatcher

import org.xml.sax.ContentHandler

trait IslandSchemaReader extends ContentHandler {
  def getSchema: IslandSchema
}
