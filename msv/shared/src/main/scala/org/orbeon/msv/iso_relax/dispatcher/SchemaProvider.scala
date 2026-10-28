package org.orbeon.msv.iso_relax.dispatcher

import java.util.Iterator

trait SchemaProvider {
  def createTopLevelVerifier(): IslandVerifier
  def getSchemaByNamespace(uri: String): IslandSchema
  def iterateNamespace(): Iterator[String]
  def getSchemata: Array[IslandSchema]
}
