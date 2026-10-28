package org.orbeon.msv.iso_relax.dispatcher.impl

import java.util.{HashMap, Iterator, Map}
import org.orbeon.msv.iso_relax.dispatcher.{IslandSchema, SchemaProvider}

abstract class AbstractSchemaProviderImpl extends SchemaProvider {
  protected val schemata: Map[String, IslandSchema] = new HashMap[String, IslandSchema]()

  def addSchema(uri: String, s: IslandSchema): Unit = {
    if (schemata.containsKey(uri))
      throw new IllegalArgumentException()
    schemata.put(uri, s)
  }

  def getSchemaByNamespace(uri: String): IslandSchema =
    schemata.get(uri)

  def iterateNamespace(): Iterator[String] =
    schemata.keySet().iterator()

  def getSchemata: Array[IslandSchema] =
    schemata.values().toArray(new Array[IslandSchema](schemata.size()))
}
