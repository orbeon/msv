package org.orbeon.msv.relaxng.datatype.helpers

import org.orbeon.msv.relaxng.datatype.{DatatypeLibrary, DatatypeLibraryFactory}
import scala.collection.mutable.ListBuffer

/**
 * Discovers the datatype library implementation from the classpath.
 * <p/>
 * The call of the createDatatypeLibrary method finds an implementation
 * from a given datatype library URI at run-time.
 */
class DatatypeLibraryLoader extends DatatypeLibraryFactory {

  def createDatatypeLibrary(uri: String): DatatypeLibrary = {
    var found: DatatypeLibrary = null
    val it = DatatypeLibraryLoader.registeredFactories.iterator
    while (found == null && it.hasNext) {
      val factory = it.next()
      val lib = factory.createDatatypeLibrary(uri)
      if (lib != null) {
        found = lib
      }
    }
    if (found != null) found
    else DatatypeLibraryLoaderPlatform.createDatatypeLibrary(uri)
  }
}

object DatatypeLibraryLoader {
  private val factories = ListBuffer[DatatypeLibraryFactory]()

  def register(factory: DatatypeLibraryFactory): Unit = synchronized {
    if (!factories.contains(factory))
      factories += factory
  }

  def registeredFactories: Seq[DatatypeLibraryFactory] = synchronized {
    factories.toSeq
  }
}
