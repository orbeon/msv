package org.xml.sax.helpers

import java.util.{ArrayList as JArrayList, Collections, Enumeration, HashMap as JHashMap, HashSet as JHashSet, List as JList, Map as JMap, NoSuchElementException, Set as JSet}

/**
 * Encapsulate Namespace logic for use by SAX drivers or verification engines.
 */
class NamespaceSupport {

  import NamespaceSupport.*

  private final class Context {
    var prefixTable: JMap[String, String] = new JHashMap[String, String]()
    var uriTable: JMap[String, String] = new JHashMap[String, String]()
    var declarations: JList[String] = new JArrayList[String]()
    var parent: Context = null

    def setParent(parent: Context): Unit = {
      this.parent = parent
      this.declarations.clear()
      this.prefixTable.clear()
      this.uriTable.clear()
      if (parent != null) {
        this.prefixTable.putAll(parent.prefixTable)
        this.uriTable.putAll(parent.uriTable)
      }
    }

    def declarePrefix(prefix: String, uri: String): Unit = {
      declarations.add(prefix)
      if (uri.isEmpty) {
        prefixTable.remove(prefix)
      } else {
        prefixTable.put(prefix, uri)
        uriTable.put(uri, prefix)
      }
    }

    def getURI(prefix: String): String =
      prefixTable.get(prefix)

    def getPrefix(uri: String): String =
      uriTable.get(uri)

    def getDeclaredPrefixes: Enumeration[String] =
      Collections.enumeration(declarations)

    def getPrefixes: Enumeration[String] =
      Collections.enumeration(prefixTable.keySet())

    def getPrefixes(uri: String): Enumeration[String] = {
      val prefixes = new JArrayList[String]()
      val it = prefixTable.entrySet().iterator()
      while (it.hasNext) {
        val entry = it.next()
        if (entry.getValue == uri) {
          prefixes.add(entry.getKey)
        }
      }
      Collections.enumeration(prefixes)
    }
  }

  private var contexts: Array[Context] = new Array[Context](32)
  private var contextPos: Int = 0
  private var currentContext: Context = null

  reset()

  def reset(): Unit = {
    contexts = new Array[Context](32)
    contextPos = 0
    contexts(contextPos) = new Context()
    currentContext = contexts(contextPos)
    currentContext.declarePrefix("xml", XMLNS)
  }

  def pushContext(): Unit = {
    val max = contexts.length
    contextPos += 1
    if (contextPos >= max) {
      val newContexts = new Array[Context](max * 2)
      System.arraycopy(contexts, 0, newContexts, 0, max)
      contexts = newContexts
    }
    if (contexts(contextPos) == null) {
      contexts(contextPos) = new Context()
    }
    contexts(contextPos).setParent(currentContext)
    currentContext = contexts(contextPos)
  }

  def popContext(): Unit = {
    if (contextPos <= 0) {
      throw new NoSuchElementException("no context to pop")
    }
    contextPos -= 1
    currentContext = contexts(contextPos)
  }

  def declarePrefix(prefix: String, uri: String): Boolean = {
    if (prefix == "xml" || prefix == "xmlns") {
      false
    } else {
      currentContext.declarePrefix(prefix, uri)
      true
    }
  }

  def processName(qName: String, parts: Array[String], isAttribute: Boolean): Array[String] = {
    val myParts = if (parts == null) new Array[String](3) else parts
    val index = qName.indexOf(':')
    if (index < 0) {
      if (isAttribute) {
        if (qName == "xmlns") {
          myParts(0) = NSDECL
          myParts(1) = "xmlns"
          myParts(2) = "xmlns"
        } else {
          myParts(0) = ""
          myParts(1) = qName
          myParts(2) = qName
        }
      } else {
        val defaultURI = currentContext.getURI("")
        if (defaultURI == null) {
          myParts(0) = ""
        } else {
          myParts(0) = defaultURI
        }
        myParts(1) = qName
        myParts(2) = qName
      }
      myParts
    } else {
      val prefix = qName.substring(0, index)
      val localName = qName.substring(index + 1)
      val uri = if (prefix == "xmlns") {
        NSDECL
      } else {
        currentContext.getURI(prefix)
      }
      if (uri == null) {
        null
      } else {
        myParts(0) = uri
        myParts(1) = localName
        myParts(2) = qName
        myParts
      }
    }
  }

  def getURI(prefix: String): String =
    currentContext.getURI(prefix)

  def getPrefixes: Enumeration[?] =
    currentContext.getPrefixes

  def getPrefix(uri: String): String =
    currentContext.getPrefix(uri)

  def getPrefixes(uri: String): Enumeration[?] =
    currentContext.getPrefixes(uri)

  def getDeclaredPrefixes: Enumeration[?] =
    currentContext.getDeclaredPrefixes
}

object NamespaceSupport {
  val XMLNS: String = "http://www.w3.org/XML/1998/namespace"
  val NSDECL: String = "http://www.w3.org/xmlns/2000/"
}
