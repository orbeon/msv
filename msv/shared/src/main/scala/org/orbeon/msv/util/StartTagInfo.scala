package org.orbeon.msv.util

import org.orbeon.msv.datatype.xsd.WhiteSpaceProcessor
import org.orbeon.msv.grammar.{IDContextProvider, IDContextProvider2}
import org.orbeon.msv.grammar.util.IDContextProviderWrapper
import org.xml.sax.Attributes
import scala.compiletime.uninitialized

/**
 * immutable start tag information
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class StartTagInfo {
  var namespaceURI: String = uninitialized
  var localName: String = uninitialized
  var qName: String = uninitialized
  var attributes: Attributes = uninitialized
  var context: IDContextProvider2 = uninitialized

  @deprecated("use IDContextProvider2", "")
  def this(
    namespaceURI: String,
    localName: String,
    qName: String,
    attributes: Attributes,
    context: IDContextProvider
  ) = {
    this()
    reinit(namespaceURI, localName, qName, attributes, context)
  }

  def this(
    namespaceURI: String,
    localName: String,
    qName: String,
    attributes: Attributes,
    context: IDContextProvider2
  ) = {
    this()
    reinit(namespaceURI, localName, qName, attributes, context)
  }

  def this(
    namespaceURI: String,
    localName: String,
    qName: String,
    attributes: Attributes
  ) = {
    this(namespaceURI, localName, qName, attributes, null.asInstanceOf[IDContextProvider2])
  }

  @deprecated("use IDContextProvider2", "")
  def reinit(
    namespaceURI: String,
    localName: String,
    qName: String,
    attributes: Attributes,
    context: IDContextProvider
  ): Unit = {
    reinit(namespaceURI, localName, qName, attributes, IDContextProviderWrapper.create(context))
  }

  def reinit(
    namespaceURI: String,
    localName: String,
    qName: String,
    attributes: Attributes,
    context: IDContextProvider2
  ): Unit = {
    this.namespaceURI = namespaceURI
    this.localName = localName
    this.qName = qName
    this.attributes = attributes
    this.context = context
  }

  final def containsAttribute(attrName: String): Boolean =
    containsAttribute("", attrName)

  final def containsAttribute(namespaceURI: String, attrName: String): Boolean =
    attributes.getIndex(namespaceURI, attrName) != -1

  final def getAttribute(attrName: String): String =
    getAttribute("", attrName)

  final def getAttribute(namespaceURI: String, attrName: String): String =
    attributes.getValue(namespaceURI, attrName)

  final def getCollapsedAttribute(attrName: String): String = {
    val s = getAttribute(attrName)
    if (s == null) null
    else WhiteSpaceProcessor.collapse(s)
  }

  final def getDefaultedAttribute(attrName: String, defaultValue: String): String =
    getDefaultedAttribute("", attrName, defaultValue)

  final def getDefaultedAttribute(namespaceURI: String, attrName: String, defaultValue: String): String = {
    val v = getAttribute(namespaceURI, attrName)
    if (v != null) v
    else defaultValue
  }
}
