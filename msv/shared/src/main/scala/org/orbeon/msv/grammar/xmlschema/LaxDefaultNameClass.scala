package org.orbeon.msv.grammar.xmlschema

import java.util.Set as JSet
import org.orbeon.msv.grammar.{DifferenceNameClass, NameClass, NameClassVisitor, SimpleNameClass}
import org.orbeon.msv.util.StringPair

/**
 * Special name class implementation used for the wild card of the "lax" mode.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class LaxDefaultNameClass(private val base: NameClass) extends NameClass:

  private val names: JSet[StringPair] = new java.util.HashSet[StringPair]()
  names.add(new StringPair(NameClass.NAMESPACE_WILDCARD, NameClass.LOCALNAME_WILDCARD))

  protected var equivalentNameClass: NameClass = scala.compiletime.uninitialized

  def visit(visitor: NameClassVisitor): AnyRef =
    if equivalentNameClass == null then
      var nc = base
      val items = names.toArray(new Array[StringPair](names.size()))
      var i = 0
      while i < items.length do
        if items(i).namespaceURI != NameClass.NAMESPACE_WILDCARD && items(i).localName != NameClass.LOCALNAME_WILDCARD then
          nc = new DifferenceNameClass(nc, new SimpleNameClass(items(i)))
        i += 1
      equivalentNameClass = nc

    equivalentNameClass.visit(visitor)

  def accepts(namespaceURI: String, localName: String): Boolean =
    base.accepts(namespaceURI, localName) &&
      !names.contains(new StringPair(namespaceURI, localName))

  def addName(namespaceURI: String, localName: String): Unit =
    names.add(new StringPair(namespaceURI, localName))
    names.add(new StringPair(namespaceURI, NameClass.LOCALNAME_WILDCARD))
    names.add(new StringPair(NameClass.NAMESPACE_WILDCARD, localName))
