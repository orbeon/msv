package org.orbeon.msv.grammar.util

import java.util.{HashSet, Set => JSet}
import org.orbeon.msv.grammar.{AnyNameClass, ChoiceNameClass, DifferenceNameClass, NameClass, NamespaceNameClass, NotNameClass, SimpleNameClass}
import org.orbeon.msv.util.StringPair

/**
 * Minimizes a name class.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
object NameClassSimplifier {
  def simplify(nc: NameClass): NameClass = {
    val possibleNames: JSet[StringPair] = PossibleNamesCollector.calc(nc)
    val MAGIC = PossibleNamesCollector.MAGIC

    val uris: JSet[String] = new HashSet[String]()

    val itr = possibleNames.iterator()
    while (itr.hasNext) {
      val name = itr.next()
      if (name.localName != MAGIC) {
        if (nc.accepts(name) == nc.accepts(name.namespaceURI, MAGIC)) {
          itr.remove()
        } else if (name.namespaceURI != MAGIC) {
          uris.add(name.namespaceURI)
        }
      } else if (name.namespaceURI != MAGIC) {
        if (nc.accepts(name) == nc.accepts(MAGIC, MAGIC)) {
          itr.remove()
        } else {
          uris.add(name.namespaceURI)
        }
      }
    }

    if (!nc.accepts(MAGIC, MAGIC))
      possibleNames.remove(new StringPair(MAGIC, MAGIC))

    var result: NameClass = null
    val jtr = uris.iterator()
    while (jtr.hasNext) {
      val uri = jtr.next()
      var local: NameClass = null

      val itr2 = possibleNames.iterator()
      while (itr2.hasNext) {
        val name = itr2.next()
        if (name.namespaceURI == uri && name.localName != MAGIC) {
          if (local == null)
            local = new SimpleNameClass(name)
          else
            local = new ChoiceNameClass(local, new SimpleNameClass(name))
        }
      }

      if (possibleNames.contains(new StringPair(uri, MAGIC))) {
        if (local == null)
          local = new NamespaceNameClass(uri)
        else
          local = new DifferenceNameClass(new NamespaceNameClass(uri), local)
      }

      if (local != null) {
        if (result == null)
          result = local
        else
          result = new ChoiceNameClass(result, local)
      }
    }

    if (nc.accepts(MAGIC, MAGIC)) {
      if (result == null)
        result = NameClass.ALL
      else
        result = new NotNameClass(result)
    }

    if (result == null)
      result = NameClass.NONE

    result
  }
}
