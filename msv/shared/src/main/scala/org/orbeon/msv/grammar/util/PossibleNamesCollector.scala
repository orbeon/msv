package org.orbeon.msv.grammar.util

import java.util.{HashSet, Set => JSet}
import org.orbeon.msv.grammar.{AnyNameClass, ChoiceNameClass, DifferenceNameClass, NameClass, NameClassVisitor, NamespaceNameClass, NotNameClass, SimpleNameClass}
import org.orbeon.msv.util.StringPair

/**
 * computes the possible names.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class PossibleNamesCollector extends NameClassVisitor {
  import PossibleNamesCollector._

  private val names: JSet[StringPair] = new HashSet[StringPair]()

  def onChoice(nc: ChoiceNameClass): AnyRef = {
    nc.nc1.visit(this)
    nc.nc2.visit(this)
    null
  }

  def onAnyName(nc: AnyNameClass): AnyRef = {
    names.add(pairForAny)
    null
  }

  def onSimple(nc: SimpleNameClass): AnyRef = {
    names.add(new StringPair(nc.namespaceURI, nc.localName))
    null
  }

  def onNsName(nc: NamespaceNameClass): AnyRef = {
    names.add(new StringPair(nc.namespaceURI, MAGIC))
    null
  }

  def onNot(nc: NotNameClass): AnyRef = {
    names.add(pairForAny)
    nc.child.visit(this)
    null
  }

  def onDifference(nc: DifferenceNameClass): AnyRef = {
    nc.nc1.visit(this)
    nc.nc2.visit(this)
    null
  }
}

object PossibleNamesCollector {
  val MAGIC: String = "\u0000"
  private val pairForAny = new StringPair(MAGIC, MAGIC)

  def calc(nc: NameClass): JSet[StringPair] = {
    val col = new PossibleNamesCollector()
    nc.visit(col)
    col.names
  }
}
