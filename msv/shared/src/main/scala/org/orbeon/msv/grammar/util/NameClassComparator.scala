package org.orbeon.msv.grammar.util

import org.orbeon.msv.grammar.{AnyNameClass, ChoiceNameClass, DifferenceNameClass, NameClass, NameClassVisitor, NamespaceNameClass, NotNameClass, SimpleNameClass}
import scala.compiletime.uninitialized

/**
 * Abstract implementation of a function
 * NameClass x NameClass -> boolean
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class NameClassComparator extends NameClassVisitor {
  protected var nc1: NameClass = uninitialized
  protected var nc2: NameClass = uninitialized

  protected val eureka: RuntimeException = new RuntimeException()

  def check(_new: NameClass, _old: NameClass): Boolean = {
    try {
      nc1 = _new
      nc2 = _old
      _old.visit(this)
      _new.visit(this)
      false
    } catch {
      case e: RuntimeException if e eq eureka =>
        true
    }
  }

  protected def probe(uri: String, local: String): Unit

  private val MAGIC = "\u0000"

  def onAnyName(nc: AnyNameClass): AnyRef = {
    probe(MAGIC, MAGIC)
    null
  }

  def onNsName(nc: NamespaceNameClass): AnyRef = {
    probe(nc.namespaceURI, MAGIC)
    null
  }

  def onSimple(nc: SimpleNameClass): AnyRef = {
    probe(nc.namespaceURI, nc.localName)
    null
  }

  def onNot(nc: NotNameClass): AnyRef = {
    nc.child.visit(this)
    null
  }

  def onDifference(nc: DifferenceNameClass): AnyRef = {
    nc.nc1.visit(this)
    nc.nc2.visit(this)
    null
  }

  def onChoice(nc: ChoiceNameClass): AnyRef = {
    nc.nc1.visit(this)
    nc.nc2.visit(this)
    null
  }
}
