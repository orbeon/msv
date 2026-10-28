package org.orbeon.msv.grammar

/**
 * Visitor interface for [[NameClass]].
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait NameClassVisitor {
  def onChoice(nc: ChoiceNameClass): AnyRef
  def onAnyName(nc: AnyNameClass): AnyRef
  def onSimple(nc: SimpleNameClass): AnyRef
  def onNsName(nc: NamespaceNameClass): AnyRef
  def onNot(nc: NotNameClass): AnyRef
  def onDifference(nc: DifferenceNameClass): AnyRef
}
