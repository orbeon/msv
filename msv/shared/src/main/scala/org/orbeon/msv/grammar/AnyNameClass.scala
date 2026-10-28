package org.orbeon.msv.grammar

/**
 * a NameClass that accepts any name.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class AnyNameClass extends NameClass {
  def accepts(namespaceURI: String, localName: String): Boolean = true

  def visit(visitor: NameClassVisitor): AnyRef = visitor.onAnyName(this)

  override def toString: String = "*:*"

  private def readResolve(): AnyRef = AnyNameClass.theInstance
}

object AnyNameClass {
  val theInstance: NameClass = new AnyNameClass()
}
