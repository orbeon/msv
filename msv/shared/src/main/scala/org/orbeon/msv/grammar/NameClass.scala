package org.orbeon.msv.grammar

import org.orbeon.msv.grammar.util.{NameClassCollisionChecker, NameClassComparator, NameClassSimplifier}
import org.orbeon.msv.util.StringPair

/**
 * validator of (namespaceURI,localPart) pair.
 *
 * This is equivalent to RELAX NG's "name class".
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class NameClass extends Serializable {
  /**
   * checks if this name class accepts given namespace:localName pair.
   *
   * @param namespaceURI
   *        namespace URI to be tested. If this value equals to
   *        NAMESPACE_WILDCARD, implementation must assume that
   *        valid namespace is specified. this twist will be used for
   *        error diagnosis.
   *
   * @param localName
   *        local part to be tested. As with namespaceURI, LOCALNAME_WILDCARD
   *        will acts as a wild card.
   *
   * @return
   *        true if the pair is accepted,
   *        false otherwise.
   */
  def accepts(namespaceURI: String, localName: String): Boolean

  final def accepts(name: StringPair): Boolean =
    accepts(name.namespaceURI, name.localName)

  /** Returns true if this name class is a superset of another name class. */
  final def includes(rhs: NameClass): Boolean = {
    val r = new NameClassComparator {
      protected def probe(uri: String, local: String): Unit = {
        if (!nc1.accepts(uri, local) && nc2.accepts(uri, local))
          throw eureka // this is not a super-set!
      }
    }.check(this, rhs)
    !r
  }

  /** Returns true if this name class doesn't accept anything. */
  def isNull: Boolean =
    !new NameClassCollisionChecker().check(this, NameClass.ALL)

  /**
   * Returns true if this name class represents the same set as the given name class.
   */
  final def isEqualTo(rhs: NameClass): Boolean = {
    val r = new NameClassComparator {
      protected def probe(uri: String, local: String): Unit = {
        val a = nc1.accepts(uri, local)
        val b = nc2.accepts(uri, local)
        if ((a && !b) || (!a && b)) throw eureka
      }
    }.check(this, rhs)
    !r
  }

  /**
   * Computes the equivalent but simple name class.
   */
  def simplify(): NameClass =
    NameClassSimplifier.simplify(this)

  /**
   * visitor pattern support
   */
  def visit(visitor: NameClassVisitor): AnyRef
}

object NameClass {
  /** wildcard should be accepted by any name class. */
  val NAMESPACE_WILDCARD: String = "*"
  val LOCALNAME_WILDCARD: String = "*"

  /** Computes the intersection of two name classes. */
  def intersection(lhs: NameClass, rhs: NameClass): NameClass =
    NameClassSimplifier.simplify(new DifferenceNameClass(lhs, new NotNameClass(rhs)))

  /** Computes the union of two name classes. */
  def union(lhs: NameClass, rhs: NameClass): NameClass =
    NameClassSimplifier.simplify(new ChoiceNameClass(lhs, rhs))

  /** name class that accepts everything. */
  lazy val ALL: NameClass = new AnyNameClass()

  /** Name class that accepts nothing. */
  lazy val NONE: NameClass = new NotNameClass(ALL)
}
