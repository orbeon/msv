package org.orbeon.msv.grammar

import java.util.{HashMap, Iterator, Map}

/**
 * Container of [[ReferenceExp]]. A map from name to ReferenceExp.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ReferenceContainer extends Serializable {
  protected val impl: Map[String, ReferenceExp] = new HashMap[String, ReferenceExp]()

  /**
   * gets or creates ReferenceExp object.
   */
  final def _getOrCreate(name: String): ReferenceExp = {
    val o = impl.get(name)
    if (o != null) return o

    val exp = createReference(name)
    impl.put(name, exp)
    exp
  }

  /** creates a new reference object with given name */
  protected def createReference(name: String): ReferenceExp

  /**
   * replaces the current ReferenceExp by newly specified reference exp.
   */
  def redefine(name: String, newExp: ReferenceExp): Unit = {
    if (impl.put(name, newExp) == null)
      throw new IllegalArgumentException()
  }

  /**
   * gets a referenced expression.
   */
  final def _get(name: String): ReferenceExp =
    impl.get(name)

  /** iterates all ReferenceExp in this container */
  final def iterator(): Iterator[ReferenceExp] =
    impl.values().iterator()

  /** obtains all items in this container. */
  final def getAll: Array[ReferenceExp] = {
    val r = new Array[ReferenceExp](size())
    impl.values().toArray(r)
    r
  }

  /** removes an object from this container. */
  final def remove(name: String): ReferenceExp =
    impl.remove(name)

  /** gets the number of ReferenceExps in this container. */
  final def size(): Int =
    impl.size()
}
