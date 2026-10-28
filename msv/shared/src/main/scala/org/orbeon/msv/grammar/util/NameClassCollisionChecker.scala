package org.orbeon.msv.grammar.util

/**
 * Computes if two name classes collide or not.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class NameClassCollisionChecker extends NameClassComparator {
  protected def probe(uri: String, local: String): Unit = {
    if (nc1.accepts(uri, local) && nc2.accepts(uri, local))
      throw eureka
  }
}
