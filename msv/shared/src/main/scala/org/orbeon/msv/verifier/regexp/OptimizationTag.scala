package org.orbeon.msv.verifier.regexp

import java.util.{Hashtable, Map}
import org.orbeon.msv.grammar.{ElementExp, Expression}
import scala.compiletime.uninitialized

/**
 * this object will be added to Expression.verifierTag
 * to speed up typical validation.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class OptimizationTag {
  import OptimizationTag._

  var stringCareLevel: Int = STRING_NOTCOMPUTED

  val simpleElementTokenResidual: Map[AnyRef, AnyRef] = new Hashtable[AnyRef, AnyRef]()

  val transitions: Map[AnyRef, AnyRef] = new Hashtable[AnyRef, AnyRef]()

  var attributePrunedExpression: Expression = uninitialized
}

object OptimizationTag {
  val STRING_NOTCOMPUTED: Int = -1

  final class OwnerAndCont(val owner: ElementExp, val continuation: Expression)
}
