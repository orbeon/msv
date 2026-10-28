package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.ElementExp

/**
 * special Token that matchs any element.
 *
 * this token is used only for error recovery, to compute
 * "residual of elements of concern"(EoCR).
 *
 * EoCR is defined as follows
 *
 * <PRE>
 * EoCR(exp) := exp/e1 | exp/e2 | ... | exp/en
 *
 * {ei} = elements of concern
 * exp/ei = residual(exp,ei)
 * '|' represents choice
 * </PRE>
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class AnyElementToken private () extends ElementToken(null):
  override def `match`(exp: ElementExp): Boolean = true

object AnyElementToken:
  /**
   * use this singleton instance instead of creating an object.
   */
  val theInstance: Token = new AnyElementToken()
