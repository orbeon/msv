package org.orbeon.msv.verifier.regexp

import java.util.{HashSet as JHashSet, Set as JSet}
import org.orbeon.msv.grammar.{DataExp, ListExp, ValueExp}
import org.orbeon.msv.util.DatatypeRef

/**
 * special StringToken that acts as a wild card.
 *
 * This object is used for error recovery. It collects all TypedStringExps
 * that ate the token.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class StringRecoveryToken(
  base: StringToken,
  /**
   * TypedStringExps and ListExps that
   * rejected this token are collected into this set.
   */
  val failedExps: JSet[AnyRef]
) extends StringToken(base.resCalc, base.literal, base.context, null):

  def this(base: StringToken) =
    this(base, new JHashSet[AnyRef]())

  override def `match`(exp: DataExp): Boolean =
    if super.`match`(exp) then
      true
    else
      // this datatype didn't accept me. so record it for diagnosis.
      failedExps.add(exp)
      true

  override def `match`(exp: ValueExp): Boolean =
    if super.`match`(exp) then
      true
    else
      // this datatype didn't accept me. so record it for diagnosis.
      failedExps.add(exp)
      true

  override def `match`(exp: ListExp): Boolean =
    super.`match`(exp)
    true

  override protected def createChildStringToken(literal: String, dtRef: DatatypeRef): StringToken =
    new StringRecoveryToken(
      new StringToken(resCalc, literal, context, dtRef)
    )
