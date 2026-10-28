package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{AttributeExp, DataExp, ElementExp, ListExp, ValueExp}

/**
 * primitive unit of XML instance.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class Token {
  /** returns true if the given ElementExp can consume this token */
  def `match`(p: ElementExp): Boolean = false
  def `match`(p: AttributeExp): Boolean = false
  /** returns true if the given DataExp can consume this token */
  def `match`(p: DataExp): Boolean = false
  def `match`(p: ValueExp): Boolean = false
  /** returns true if the given ListExp can consume this token */
  def `match`(p: ListExp): Boolean = false

  def matchExp(p: ElementExp): Boolean = `match`(p)
  def matchExp(p: AttributeExp): Boolean = `match`(p)
  def matchExp(p: DataExp): Boolean = `match`(p)
  def matchExp(p: ValueExp): Boolean = `match`(p)
  def matchExp(p: ListExp): Boolean = `match`(p)

  /** returns true if anyString pattern can consume this token */
  def matchAnyString(): Boolean = false

  /** checks if this token is ignorable. */
  def isIgnorable: Boolean = false
}
