package org.orbeon.msv.grammar.xmlschema

import org.orbeon.msv.grammar.{Expression, OtherExp}

/**
 * Used to mark a occurrence constraint which cannot
 * be easily represented by primitive expressions.
 * 
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
class OccurrenceExp(
  preciseExp: Expression,
  val maxOccurs: Int,
  val minOccurs: Int,
  val itemExp: Expression
) extends OtherExp(preciseExp):

  override def toString: String =
    itemExp.toString + "[" + minOccurs + "," + (if maxOccurs == -1 then "inf" else String.valueOf(maxOccurs)) + "]"
