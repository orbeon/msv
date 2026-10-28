package org.orbeon.msv.datatype.xsd.conformance

import org.orbeon.msv.datatype.xsd.TypeIncubator
import org.orbeon.msv.relaxng.datatype.DatatypeException

/**
 * Tests every possible combination of child patterns.
 */
class FullCombinationPattern(rawChildren: Array[TestPattern], val mergeMode: Boolean) extends TestPattern:

  private val children: Array[TestPattern] = rawChildren.map(c => new FullCombinationPattern.PatternWithEmpty(c))
  private var noMore: Boolean = false
  reset()

  override def totalCases(): Long =
    var result = 1L
    for c <- children do
      result *= c.totalCases()
    result

  override def reset(): Unit =
    for c <- children do
      c.reset()
    noMore = false

  override def get(ti: TypeIncubator): String =
    var answer: String = null
    for c <- children do
      answer = TestPatternGenerator.merge(answer, c.get(ti), mergeMode)
    answer

  override def next(): Unit =
    var i = children.length - 1
    while i >= 0 do
      children(i).next()
      if children(i).hasMore then
        i += 1
        while i < children.length do
          children(i).reset()
          i += 1
        return
      i -= 1
    noMore = true

  override def hasMore: Boolean = !noMore

  override def isExpectedFailure: Boolean =
    children.exists(_.isExpectedFailure)

  override def getExpectedFailureReason: String =
    children.find(_.isExpectedFailure).flatMap(c => Option(c.getExpectedFailureReason)).orNull

object FullCombinationPattern:

  /** Adds an empty test case to the base pattern. */
  private class PatternWithEmpty(val base: TestPattern) extends TestPattern:
    private var mode: Int = 0
    reset()

    override def totalCases(): Long = base.totalCases() + 1L

    override def reset(): Unit =
      base.reset()
      mode = 0

    override def get(ti: TypeIncubator): String =
      mode match
        case 0 => base.get(ti)
        case 1 => null
        case _ => throw new AssertionError("Invalid mode in PatternWithEmpty")

    override def next(): Unit =
      mode match
        case 0 =>
          base.next()
          if !base.hasMore then mode = 1
        case 1 =>
          mode = 2
        case _ =>
          throw new AssertionError("Invalid mode in PatternWithEmpty.next()")

    override def hasMore: Boolean = mode != 2

    override def isExpectedFailure: Boolean =
      mode == 0 && base.isExpectedFailure

    override def getExpectedFailureReason: String =
      if mode == 0 then base.getExpectedFailureReason else null
