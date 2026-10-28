package org.orbeon.msv.datatype.xsd.conformance

import org.orbeon.msv.datatype.xsd.TypeIncubator
import org.orbeon.msv.relaxng.datatype.DatatypeException

/**
 * Chooses one from child patterns.
 */
class ChoiceTestPattern(val children: Array[TestPattern]) extends TestPattern:

  private var idx: Int = 0
  reset()

  override def totalCases(): Long =
    var result = 0L
    for c <- children do
      result += c.totalCases()
    result

  override def reset(): Unit =
    for c <- children do
      c.reset()
    idx = 0

  override def get(ti: TypeIncubator): String =
    children(idx).get(ti)

  override def next(): Unit =
    val prev = idx
    children(idx).next()
    var continueLoop = true
    while continueLoop do
      idx = (idx + 1) % children.length
      if children(idx).hasMore || idx == prev then
        continueLoop = false

    if !children(idx).hasMore then
      idx = -1

  override def hasMore: Boolean = idx != -1

  override def isExpectedFailure: Boolean =
    idx != -1 && children(idx).isExpectedFailure

  override def getExpectedFailureReason: String =
    if idx == -1 then null else children(idx).getExpectedFailureReason
