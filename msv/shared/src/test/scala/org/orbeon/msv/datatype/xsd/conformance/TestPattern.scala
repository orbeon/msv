package org.orbeon.msv.datatype.xsd.conformance

import org.orbeon.msv.datatype.xsd.TypeIncubator
import org.orbeon.msv.relaxng.datatype.DatatypeException

/**
 * Test pattern interface for enumerating facet combinations and their predicted results.
 */
trait TestPattern:
  /** Returns the total number of test cases to be generated. */
  def totalCases(): Long

  /** Restarts generating test cases. */
  def reset(): Unit

  /**
   * Applies the current facet combination to the incubator and returns the predicted answer.
   */
  def get(ti: TypeIncubator): String

  /** Generates the next test case. */
  def next(): Unit

  /** True if more test cases remain. */
  def hasMore: Boolean

  /** True if the current pattern is marked as an expected failure. */
  def isExpectedFailure: Boolean

  /** Optional human-readable reason for expected failure. */
  def getExpectedFailureReason: String
