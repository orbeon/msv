package org.orbeon.msv.datatype.xsd.conformance

import org.orbeon.msv.datatype.xsd.TypeIncubator
import org.orbeon.msv.relaxng.datatype.DatatypeException

/**
 * Test pattern corresponding to one individual facet test.
 */
class SimpleTestPattern(
  val facetName: String,
  val facetValue: String,
  val answer: String,
  val expectedFailure: Boolean,
  val expectedFailureReason: String
) extends TestPattern:

  private var idx: Int = 0
  reset()

  override def totalCases(): Long = 1L

  override def reset(): Unit = idx = 0

  override def get(incubator: TypeIncubator): String =
    idx match
      case 0 =>
        incubator.addFacet(facetName, facetValue, false, DummyContextProvider)
        answer
      case _ =>
        throw new AssertionError("Invalid index in SimpleTestPattern")

  override def next(): Unit = idx += 1

  override def hasMore: Boolean = idx != 1

  override def isExpectedFailure: Boolean = idx == 0 && expectedFailure

  override def getExpectedFailureReason: String =
    if idx == 0 && expectedFailure then expectedFailureReason else null
