package org.orbeon.msv.relaxng.datatype.helpers

import org.orbeon.msv.relaxng.datatype.*

/**
 * Dummy implementation of {@link DatatypeStreamingValidator}.
 * 
 * <p>
 * This implementation can be used as a quick hack when the performance
 * of streaming validation is not important. And this implementation
 * also shows you how to implement the DatatypeStreamingValidator interface.
 * 
 * <p>
 * Typical usage would be:
 * <PRE><XMP>
 * class MyDatatype implements Datatype {
 *     ....
 *     public DatatypeStreamingValidator createStreamingValidator( ValidationContext context ) {
 *         return new StreamingValidatorImpl(this,context);
 *     }
 *     ....
 * }
 * </XMP></PRE>
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
final class StreamingValidatorImpl(
  /** Datatype object that creates this streaming validator. */
  private val baseType: Datatype,
  /** The current context. */
  private val context: ValidationContext
) extends DatatypeStreamingValidator {

  /** This buffer accumulates characters. */
  private val buffer = new StringBuilder()

  def addCharacters(buf: Array[Char], start: Int, len: Int): Unit =
    buffer.appendAll(buf, start, len)

  def isValid: Boolean =
    baseType.isValid(buffer.toString(), context)

  @throws(classOf[DatatypeException])
  def checkValid(): Unit =
    baseType.checkValid(buffer.toString(), context)
}
