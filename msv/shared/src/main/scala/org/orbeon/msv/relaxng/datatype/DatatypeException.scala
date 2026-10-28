package org.orbeon.msv.relaxng.datatype

/**
 * Signals Datatype related exceptions.
 *
 * @author <a href="mailto:jjc@jclark.com">James Clark</a>
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class DatatypeException(
  val index: Int,
  msg: String
) extends Exception(msg) {

  def this(msg: String) =
    this(DatatypeException.UNKNOWN, msg)

  /**
   * A constructor for those datatype libraries which don't support any
   * diagnostic information at all.
   */
  def this() =
    this(DatatypeException.UNKNOWN, null)

  /**
   * Gets the index of the content where the error occurred.
   * UNKNOWN can be returned to indicate that no index information
   * is available.
   */
  def getIndex: Int = index
}

object DatatypeException {
  val UNKNOWN: Int = -1
}
