package org.orbeon.msv.verifier.util

import org.xml.sax.ErrorHandler
import org.xml.sax.SAXParseException

/**
 * default implementation of ErrorHandler.
 *
 * If an error is found, this implementation will throw it
 * as an exception so that no further processing of the document will be performed.
 *
 * <p>
 * All warnings are ignored.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ErrorHandlerImpl extends ErrorHandler:

  @throws(classOf[SAXParseException])
  def fatalError(error: SAXParseException): Unit =
    throw error

  @throws(classOf[SAXParseException])
  def error(error: SAXParseException): Unit =
    throw error

  def warning(warning: SAXParseException): Unit = ()

object ErrorHandlerImpl:
  /**
   * the singleton instance of this object. This class doesn't have any
   * internal state so
   */
  val theInstance: ErrorHandler = new ErrorHandlerImpl()
