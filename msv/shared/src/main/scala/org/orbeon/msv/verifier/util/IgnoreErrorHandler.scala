package org.orbeon.msv.verifier.util

import org.xml.sax.ErrorHandler
import org.xml.sax.SAXParseException

/**
 * do-nothing implementation of ErrorHandler.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IgnoreErrorHandler extends ErrorHandler:

  @throws(classOf[SAXParseException])
  def fatalError(e: SAXParseException): Unit = throw e

  def error(error: SAXParseException): Unit = ()

  def warning(warning: SAXParseException): Unit = ()
