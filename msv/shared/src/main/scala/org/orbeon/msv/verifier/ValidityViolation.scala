package org.orbeon.msv.verifier

import org.xml.sax.{Locator, SAXParseException}

/**
 * Contains information about where and how validity violation was happened.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ValidityViolation(loc: Locator, msg: String, val errorInfo: ErrorInfo) extends SAXParseException(msg, loc):
  def getErrorInfo: ErrorInfo = errorInfo
