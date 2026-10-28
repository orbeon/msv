package org.orbeon.msv.verifier

import org.xml.sax.SAXParseException

/**
 * Exception that signals error was fatal and recovery was not possible.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ValidationUnrecoverableException(vv: SAXParseException)
  extends SAXParseException(vv.getMessage, vv.getPublicId, vv.getSystemId, vv.getLineNumber, vv.getColumnNumber, vv)
