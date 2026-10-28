package org.orbeon.msv.iso_relax.verifier

import org.xml.sax.SAXException

class VerifierException(message: String, e: Exception) extends SAXException(message, e) {
  def this(message: String) = this(message, null)
  def this(e: Exception) = this(if (e != null) e.getMessage else null, e)
}
