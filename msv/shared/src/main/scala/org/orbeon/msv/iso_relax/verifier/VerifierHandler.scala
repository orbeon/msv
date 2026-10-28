package org.orbeon.msv.iso_relax.verifier

import org.xml.sax.ContentHandler

trait VerifierHandler extends ContentHandler {
  @throws(classOf[IllegalStateException])
  def isValid: Boolean
}
