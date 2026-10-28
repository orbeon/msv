package org.orbeon.msv.iso_relax.verifier

import org.xml.sax.XMLFilter

trait VerifierFilter extends XMLFilter {
  @throws(classOf[IllegalStateException])
  def isValid: Boolean
}
