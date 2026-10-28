package org.orbeon.msv.verifier.jarv

import org.orbeon.msv.iso_relax.verifier.{VerifierConfigurationException, VerifierHandler}
import org.orbeon.msv.verifier.IVerifier
import org.xml.sax.{ErrorHandler, XMLReader}

class VerifierImpl @throws(classOf[VerifierConfigurationException]) (
  private val verifier: IVerifier,
  reader: XMLReader
) extends org.orbeon.msv.iso_relax.verifier.impl.VerifierImpl:

  this.reader = reader

  override protected def prepareXMLReader(): Unit = ()

  override def setErrorHandler(handler: ErrorHandler): Unit =
    super.setErrorHandler(handler)
    verifier.setErrorHandler(handler)

  override def getVerifierHandler: VerifierHandler = verifier
