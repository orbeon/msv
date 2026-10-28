package org.orbeon.msv.verifier

import org.orbeon.msv.iso_relax.verifier.VerifierHandler
import org.orbeon.msv.relaxng.datatype.Datatype
import org.xml.sax.{ErrorHandler, Locator}

/**
 * Interface of verifier.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait IVerifier extends VerifierHandler:

  def isValid: Boolean

  def getCurrentElementType: AnyRef

  def getLastCharacterType: Array[Datatype]

  def setPanicMode(usePanicMode: Boolean): Unit

  def getLocator: Locator

  def getErrorHandler: ErrorHandler

  def setErrorHandler(handler: ErrorHandler): Unit
