package org.orbeon.msv.iso_relax.verifier

class VerifierConfigurationException(message: String, cause: Throwable) extends Exception(message, cause) {
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(if (cause != null) cause.getMessage else null, cause)

  def getException: Exception =
    cause match {
      case e: Exception => e
      case _            => null
    }

  def getCauseException: Exception = getException
}
