package org.orbeon.msv.iso_relax.verifier

private[verifier] object VerifierFactoryPlatform:

  def findFactory(language: String): Option[VerifierFactory] =
    None
