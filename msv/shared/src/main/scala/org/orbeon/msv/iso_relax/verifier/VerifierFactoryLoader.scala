package org.orbeon.msv.iso_relax.verifier

trait VerifierFactoryLoader {
  def createFactory(schemaLanguage: String): VerifierFactory
}
