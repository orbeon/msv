package org.orbeon.msv.iso_relax.verifier

/**
 * A compiled representation of a schema.
 */
trait Schema {
  @throws(classOf[VerifierConfigurationException])
  def newVerifier(): Verifier
}
