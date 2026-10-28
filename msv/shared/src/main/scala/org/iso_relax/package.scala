package org

package object iso_relax {
  object verifier {
    type Schema = org.orbeon.msv.iso_relax.verifier.Schema
    type Verifier = org.orbeon.msv.iso_relax.verifier.Verifier
    val Verifier = org.orbeon.msv.iso_relax.verifier.Verifier
    type VerifierConfigurationException = org.orbeon.msv.iso_relax.verifier.VerifierConfigurationException
    type VerifierException = org.orbeon.msv.iso_relax.verifier.VerifierException
    type VerifierFactory = org.orbeon.msv.iso_relax.verifier.VerifierFactory
    val VerifierFactory = org.orbeon.msv.iso_relax.verifier.VerifierFactory
    type VerifierFactoryLoader = org.orbeon.msv.iso_relax.verifier.VerifierFactoryLoader
    type VerifierFilter = org.orbeon.msv.iso_relax.verifier.VerifierFilter
    type VerifierHandler = org.orbeon.msv.iso_relax.verifier.VerifierHandler

    object impl {
      type VerifierImpl = org.orbeon.msv.iso_relax.verifier.impl.VerifierImpl
      val VerifierImpl = org.orbeon.msv.iso_relax.verifier.impl.VerifierImpl
      type VerifierFilterImpl = org.orbeon.msv.iso_relax.verifier.impl.VerifierFilterImpl
      type ForkContentHandler = org.orbeon.msv.iso_relax.verifier.impl.ForkContentHandler
      val ForkContentHandler = org.orbeon.msv.iso_relax.verifier.impl.ForkContentHandler
    }
  }

  object dispatcher {
    type AttributesDecl = org.orbeon.msv.iso_relax.dispatcher.AttributesDecl
    type AttributesVerifier = org.orbeon.msv.iso_relax.dispatcher.AttributesVerifier
    type Dispatcher = org.orbeon.msv.iso_relax.dispatcher.Dispatcher
    val Dispatcher = org.orbeon.msv.iso_relax.dispatcher.Dispatcher
    type ElementDecl = org.orbeon.msv.iso_relax.dispatcher.ElementDecl
    type IslandSchema = org.orbeon.msv.iso_relax.dispatcher.IslandSchema
    type IslandSchemaReader = org.orbeon.msv.iso_relax.dispatcher.IslandSchemaReader
    type IslandVerifier = org.orbeon.msv.iso_relax.dispatcher.IslandVerifier
    type SchemaProvider = org.orbeon.msv.iso_relax.dispatcher.SchemaProvider

    object impl {
      type AbstractSchemaProviderImpl = org.orbeon.msv.iso_relax.dispatcher.impl.AbstractSchemaProviderImpl
      type DispatcherImpl = org.orbeon.msv.iso_relax.dispatcher.impl.DispatcherImpl
      val DispatcherImpl = org.orbeon.msv.iso_relax.dispatcher.impl.DispatcherImpl
      type IgnoredSchema = org.orbeon.msv.iso_relax.dispatcher.impl.IgnoredSchema
      val IgnoredSchema = org.orbeon.msv.iso_relax.dispatcher.impl.IgnoredSchema
      type IgnoreVerifier = org.orbeon.msv.iso_relax.dispatcher.impl.IgnoreVerifier
    }
  }
}
