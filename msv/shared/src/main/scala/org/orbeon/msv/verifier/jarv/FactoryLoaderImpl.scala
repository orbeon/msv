package org.orbeon.msv.verifier.jarv

import org.orbeon.msv.iso_relax.verifier.{VerifierFactory, VerifierFactoryLoader}
import org.orbeon.msv.reader.trex.ng.RELAXNGReader
import org.orbeon.msv.reader.xmlschema.XMLSchemaReader

class FactoryLoaderImpl extends VerifierFactoryLoader:

  def createFactory(language: String): VerifierFactory =
    if language == null then null
    else if language == RELAXNGReader.RELAXNGNamespace then
      new RELAXNGFactoryImpl()
    else if language == XMLSchemaReader.XMLSchemaNamespace || language == XMLSchemaReader.XMLSchemaNamespace_old then
      new XSFactoryImpl()
    else if language == "relax" then
      new TheFactoryImpl()
    else
      null
