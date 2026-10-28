package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.datatype.xsd.ngimpl.DataTypeLibraryImpl
import org.orbeon.msv.grammar.relaxng.datatype.{BuiltinDatatypeLibrary, CompatibilityDatatypeLibrary}
import org.orbeon.msv.reader.datatype.xsd.XSDVocabulary
import org.orbeon.msv.relaxng.datatype.{DatatypeLibrary, DatatypeLibraryFactory}
import org.orbeon.msv.relaxng.datatype.helpers.DatatypeLibraryLoader

/**
 * Default implementation of DatatypeLibraryFactory
 * 
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
class DefaultDatatypeLibraryFactory extends DatatypeLibraryFactory:
  private val loader: DatatypeLibraryFactory = new DatatypeLibraryLoader()
  private var xsdlib: DatatypeLibrary = null
  private var compatibilityLib: DatatypeLibrary = null

  def createDatatypeLibrary(namespaceURI: String): DatatypeLibrary =
    val lib = loader.createDatatypeLibrary(namespaceURI)
    if lib != null then return lib

    if namespaceURI == "" then
      BuiltinDatatypeLibrary.theInstance
    else if namespaceURI == XSDVocabulary.XMLSchemaNamespace || namespaceURI == XSDVocabulary.XMLSchemaNamespace2 then
      if xsdlib == null then
        xsdlib = new DataTypeLibraryImpl()
      xsdlib
    else if namespaceURI == CompatibilityDatatypeLibrary.namespaceURI then
      if compatibilityLib == null then
        compatibilityLib = new CompatibilityDatatypeLibrary()
      compatibilityLib
    else
      null
