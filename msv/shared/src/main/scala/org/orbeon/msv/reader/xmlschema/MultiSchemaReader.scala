package org.orbeon.msv.reader.xmlschema

import org.xml.sax.InputSource
import org.xml.sax.helpers.LocatorImpl
import org.orbeon.msv.grammar.xmlschema.XMLSchemaGrammar
import org.orbeon.msv.util.Util

/**
 * A utility class that
 * reads multiple XML Schema documents and combines them into one schema object.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class MultiSchemaReader(private val reader: XMLSchemaReader):

  reader.setDocumentLocator(new LocatorImpl())

  private var finalized: Boolean = false

  def getReader: XMLSchemaReader = reader

  /**
   * Obtains the parsed grammar.
   */
  def getResult: XMLSchemaGrammar =
    finish()
    reader.getResult

  def parse(source: String): Unit =
    parse(Util.getInputSource(source))

  /**
   * Parses an additional schema.
   *
   * The caller can call this method many times to parse
   * multiple schemas.
   *
   * If possible, the caller should set the system Id to the InputSource.
   */
  def parse(is: InputSource): Unit =
    reader.switchSource(
      is,
      new RootIncludedSchemaState(reader.sfactory.schemaHead(null))
    )

  /**
   * This method should be called when all the schemas are parsed.
   */
  def finish(): Unit =
    if !finalized then
      finalized = true
      reader.wrapUp()
