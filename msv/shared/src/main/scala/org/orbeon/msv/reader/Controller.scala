package org.orbeon.msv.reader

import java.io.IOException
import javax.xml.parsers.ParserConfigurationException
import org.xml.sax.{ErrorHandler, InputSource, Locator, SAXException, SAXParseException}
import org.xml.sax.helpers.LocatorImpl

/**
 * Internal view of GrammarReaderController.
 * 
 * This class wraps a GrammarReaderController and
 * adds several convenient methods for the caller.
 */
class Controller(val core: GrammarReaderController) extends GrammarReaderController with ErrorHandler:

  def getCore: GrammarReaderController = core

  private var _hadError: Boolean = false

  def hadError: Boolean = _hadError

  final def setErrorFlag(): Unit =
    _hadError = true

  def resolveEntity(p: String, s: String): InputSource =
    core.resolveEntity(p, s)

  def warning(locs: Array[Locator], errorMessage: String): Unit =
    core.warning(locs, errorMessage)

  def error(locs: Array[Locator], errorMessage: String, nestedException: Exception): Unit =
    setErrorFlag()
    core.error(locs, errorMessage, nestedException)

  def error(errorMessage: String, nestedException: Exception): Unit =
    error(Array.empty[Locator], errorMessage, nestedException)

  def fatalError(spe: SAXParseException): Unit =
    error(spe)

  def error(spe: SAXParseException): Unit =
    error(getLocator(spe), spe.getMessage, spe.getException)

  def warning(spe: SAXParseException): Unit =
    warning(getLocator(spe), spe.getMessage)

  def error(e: IOException, source: Locator): Unit =
    error(Array(source), e.getMessage, e)

  def error(e: SAXException, source: Locator): Unit =
    if e.getException != null && e.getException.isInstanceOf[RuntimeException] then
      throw e.getException.asInstanceOf[RuntimeException]

    if e.isInstanceOf[SAXParseException] then
      error(e.asInstanceOf[SAXParseException])
    else
      error(Array(source), e.getMessage, e)

  def error(e: ParserConfigurationException, source: Locator): Unit =
    error(Array(source), e.getMessage, e)

  protected def getLocator(spe: SAXParseException): Array[Locator] =
    val loc = new LocatorImpl()
    loc.setColumnNumber(spe.getColumnNumber)
    loc.setLineNumber(spe.getLineNumber)
    loc.setSystemId(spe.getSystemId)
    loc.setPublicId(spe.getPublicId)
    Array[Locator](loc)
