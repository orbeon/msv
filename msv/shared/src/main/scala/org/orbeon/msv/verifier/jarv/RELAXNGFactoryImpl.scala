package org.orbeon.msv.verifier.jarv

import javax.xml.parsers.SAXParserFactory
import org.orbeon.msv.grammar.Grammar
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.reader.trex.ng.RELAXNGReader
import org.orbeon.msv.relaxng.datatype.DatatypeLibraryFactory
import org.xml.sax.{InputSource, SAXNotRecognizedException, SAXNotSupportedException}

class RELAXNGFactoryImpl(factory: SAXParserFactory) extends FactoryImpl(factory):

  def this() = this {
    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(true)
    f
  }

  private var datatypeLibraryFactory: DatatypeLibraryFactory = null

  override protected def parse(is: InputSource, controller: GrammarReaderController): Grammar =
    val reader = new RELAXNGReader(controller, factory)
    if datatypeLibraryFactory != null then
      reader.setDatatypeLibraryFactory(datatypeLibraryFactory)
    reader.parse(is)
    reader.getResult()

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  override def getProperty(name: String): AnyRef =
    if name == RELAXNGFactoryImpl.PropName then
      datatypeLibraryFactory
    else
      super.getProperty(name)

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  override def setProperty(name: String, value: AnyRef): Unit =
    if name == RELAXNGFactoryImpl.PropName then
      datatypeLibraryFactory = value.asInstanceOf[DatatypeLibraryFactory]
    else
      super.setProperty(name, value)

object RELAXNGFactoryImpl:
  final val PropName = "datatypeLibraryFactory"
