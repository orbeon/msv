package org.orbeon.msv.verifier.identity

import org.orbeon.msv.relaxng.datatype.Datatype
import org.xml.sax.SAXException

/**
 * Base abstract implementation of XPath matcher.
 *
 * XPath mathcer tracks the startElement event and the endElement event.
 * The characters event is also used by some derived classes.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class Matcher(val owner: IDConstraintChecker):

  @throws(classOf[SAXException])
  protected[identity] def startElement(namespaceURI: String, localName: String): Unit

  @throws(classOf[SAXException])
  protected[identity] def onAttribute(namespaceURI: String, localName: String, value: String, `type`: Datatype): Unit

  @throws(classOf[SAXException])
  protected[identity] def endElement(`type`: Datatype): Unit

  @throws(classOf[SAXException])
  protected[identity] def characters(buf: Array[Char], start: Int, len: Int): Unit = ()
