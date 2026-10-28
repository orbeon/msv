package org.orbeon.msv.verifier

import org.orbeon.msv.util.StartTagInfo

/**
 * Instances of this class is attached to [[ValidityViolation]] to
 * provide detailed information about the error.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ErrorInfo

object ErrorInfo:

  abstract class ElementErrorInfo(
    val qName: String,
    val namespaceURI: String,
    val localName: String
  ) extends ErrorInfo:
    def this(sti: StartTagInfo) = this(sti.qName, sti.namespaceURI, sti.localName)

  class BadText(val literal: String) extends ErrorInfo:
    def this(buf: java.lang.StringBuffer) = this(buf.toString)

  class BadTagName(qn: String, ns: String, loc: String) extends ElementErrorInfo(qn, ns, loc):
    def this(sti: StartTagInfo) = this(sti.qName, sti.namespaceURI, sti.localName)

  class BadAttribute(
    sti: StartTagInfo,
    val attQName: String,
    val attNamespaceURI: String,
    val attLocalName: String,
    val attValue: String
  ) extends ElementErrorInfo(sti)

  class MissingAttribute(qn: String, ns: String, loc: String) extends ElementErrorInfo(qn, ns, loc):
    def this(sti: StartTagInfo) = this(sti.qName, sti.namespaceURI, sti.localName)

  class IncompleteContentModel(qn: String, ns: String, loc: String) extends ElementErrorInfo(qn, ns, loc):
    def this(sti: StartTagInfo) = this(sti.qName, sti.namespaceURI, sti.localName)
