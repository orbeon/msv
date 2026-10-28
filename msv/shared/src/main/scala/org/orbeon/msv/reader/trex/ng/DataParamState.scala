package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.reader.{ChildlessState, GrammarReader}
import org.orbeon.msv.relaxng.datatype.DatatypeException

/**
 * parses &lt;param&gt; element inside &lt;data&gt; element.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DataParamState extends ChildlessState:
  protected val text: java.lang.StringBuffer = new java.lang.StringBuffer()

  override def characters(buf: Array[Char], from: Int, len: Int): Unit =
    text.append(buf, from, len)

  override def ignorableWhitespace(buf: Array[Char], from: Int, len: Int): Unit =
    text.append(buf, from, len)

  protected override def endSelf(): Unit =
    val facet = startTag.getAttribute("name")
    if facet == null then
      reader.reportError(GrammarReader.ERR_MISSING_ATTRIBUTE, startTag.qName, "name")
    else
      try
        parentState.asInstanceOf[DataState].typeBuilder.addParameter(
          facet, text.toString, reader
        )
      catch
        case dte: DatatypeException =>
          reader.reportError(RELAXNGReader.ERR_BAD_FACET, facet, dte.getMessage)

    super.endSelf()
