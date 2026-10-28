package org.orbeon.msv.reader.trex

import org.orbeon.msv.datatype.xsd.WhiteSpaceProcessor
import org.orbeon.msv.grammar.{NameClass, SimpleNameClass}

/**
 * parses &lt;name&gt; name class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NameClassNameState extends NameClassWithoutChildState:
  protected val text: java.lang.StringBuffer = new java.lang.StringBuffer()

  override def characters(buf: Array[Char], from: Int, len: Int): Unit =
    text.append(buf, from, len)

  override def ignorableWhitespace(buf: Array[Char], from: Int, len: Int): Unit =
    text.append(buf, from, len)

  protected def makeNameClass(): NameClass =
    val name = WhiteSpaceProcessor.collapse(text.toString)

    val idx = name.indexOf(':')
    if idx < 0 then
      // if the name is NCName
      new SimpleNameClass(getPropagatedNamespace(), name)
    else
      // if it's a QName, resolve it.
      val qname = reader.splitQName(name)
      if qname == null then
        reader.reportError(TREXBaseReader.ERR_UNDECLARED_PREFIX, name)
        new SimpleNameClass("", name)
      else
        new SimpleNameClass(qname(0), qname(1), qname(2))
