package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.ExpressionWithoutChildState
import org.orbeon.msv.util.StringPair
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeException}

/**
 * parses &lt;value&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ValueState extends ExpressionWithoutChildState:
  protected val text: java.lang.StringBuffer = new java.lang.StringBuffer()

  override def characters(buf: Array[Char], from: Int, len: Int): Unit =
    text.append(buf, from, len)

  override def ignorableWhitespace(buf: Array[Char], from: Int, len: Int): Unit =
    text.append(buf, from, len)

  protected def makeExpression(): Expression =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    val typeName = startTag.getCollapsedAttribute("type")

    var dt: Datatype = null
    var typeFullName: StringPair = null

    if typeName == null then
      try
        dt = rngReader.resolveDataTypeLibrary("").createDatatype("token")
        typeFullName = new StringPair("", "token")
      catch
        case e: DatatypeException =>
          throw new InternalError(e.getMessage)
    else
      dt = rngReader.resolveDataType(typeName)
      typeFullName = new StringPair(rngReader.datatypeLibURI, typeName)

    val value = dt.createValue(text.toString, rngReader)
    if value == null then
      rngReader.reportError(RELAXNGReader.ERR_BAD_DATA_VALUE, typeName, text.toString.trim)
      Expression.nullSet
    else
      rngReader.pool.createValue(dt, typeFullName, value)
