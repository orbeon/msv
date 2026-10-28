package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.datatype.ErrorDatatypeLibrary
import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.{ExpressionOwner, ExpressionState, GrammarReader, State}
import org.orbeon.msv.util.{StartTagInfo, StringPair}
import org.orbeon.msv.relaxng.datatype.{DatatypeBuilder, DatatypeException}

/**
 * parses &lt;data&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DataState extends ExpressionState with ExpressionOwner:
  var typeBuilder: DatatypeBuilder = null
  protected var baseTypeName: StringPair = null
  protected var except: Expression = null

  protected override def createChildState(tag: StartTagInfo): State =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    if tag.localName == "except" then rngReader.getStateFactory.dataExcept(this, tag)
    else if tag.localName == "param" then rngReader.getStateFactory.dataParam(this, tag)
    else null

  protected override def startSelf(): Unit =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    super.startSelf()

    val localName = startTag.getCollapsedAttribute("type")
    if localName == null then
      reader.reportError(GrammarReader.ERR_MISSING_ATTRIBUTE, "data", "type")
    else
      baseTypeName = new StringPair(rngReader.datatypeLibURI, localName)
      try
        typeBuilder = rngReader.getCurrentDatatypeLibrary.createDatatypeBuilder(localName)
      catch
        case dte: DatatypeException =>
          reader.reportError(RELAXNGReader.ERR_UNDEFINED_DATATYPE_1, localName, dte.getMessage)

    if typeBuilder == null then
      typeBuilder = ErrorDatatypeLibrary.theInstance

  def onEndChild(child: Expression): Unit =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    if except != null then
      rngReader.reportError(RELAXNGReader.ERR_MULTIPLE_EXCEPT)
    except = child

  protected def makeExpression(): Expression =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    try
      val exc = if except == null then Expression.nullSet else except
      rngReader.pool.createData(typeBuilder.createDatatype(), baseTypeName, exc)
    catch
      case dte: DatatypeException =>
        rngReader.reportError(RELAXNGReader.ERR_INVALID_PARAMETERS, dte.getMessage)
        Expression.nullSet
