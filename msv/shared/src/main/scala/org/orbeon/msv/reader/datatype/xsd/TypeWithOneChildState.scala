package org.orbeon.msv.reader.datatype.xsd

import org.orbeon.msv.datatype.xsd.StringType
import org.orbeon.msv.reader.GrammarReader
import org.orbeon.msv.relaxng.datatype.DatatypeException

/**
 * State which has at most one TypeState as its child.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class TypeWithOneChildState extends TypeState with XSTypeOwner:

  protected var `type`: XSDatatypeExp = scala.compiletime.uninitialized

  def onEndChild(child: XSDatatypeExp): Unit =
    if `type` != null then
      reader.reportError(GrammarReader.ERR_MORE_THAN_ONE_CHILD_TYPE)
    else
      `type` = child

  protected final def makeType(): XSDatatypeExp =
    if `type` == null then
      reader.reportError(GrammarReader.ERR_MISSING_CHILD_TYPE)
      new XSDatatypeExp(StringType.theInstance, reader.pool)
    else
      annealType(`type`)

  @throws(classOf[DatatypeException])
  protected def annealType(dt: XSDatatypeExp): XSDatatypeExp = dt
