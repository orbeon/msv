package org.orbeon.msv.reader.datatype.xsd

import org.orbeon.msv.reader.{ChildlessState, GrammarReader}
import org.orbeon.msv.relaxng.datatype.DatatypeException

/**
 * state that reads facets.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class FacetState extends ChildlessState:

  override protected def startSelf(): Unit =
    super.startSelf()
    val value = startTag.getAttribute("value")

    if value == null then
      reader.reportError(GrammarReader.ERR_MISSING_ATTRIBUTE, startTag.localName, "value")
    else
      try
        parentState.asInstanceOf[FacetStateParent].getIncubator.addFacet(
          startTag.localName,
          value,
          "true" == startTag.getAttribute("fixed"),
          reader
        )
      catch
        case e: DatatypeException =>
          reader.reportError(e, GrammarReader.ERR_BAD_TYPE, e.getMessage)

object FacetState:
  val facetNames: Set[String] = Set(
    "length", "minLength", "maxLength", "pattern", "enumeration",
    "maxInclusive", "minInclusive", "maxExclusive", "minExclusive",
    "whiteSpace", "fractionDigits", "totalDigits"
  )
