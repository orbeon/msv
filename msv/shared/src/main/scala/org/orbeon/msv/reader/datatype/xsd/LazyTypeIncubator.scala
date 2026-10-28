package org.orbeon.msv.reader.datatype.xsd

import java.util.{Iterator as JIterator, List as JList}
import org.orbeon.msv.datatype.xsd.{TypeIncubator, XSDatatype}
import org.orbeon.msv.reader.GrammarReader
import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

/**
 * Lazy XSTypeIncubator
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class LazyTypeIncubator(private val baseType: XSDatatypeExp, private val reader: GrammarReader) extends XSTypeIncubator:

  private val facets: JList[Facet] = new java.util.LinkedList[Facet]()

  def addFacet(name: String, strValue: String, fixed: Boolean, context: ValidationContext): Unit =
    facets.add(new Facet(name, strValue, fixed, context))

  def derive(nsUri: String, localName: String): XSDatatypeExp =
    val facetSize = facets.size()
    if facetSize == 0 then return baseType

    new XSDatatypeExp(nsUri, localName, reader, new XSDatatypeExp.Renderer:
      def render(context: XSDatatypeExp.RenderingContext): XSDatatype =
        val ti = new TypeIncubator(baseType.getType(context))
        val itr: JIterator[Facet] = facets.iterator()
        var i = 0
        while i < facetSize do
          val f = itr.next()
          ti.addFacet(f.name, f.value, f.fixed, f.context)
          i += 1
        ti.derive(nsUri, localName)
    )

  private class Facet(val name: String, val value: String, val fixed: Boolean, val context: ValidationContext)
