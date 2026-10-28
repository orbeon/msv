package org.orbeon.msv.reader

import org.xml.sax.{Attributes, Locator}
import org.xml.sax.helpers.AttributesImpl
import org.orbeon.msv.util.StartTagInfo

/**
 * base interface of the most of parsing states.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class SimpleState extends State:

  /** checks if this element belongs to the grammar. */
  protected def isGrammarElement(tag: StartTagInfo): Boolean =
    reader.isGrammarElement(tag)

  def startElement(namespaceURI: String, localName: String, qName: String, atts: Attributes): Unit =
    val tag = new StartTagInfo(namespaceURI, localName, qName, new AttributesImpl(atts))
    if isGrammarElement(tag) then
      val nextState = createChildState(tag)
      if nextState != null then
        reader.pushState(nextState, this, tag)
        return
      reader.reportError(GrammarReader.ERR_MALPLACED_ELEMENT, tag.qName)
    else
      if parentState == null then
        reader.reportError(GrammarReader.ERR_MALPLACED_ELEMENT, tag.qName)
        reader.reportError(GrammarReader.WRN_MAYBE_WRONG_NAMESPACE, tag.namespaceURI)

    reader.pushState(new IgnoreState(), this, tag)

  /** creates appropriate child state object for this element */
  protected def createChildState(tag: StartTagInfo): State

  final def endElement(namespaceURI: String, localName: String, qName: String): Unit =
    val prevLoc = reader.locator
    try
      reader.locator = this.location
      endSelf()
    finally
      reader.locator = prevLoc
    reader.popState()

  /**
   * this method is called in endElement method
   * derived classes should perform clean up and instantiate
   * model.
   */
  protected def endSelf(): Unit = ()
