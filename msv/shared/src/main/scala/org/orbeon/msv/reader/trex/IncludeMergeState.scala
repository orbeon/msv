package org.orbeon.msv.reader.trex

import org.orbeon.msv.reader.{AbortException, SimpleState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * &lt;include&gt; element as an immediate child of &lt;grammar&gt; element.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IncludeMergeState extends SimpleState:
  protected override def createChildState(tag: StartTagInfo): State = null

  protected override def endSelf(): Unit =
    val href = startTag.getAttribute("href")
    if href == null then
      reader.reportError(org.orbeon.msv.reader.GrammarReader.ERR_MISSING_ATTRIBUTE, "include", "href")
    else
      try
        val trexReader = reader.asInstanceOf[TREXBaseReader]
        trexReader.switchSource(this, href, trexReader.sfactory.includedGrammar())
      catch
        case _: AbortException =>

    super.endSelf()
