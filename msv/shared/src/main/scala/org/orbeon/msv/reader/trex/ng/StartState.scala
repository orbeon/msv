package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.grammar.ReferenceExp
import org.orbeon.msv.reader.GrammarReader

/**
 * parses &lt;start&gt; declaration.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class StartState extends DefineState:
  protected override def getReference(): ReferenceExp =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    if startTag.containsAttribute("name") then
      reader.reportError(GrammarReader.ERR_DISALLOWED_ATTRIBUTE, startTag.qName, "name")
      null
    else
      rngReader.getGrammar
