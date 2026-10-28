package org.orbeon.msv.reader.trex.ng

/**
 * parses &lt;grammar&gt; element.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class GrammarState extends org.orbeon.msv.reader.trex.GrammarState:
  protected override def startSelf(): Unit =
    super.startSelf()

    val rngReader = reader.asInstanceOf[RELAXNGReader]
    if rngReader.currentNamedPattern != null then
      if rngReader.directReference then
        rngReader.currentNamedPattern.directRefs.add(newGrammar)
      else
        rngReader.currentNamedPattern.indirectRefs.add(newGrammar)
