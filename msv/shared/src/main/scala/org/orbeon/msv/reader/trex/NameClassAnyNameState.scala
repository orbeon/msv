package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.NameClass

/**
 * parses &lt;anyName&gt; name class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NameClassAnyNameState extends NameClassWithoutChildState:
  protected def makeNameClass(): NameClass = NameClass.ALL
