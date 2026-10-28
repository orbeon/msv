package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.{NameClass, NamespaceNameClass}

/**
 * parses &lt;nsName&gt; name class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NameClassNsNameState extends NameClassWithoutChildState:
  protected def makeNameClass(): NameClass =
    new NamespaceNameClass(getPropagatedNamespace())
