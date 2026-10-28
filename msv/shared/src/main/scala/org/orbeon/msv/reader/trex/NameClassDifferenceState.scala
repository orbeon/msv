package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.{DifferenceNameClass, NameClass}

/**
 * parses &lt;difference&gt; name class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NameClassDifferenceState extends NameClassWithChildState:
  protected def castNameClass(halfCasted: NameClass, newChild: NameClass): NameClass =
    if halfCasted == null then newChild // first item
    else new DifferenceNameClass(halfCasted, newChild)
