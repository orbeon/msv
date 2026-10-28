package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.{ChoiceNameClass, NameClass}

/**
 * parses &lt;choice&gt; name class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NameClassChoiceState extends NameClassWithChildState:
  protected def castNameClass(halfCasted: NameClass, newChild: NameClass): NameClass =
    if halfCasted == null then newChild // first item
    else new ChoiceNameClass(halfCasted, newChild)
