package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.{NameClass, NotNameClass}

/**
 * parses &lt;not&gt; name class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NameClassNotState extends NameClassWithChildState:
  protected def castNameClass(halfCastedNameClass: NameClass, child: NameClass): NameClass =
    if halfCastedNameClass != null then
      // <not> only allows one child.
      reader.reportError(TREXBaseReader.ERR_MORE_THAN_ONE_NAMECLASS)
      halfCastedNameClass
    else
      new NotNameClass(child)
