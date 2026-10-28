package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.grammar.ReferenceExp

/**
 * parses &lt;ref&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RefState(parentRef: Boolean) extends org.orbeon.msv.reader.trex.RefState(parentRef):
  protected override def wrapUp(r: ReferenceExp): Unit =
    super.wrapUp(r)
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    if rngReader.currentNamedPattern != null then
      if rngReader.directReference then
        rngReader.currentNamedPattern.directRefs.add(r)
      else
        rngReader.currentNamedPattern.indirectRefs.add(r)
