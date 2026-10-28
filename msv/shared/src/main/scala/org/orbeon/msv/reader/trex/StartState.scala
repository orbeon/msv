package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.SequenceState

/**
 * parses &lt;start&gt; element.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class StartState extends SequenceState:
  protected final def getReader(): TREXBaseReader = reader.asInstanceOf[TREXBaseReader]

  protected override def annealExpression(exp: Expression): Expression =
    var e = exp
    val name = startTag.getAttribute("name")
    if name != null then
      val ref = getReader().grammar.namedPatterns.getOrCreate(name)
      ref.exp = e
      e = ref

    getReader().grammar.exp = e
    null
