package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.State
import org.orbeon.msv.util.StartTagInfo

/**
 * invokes State object that parses the document element.
 * 
 * This class is used to parse the first grammar.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RootState extends RootIncludedPatternState(null):
  private var simple: Boolean = false

  protected override def createChildState(tag: StartTagInfo): State =
    val trexReader = reader.asInstanceOf[TREXBaseReader]
    if tag.localName == "grammar" then
      trexReader.sfactory.grammar(null, tag)
    else
      val s = super.createChildState(tag)
      if s != null then
        trexReader.grammar = trexReader.sfactory.createGrammar(trexReader.pool, null)
        simple = true
      s

  override def onEndChild(exp: Expression): Unit =
    super.onEndChild(exp)
    val trexReader = reader.asInstanceOf[TREXBaseReader]
    if simple then
      trexReader.grammar.exp = exp
    trexReader.wrapUp()
