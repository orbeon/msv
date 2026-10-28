package org.orbeon.msv.reader.trex.ng

import java.util.{HashSet, Set as JSet}
import org.orbeon.msv.grammar.{Expression, ReferenceExp}
import org.orbeon.msv.reader.{ExpressionOwner, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * parses &lt;include&gt; element as a child of &lt;grammar&gt; element.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IncludeMergeState extends org.orbeon.msv.reader.trex.IncludeMergeState with ExpressionOwner:
  protected override def createChildState(tag: StartTagInfo): State =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    if tag.localName == "define" then rngReader.getStateFactory.redefine(this, tag)
    else if tag.localName == "start" then rngReader.getStateFactory.redefineStart(this, tag)
    else null

  /** set of ReferenceExps which are redefined by this inclusion. */
  private val redefinedPatterns: JSet[ReferenceExp] = new HashSet[ReferenceExp]()

  def onEndChild(child: Expression): Unit =
    child match
      case ref: ReferenceExp => redefinedPatterns.add(ref)
      case _ =>

  override def endSelf(): Unit =
    val rngReader = reader.asInstanceOf[RELAXNGReader]

    val patterns = redefinedPatterns.toArray(new Array[ReferenceExp](redefinedPatterns.size()))
    val old = new Array[RELAXNGReader.RefExpParseInfo](patterns.length)

    for i <- patterns.indices do
      val info = rngReader.getRefExpParseInfo(patterns(i))
      old(i) = new RELAXNGReader.RefExpParseInfo()
      old(i).set(info)
      info.haveHead = false
      info.combineMethod = null
      info.redefinition = RELAXNGReader.RefExpParseInfo.originalNotFoundYet

    super.endSelf()

    for i <- patterns.indices do
      val info = rngReader.getRefExpParseInfo(patterns(i))
      if info.redefinition == RELAXNGReader.RefExpParseInfo.originalNotFoundYet then
        rngReader.reportError(RELAXNGReader.ERR_REDEFINING_UNDEFINED, patterns(i).name)
      rngReader.getRefExpParseInfo(patterns(i)).set(old(i))
