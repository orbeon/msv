package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.grammar.{Expression, ReferenceExp}
import org.xml.sax.Locator

/**
 * parses &lt;define&gt; declaration.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DefineState extends org.orbeon.msv.reader.trex.DefineState:
  private var prevNamedPattern: RELAXNGReader.RefExpParseInfo = null
  private var previousDirectReference: Boolean = false

  protected override def startSelf(): Unit =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    super.startSelf()

    prevNamedPattern = rngReader.currentNamedPattern
    previousDirectReference = rngReader.directReference

    rngReader.directReference = true

    val exp = getReference()
    if exp == null then
      rngReader.currentNamedPattern = null
    else
      rngReader.currentNamedPattern = rngReader.getRefExpParseInfo(exp)
      if rngReader.currentNamedPattern.redefinition != RELAXNGReader.RefExpParseInfo.notBeingRedefined then
        rngReader.currentNamedPattern = null

  protected override def endSelf(): Unit =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    rngReader.currentNamedPattern = prevNamedPattern
    rngReader.directReference = previousDirectReference
    super.endSelf()

  protected def doCombine(baseExp: ReferenceExp, newExp: Expression, combine: String): Expression =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    val info = rngReader.getRefExpParseInfo(baseExp)

    if combine == null then
      if info.haveHead then
        rngReader.reportError(org.orbeon.msv.reader.trex.TREXBaseReader.ERR_COMBINE_MISSING, baseExp.name)
        return baseExp.exp
      info.haveHead = true
    else
      if info.combineMethod == null then
        info.combineMethod = combine.trim
        if info.combineMethod != "choice" && info.combineMethod != "interleave" then
          rngReader.reportError(org.orbeon.msv.reader.trex.TREXBaseReader.ERR_BAD_COMBINE, info.combineMethod)
      else if info.combineMethod != combine then
        rngReader.reportError(
          Array[Locator](location, rngReader.getDeclaredLocationOf(baseExp)),
          RELAXNGReader.ERR_INCONSISTENT_COMBINE,
          Array[AnyRef](baseExp.name)
        )
        info.combineMethod = null
        return baseExp.exp

    if baseExp.exp == null then
      return newExp

    if info.redefinition != RELAXNGReader.RefExpParseInfo.notBeingRedefined then
      info.redefinition = RELAXNGReader.RefExpParseInfo.originalFound
      return baseExp.exp

    if info.combineMethod == "choice" then
      rngReader.pool.createChoice(baseExp.exp, newExp)
    else if info.combineMethod == "interleave" then
      rngReader.pool.createInterleave(baseExp.exp, newExp)
    else
      null
