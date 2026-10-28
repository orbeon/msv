package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.{NameClass, SimpleNameClass}
import org.orbeon.msv.reader.{SequenceState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * Base implementation for ElementState and AttributeState
 * 
 * This class collects one name class and patterns
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class NameClassAndExpressionState extends SequenceState with NameClassOwner:
  protected var nameClass: NameClass = null

  /**
   * gets namespace URI to which this declaration belongs
   */
  protected def getNamespace(): String =
    reader.asInstanceOf[TREXBaseReader].targetNamespace

  protected override def startSelf(): Unit =
    super.startSelf()
    val name = startTag.getCollapsedAttribute("name")
    if name != null then
      val idx = name.indexOf(':')
      if idx != -1 then
        val s = reader.splitQName(name)
        if s == null then
          reader.reportError(TREXBaseReader.ERR_UNDECLARED_PREFIX, name)
          nameClass = new SimpleNameClass("", name)
        else
          nameClass = new SimpleNameClass(s(0), s(1), s(2))
      else
        nameClass = new SimpleNameClass(getNamespace(), name)

  def onEndChild(p: NameClass): Unit =
    if nameClass != null then
      reader.reportError(TREXBaseReader.ERR_MORE_THAN_ONE_NAMECLASS)
    nameClass = p

  protected override def createChildState(tag: StartTagInfo): State =
    if nameClass == null then
      var nextState = reader.asInstanceOf[TREXBaseReader].createNameClassChildState(this, tag)
      if nextState != null then
        return nextState

      nextState = reader.createExpressionChildState(this, tag)
      if nextState != null then
        reader.reportError(TREXBaseReader.ERR_MISSING_CHILD_NAMECLASS)
        nameClass = NameClass.ALL
        nextState
      else
        null
    else
      reader.createExpressionChildState(this, tag)

  protected override def endSelf(): Unit =
    if nameClass == null then
      reader.reportError(TREXBaseReader.ERR_MISSING_CHILD_NAMECLASS)
      nameClass = NameClass.ALL
    super.endSelf()
