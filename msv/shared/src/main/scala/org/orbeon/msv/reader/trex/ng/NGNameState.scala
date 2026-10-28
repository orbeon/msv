package org.orbeon.msv.reader.trex.ng

import org.orbeon.msv.grammar.{DifferenceNameClass, NameClass, NamespaceNameClass}
import org.orbeon.msv.reader.State
import org.orbeon.msv.reader.trex.NameClassWithChildState
import org.orbeon.msv.util.StartTagInfo

/**
 * parses &lt;anyName&gt; name class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class NGNameState extends NameClassWithChildState:
  allowNullChild = true

  protected override def createChildState(tag: StartTagInfo): State =
    if nameClass == null && tag.localName == "except" then
      reader.asInstanceOf[RELAXNGReader].getStateFactory.nsExcept(this, tag)
    else
      null

  protected def castNameClass(halfCastedNameClass: NameClass, newChildNameClass: NameClass): NameClass =
    newChildNameClass

  protected override def annealNameClass(nameClass: NameClass): NameClass =
    var r = getMainNameClass()
    if nameClass != null then
      r = new DifferenceNameClass(r, nameClass)
    r

  protected def getMainNameClass(): NameClass

object NGNameState:
  class AnyNameState extends NGNameState:
    protected def getMainNameClass(): NameClass = NameClass.ALL

  class NsNameState extends NGNameState:
    protected def getMainNameClass(): NameClass =
      new NamespaceNameClass(getPropagatedNamespace())
