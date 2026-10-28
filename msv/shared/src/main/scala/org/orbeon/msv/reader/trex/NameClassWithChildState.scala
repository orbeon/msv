package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.NameClass
import org.orbeon.msv.reader.State
import org.orbeon.msv.util.StartTagInfo

/**
 * parses name class that has child name classes
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class NameClassWithChildState extends NameClassState with NameClassOwner:
  /**
   * name class object that is being created.
   */
  protected var nameClass: NameClass = null

  /**
   * if this flag is true, then it is OK not to have any children.
   */
  protected var allowNullChild: Boolean = false

  /**
   * receives a Pattern object that is contained in this element.
   */
  final def onEndChild(childNameClass: NameClass): Unit =
    nameClass = castNameClass(nameClass, childNameClass)

  protected final def makeNameClass(): NameClass =
    if nameClass == null && !allowNullChild then
      reader.reportError(TREXBaseReader.ERR_MISSING_CHILD_NAMECLASS)
      nameClass = NameClass.ALL
      // recover by assuming some name class.
    annealNameClass(nameClass)

  protected override def createChildState(tag: StartTagInfo): State =
    reader.asInstanceOf[TREXBaseReader].createNameClassChildState(this, tag)

  /**
   * combines half-made name class and newly found child name class into the name class.
   */
  protected def castNameClass(halfCastedNameClass: NameClass, newChildNameClass: NameClass): NameClass

  /**
   * performs final wrap-up and returns a fully created NameClass object
   * that represents this element.
   */
  protected def annealNameClass(nameClass: NameClass): NameClass =
    // default implementation does nothing.
    nameClass
