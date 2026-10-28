package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.NameClass
import org.orbeon.msv.reader.SimpleState

/**
 * Base implementation for NameClass primitives
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class NameClassState extends SimpleState:
  final override def endSelf(): Unit =
    // pass the pattern to the parent
    parentState.asInstanceOf[NameClassOwner].onEndChild(makeNameClass())
    super.endSelf()

  /**
   * This method is called from endElement method.
   * Implementation has to provide NameClass object that represents the content of
   * this element.
   */
  protected def makeNameClass(): NameClass

  protected final def getPropagatedNamespace(): String =
    reader.asInstanceOf[TREXBaseReader].targetNamespace
