package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.NameClass

/**
 * interface that must be implemented by the parent state of NameClassState.
 *
 * NameClassState notifies its parent by using this interface.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait NameClassOwner:
  def onEndChild(p: NameClass): Unit
