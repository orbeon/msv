package org.orbeon.msv.reader.trex

import org.orbeon.msv.reader.State
import org.orbeon.msv.util.StartTagInfo

/**
 * Base implementation for those states who cannot have any child name class.
 * (e.g., nsName, anyName)
 * 
 * Note that this class does not prohibit children from different namespace.
 * Those "foreign" elements are just ignored.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class NameClassWithoutChildState extends NameClassState:
  protected override final def createChildState(tag: StartTagInfo): State =
    // return null to indicate that this element does not accept a child.
    null
