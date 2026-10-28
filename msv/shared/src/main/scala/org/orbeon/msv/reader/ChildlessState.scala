package org.orbeon.msv.reader

import org.orbeon.msv.util.StartTagInfo

/**
 * state that has no children.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ChildlessState extends SimpleState:
  protected final def createChildState(tag: StartTagInfo): State = null
