package org.orbeon.msv.reader

import org.orbeon.msv.util.StartTagInfo

/**
 * Base implementation for those states who cannot have any children.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ExpressionWithoutChildState extends ExpressionState:
  protected final def createChildState(tag: StartTagInfo): State = null
