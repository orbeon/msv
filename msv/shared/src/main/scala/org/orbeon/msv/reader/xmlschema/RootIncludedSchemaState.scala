package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.reader.{SimpleState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse root of schema document.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RootIncludedSchemaState(protected var topLevelState: State) extends SimpleState:

  override protected def createChildState(tag: StartTagInfo): State =
    if tag.localName == "schema" then topLevelState
    else null
