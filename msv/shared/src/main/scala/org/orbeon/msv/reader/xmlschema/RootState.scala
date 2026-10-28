package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.reader.State

/**
 * RootIncludedSchemaState + final wrap up.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RootState(topLevelState: State) extends RootIncludedSchemaState(topLevelState):

  override protected def endSelf(): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    // perform final wrap up.
    reader.wrapUp()
    super.endSelf()
