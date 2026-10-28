package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.reader.{AbortException, ChildlessState}

/**
 * Used to parse &lt;include&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IncludeState extends ChildlessState:

  override protected def startSelf(): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    super.startSelf()
    try
      reader.switchSource(
        this,
        new RootIncludedSchemaState(
          reader.sfactory.schemaIncluded(this, reader.currentSchema.targetNamespace)
        )
      )
    catch
      case _: AbortException =>
        // recover by ignoring the error
