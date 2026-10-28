package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.reader.{AbortException, ChildlessState}

/**
 * Used to parse &lt;import&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ImportState extends ChildlessState:

  override protected def startSelf(): Unit =
    super.startSelf()
    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    var namespace = startTag.getAttribute("namespace")
    if namespace == null then namespace = ""

    if namespace == reader.currentSchema.targetNamespace then
      reader.reportError(XMLSchemaReader.ERR_IMPORTING_SAME_NAMESPACE)
      return

    if reader.isSchemaDefined(reader.getOrCreateSchema(namespace)) then
      return

    try
      reader.switchSource(
        this,
        new RootIncludedSchemaState(reader.sfactory.schemaHead(namespace))
      )
    catch
      case _: AbortException =>
        // recover by ignoring the error
