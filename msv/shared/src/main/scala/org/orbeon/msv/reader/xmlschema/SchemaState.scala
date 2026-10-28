package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.xmlschema.XMLSchemaSchema

/**
 * Used to parse &lt;schema&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SchemaState(expectedTargetNamespace: String) extends SchemaIncludedState(expectedTargetNamespace):

  private var old: XMLSchemaSchema = null

  override protected def onTargetNamespaceResolved(targetNs: String): Unit =
    super.onTargetNamespaceResolved(targetNs)

    val reader = this.reader.asInstanceOf[XMLSchemaReader]

    // sets new XMLSchemaGrammar object.
    old = reader.currentSchema
    reader.currentSchema = reader.getOrCreateSchema(targetNs)

    if reader.isSchemaDefined(reader.currentSchema) then
      reader.reportError(XMLSchemaReader.ERR_DUPLICATE_SCHEMA_DEFINITION, targetNs)
      // recover by providing dummy grammar object.
      // this object is not registered to the map, so it cannot be referenced.
      reader.currentSchema = new XMLSchemaSchema(targetNs, reader.grammar)

    reader.markSchemaAsDefined(reader.currentSchema)

  override protected def endSelf(): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    reader.currentSchema = old
    super.endSelf()
