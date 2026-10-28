package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.xmlschema.SimpleTypeExp
import org.orbeon.msv.reader.{AbortException, State}
import org.orbeon.msv.reader.datatype.xsd.XSDatatypeExp
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse &lt;redefine&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RedefineState extends GlobalDeclState:

  /**
   * When a simple type is being redefined, the original declaration
   * will be stored here.
   */
  private var oldSimpleTypeExp: SimpleTypeExp = null

  override protected def createChildState(tag: StartTagInfo): State =
    if tag.localName == "simpleType" then
      val reader = this.reader.asInstanceOf[XMLSchemaReader]
      val name = tag.getAttribute("name")

      var sexp = reader.currentSchema.simpleTypes.get(name)
      if sexp == null then
        reader.reportError(XMLSchemaReader.ERR_REDEFINE_UNDEFINED, name)
        // recover by using an empty declaration
        sexp = reader.currentSchema.simpleTypes.getOrCreate(name)

      reader.currentSchema.simpleTypes.redefine(name, sexp.getClone())
      oldSimpleTypeExp = sexp // memorize this declaration

    super.createChildState(tag)

  override protected def startSelf(): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    super.startSelf()

    try
      // parse included grammar first.
      reader.switchSource(
        this,
        new RootIncludedSchemaState(
          reader.sfactory.schemaIncluded(this, reader.currentSchema.targetNamespace)
        )
      )
    catch
      case _: AbortException =>
        // recover by ignoring the error

    // disable duplicate definition check.
    prevDuplicateCheck = reader.doDuplicateDefinitionCheck

  /** Previous value of reader#doDuplicateDefinitionCheck. */
  private var prevDuplicateCheck: Boolean = false

  override protected def endSelf(): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    reader.doDuplicateDefinitionCheck = prevDuplicateCheck
    super.endSelf()

  override def onEndChild(dataType: XSDatatypeExp): Unit =
    // handle redefinition of simpleType.
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val typeName = dataType.name

    if typeName == null then
      // top-level simpleType must define a named type
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "simpleType", "name")
      return // recover by ignoring this declaration

    oldSimpleTypeExp.set(dataType)
    reader.setDeclaredLocationOf(oldSimpleTypeExp)

    // restore the association
    reader.currentSchema.simpleTypes.redefine(oldSimpleTypeExp.name, oldSimpleTypeExp)

    oldSimpleTypeExp = null
