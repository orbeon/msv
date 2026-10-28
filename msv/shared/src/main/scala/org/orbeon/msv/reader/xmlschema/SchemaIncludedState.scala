package org.orbeon.msv.reader.xmlschema

import java.util.{HashSet, Set}
import org.orbeon.msv.reader.{IgnoreState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse &lt;schema&gt; element of included schema.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SchemaIncludedState(protected var expectedTargetNamespace: String) extends GlobalDeclState:

  private var previousElementFormDefault: String = null
  private var previousAttributeFormDefault: String = null
  private var previousFinalDefault: String = null
  private var previousBlockDefault: String = null
  private var previousChameleonTargetNamespace: String = null

  /**
   * This flag is set to true to indicate all the contents of this element
   * will be skipped (due to double inclusion).
   */
  private var ignoreContents: Boolean = false

  override protected def createChildState(tag: StartTagInfo): State =
    if ignoreContents then new IgnoreState()
    else super.createChildState(tag)

  override protected def startSelf(): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    super.startSelf()

    // back up the current values
    previousElementFormDefault = reader.elementFormDefault
    previousAttributeFormDefault = reader.attributeFormDefault
    previousFinalDefault = reader.finalDefault
    previousBlockDefault = reader.blockDefault
    previousChameleonTargetNamespace = reader.chameleonTargetNamespace

    // the chameleonTargetNamespace is usually null, unless we are parsing
    // a chameleon schema.
    reader.chameleonTargetNamespace = null

    var targetNs = startTag.getAttribute("targetNamespace")
    if targetNs == null then
      if expectedTargetNamespace == null then
        targetNs = "" // recover by assuming "" namespace.
      else
        targetNs = expectedTargetNamespace
        reader.chameleonTargetNamespace = expectedTargetNamespace
    else
      if expectedTargetNamespace != null && expectedTargetNamespace != targetNs then
        reader.reportError(
          XMLSchemaReader.ERR_INCONSISTENT_TARGETNAMESPACE,
          Array[AnyRef](targetNs, expectedTargetNamespace)
        )

    onTargetNamespaceResolved(targetNs)

    // check double inclusion.
    var s = reader.parsedFiles.get(targetNs)
    if s == null then
      s = new HashSet[String]()
      reader.parsedFiles.put(targetNs, s)

    if this.location != null && this.location.getSystemId != null then
      if s.contains(this.location.getSystemId) then
        ignoreContents = true
      else
        s.add(this.location.getSystemId)

    // process other attributes.
    var form = startTag.getDefaultedAttribute("elementFormDefault", "unqualified")
    if form == "qualified" then
      reader.elementFormDefault = targetNs
    else
      reader.elementFormDefault = ""
      if form != "unqualified" then
        reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "elementFormDefault", form)

    form = startTag.getDefaultedAttribute("attributeFormDefault", "unqualified")
    if form == "qualified" then
      reader.attributeFormDefault = targetNs
    else
      reader.attributeFormDefault = ""
      if form != "unqualified" then
        reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "attributeFormDefault", form)

    reader.finalDefault = startTag.getAttribute("finalDefault")
    reader.blockDefault = startTag.getAttribute("blockDefault")

  /** Does something useful with determined target namespace. */
  protected def onTargetNamespaceResolved(targetNs: String): Unit = ()

  override protected def endSelf(): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    reader.elementFormDefault = previousElementFormDefault
    reader.attributeFormDefault = previousAttributeFormDefault
    reader.finalDefault = previousFinalDefault
    reader.blockDefault = previousBlockDefault
    reader.chameleonTargetNamespace = previousChameleonTargetNamespace

    super.endSelf()
