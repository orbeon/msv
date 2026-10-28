package org.orbeon.msv.reader.xmlschema

import java.util.{ArrayList as JArrayList, List as JList}
import org.xml.sax.Locator
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.xmlschema.*
import org.orbeon.msv.reader.{ChildlessState, GrammarReader, SimpleState, State}
import org.orbeon.msv.util.StartTagInfo

/**
 * Used to parse &lt;unique&gt;,&lt;key&gt;, and &lt;keyref&gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IdentityConstraintState extends SimpleState:

  protected var selector: Array[XPath] = null
  protected val fields: JList[Field] = new JArrayList[Field]()

  protected def createChildState(tag: StartTagInfo): State =
    if tag.localName == "selector" then
      val v = tag.getAttribute("xpath")
      if v != null then
        selector = parseSelector(v)
      else
        reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "selector", "xpath")
        selector = new Array[XPath](0)
      new ChildlessState()
    else if tag.localName == "field" then
      val v = tag.getAttribute("xpath")
      if v != null then
        fields.add(parseField(v))
      else
        reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, "field", "xpath")
      new ChildlessState()
    else
      null

  override protected def endSelf(): Unit =
    createIdentityConstraint()
    super.endSelf()

  protected def createIdentityConstraint(): Unit =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    var id: IdentityConstraint = null

    val name = startTag.getAttribute("name")
    if name == null then
      reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, startTag.localName, "name")
      return

    val fs = fields.toArray(new Array[Field](fields.size))

    if startTag.localName == "key" then
      id = new KeyConstraint(reader.currentSchema.targetNamespace, name, selector, fs)
    else if startTag.localName == "unique" then
      id = new UniqueConstraint(reader.currentSchema.targetNamespace, name, selector, fs)
    else if startTag.localName == "keyref" then
      val refer = startTag.getAttribute("refer")
      if refer == null then
        reader.reportError(XMLSchemaReader.ERR_MISSING_ATTRIBUTE, startTag.localName, "refer")
        return

      val qn = reader.splitQName(refer)
      if qn == null then
        reader.reportError(XMLSchemaReader.ERR_UNDECLARED_PREFIX, refer)
        return

      val keyRef = new KeyRefConstraint(reader.currentSchema.targetNamespace, name, selector, fs)
      id = keyRef

      // back patch "key" field of KeyRefConstraint.
      reader.addBackPatchJob(new GrammarReader.BackPatch:
        def getOwnerState: State = IdentityConstraintState.this
        def patch(): Unit =
          val s = reader.grammar.getByNamespace(qn(0)).asInstanceOf[XMLSchemaSchema]
          if s == null then
            reader.reportError(XMLSchemaReader.ERR_UNDEFINED_SCHEMA, qn(0))
            return

          val idc = s.identityConstraints.get(qn(1))
          if idc == null then
            reader.reportError(XMLSchemaReader.ERR_UNDEFINED_KEY, refer)
            return

          if !idc.isInstanceOf[KeyConstraint] then
            reader.reportError(XMLSchemaReader.ERR_KEYREF_REFERRING_NON_KEY, refer)
            return

          if idc.fields.length != keyRef.fields.length then
            reader.reportError(
              Array[Locator](getLocation, reader.getDeclaredLocationOf(idc)),
              XMLSchemaReader.ERR_KEY_FIELD_NUMBER_MISMATCH,
              Array[AnyRef](
                idc.localName,
                Integer.valueOf(idc.fields.length),
                keyRef.localName,
                Integer.valueOf(keyRef.fields.length)
              )
            )
            return

          keyRef.key = idc.asInstanceOf[KeyConstraint]
      )
    else
      throw new Error()

    if reader.currentSchema.identityConstraints.get(name) != null then
      reader.reportError(
        Array[Locator](this.location, reader.getDeclaredLocationOf(id)),
        XMLSchemaReader.ERR_DUPLICATE_IDENTITY_CONSTRAINT_DEFINITION,
        Array[AnyRef](name)
      )
    else
      reader.currentSchema.identityConstraints.add(name, id)

    reader.setDeclaredLocationOf(id)
    parentState.asInstanceOf[ElementDeclState].onIdentityConstraint(id)

  protected def parseSelector(xpath: String): Array[XPath] =
    val pathObjs = new JArrayList[XPath]()
    val paths = xpath.split("\\|").map(_.trim).filter(_.nonEmpty)
    var success = true
    var i = 0
    while i < paths.length && success do
      val pathObj = new XPath()
      pathObjs.add(pathObj)
      if !parsePath(pathObj, paths(i), false) then
        success = false
      i += 1

    if success then pathObjs.toArray(new Array[XPath](pathObjs.size))
    else new Array[XPath](0)

  protected def parseField(xpath: String): Field =
    val pathObjs = new JArrayList[XPath]()
    val field = new Field()
    val paths = xpath.split("\\|").map(_.trim).filter(_.nonEmpty)
    var success = true
    var i = 0
    while i < paths.length && success do
      val pathObj = new XPath()
      pathObjs.add(pathObj)
      if !parsePath(pathObj, paths(i), true) then
        success = false
      i += 1

    if success then
      field.paths = pathObjs.toArray(new Array[XPath](pathObjs.size))
      field
    else
      new Field()

  /**
   * Parses "aa/bb/cc/.../".
   *
   * @return true if it succeeds in parsing. Otherwise false.
   */
  protected def parsePath(pathObj: XPath, xpathParam: String, parseField: Boolean): Boolean =
    var xpath = xpathParam
    val stepObjs = new JArrayList[NameClass]()
    if xpath.startsWith(".//") then
      pathObj.isAnyDescendant = true
      xpath = xpath.substring(3)

    val steps = xpath.split("/").map(_.trim).filter(_.nonEmpty)
    var success = true
    var idx = 0
    while idx < steps.length && success do
      val step = steps(idx)
      val isLast = idx == steps.length - 1
      if step != "." then
        if step == "*" then
          stepObjs.add(NameClass.ALL)
        else
          var attribute = false
          var actualStep = step
          if step.charAt(0) == '@' && parseField && isLast then
            attribute = true
            actualStep = step.substring(1)

          val qn = reader.splitQName(actualStep)
          if qn == null then
            reader.reportError(XMLSchemaReader.ERR_BAD_XPATH, actualStep)
            success = false
          else
            if attribute && actualStep.indexOf(':') < 0 then
              qn(0) = ""

            val nc =
              if qn(1) == "*" then new NamespaceNameClass(qn(0))
              else new SimpleNameClass(qn(0), qn(1))

            if attribute then
              pathObj.attributeStep = nc
            else
              stepObjs.add(nc)
      idx += 1

    if success then
      pathObj.steps = stepObjs.toArray(new Array[NameClass](stepObjs.size))
      true
    else
      false
