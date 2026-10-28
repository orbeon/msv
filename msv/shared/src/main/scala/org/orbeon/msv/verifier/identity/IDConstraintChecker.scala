package org.orbeon.msv.verifier.identity

import java.util.{HashMap as JHashMap, HashSet as JHashSet, Map as JMap, Set as JSet, Vector as JVector}
import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.xmlschema.{ElementDeclExp, IdentityConstraint, KeyRefConstraint, XMLSchemaGrammar}
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.{LightStack, StartTagInfo}
import org.orbeon.msv.verifier.{Acceptor, ErrorInfo, ValidityViolation, Verifier}
import org.orbeon.msv.verifier.regexp.REDocumentDeclaration
import org.orbeon.msv.verifier.regexp.xmlschema.XSREDocDecl
import org.xml.sax.{ErrorHandler, Locator, SAXException}

/**
 * Verifier with XML Schema-related enforcement.
 *
 * <p>
 * This class can be used in the same way as {@link Verifier}.
 * This class also checks XML Schema's identity constraint.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IDConstraintChecker(
  protected val grammar: XMLSchemaGrammar,
  errorHandler: ErrorHandler
) extends Verifier(new XSREDocDecl(grammar), errorHandler):

  /** active matchers. */
  protected val matchers: JVector[Matcher] = new JVector[Matcher]()

  def add(matcher: Matcher): Unit =
    matchers.add(matcher)

  def remove(matcher: Matcher): Unit =
    matchers.remove(matcher)

  /**
   * a map from SelectorMatcher to set of KeyValues.
   */
  private val keyValues: JMap[SelectorMatcher, JSet[KeyValue]] = new JHashMap[SelectorMatcher, JSet[KeyValue]]()

  /**
   * a map from keyref SelectorMatcher to key/unique SelectorMatcher.
   */
  private val referenceScope: JMap[SelectorMatcher, SelectorMatcher] = new JHashMap[SelectorMatcher, SelectorMatcher]()

  /**
   * a map from IdentityConstraint to a LightStack of SelectorMatcher.
   */
  private val activeScopes: JMap[IdentityConstraint, LightStack] = new JHashMap[IdentityConstraint, LightStack]()

  def getActiveScope(c: IdentityConstraint): SelectorMatcher =
    val s = activeScopes.get(c)
    if s == null || s.size() == 0 then null
    else s.top().asInstanceOf[SelectorMatcher]

  def pushActiveScope(c: IdentityConstraint, matcher: SelectorMatcher): Unit =
    var s = activeScopes.get(c)
    if s == null then
      s = new LightStack()
      activeScopes.put(c, s)
    s.push(matcher)

  def popActiveScope(c: IdentityConstraint, matcher: SelectorMatcher): Unit =
    val s = activeScopes.get(c)
    if s == null || s.pop() != matcher then
      throw new Error()

  /**
   * adds a new KeyValue to the value set.
   * @return true if this is a new value.
   */
  def addKeyValue(scope: SelectorMatcher, value: KeyValue): Boolean =
    var keys = keyValues.get(scope)
    if keys == null then
      keys = new JHashSet[KeyValue]()
      keyValues.put(scope, keys)
    keys.add(value)

  /**
   * gets the all KeyValues that were added within the specified scope.
   */
  def getKeyValues(scope: SelectorMatcher): Array[KeyValue] =
    val keys = keyValues.get(scope)
    if keys == null then new Array[KeyValue](0)
    else keys.toArray(new Array[KeyValue](keys.size()))

  @throws(classOf[SAXException])
  override def startDocument(): Unit =
    super.startDocument()
    keyValues.clear()

  @throws(classOf[SAXException])
  override def endDocument(): Unit =
    super.endDocument()

    // keyref check
    val entries = keyValues.entrySet().toArray(new Array[JMap.Entry[SelectorMatcher, JSet[KeyValue]]](keyValues.size()))
    if Debug.debug then
      println(s"key/keyref check: there are ${keyValues.size()} scope(s)")

    var i = 0
    while i < entries.length do
      val key = entries(i).getKey
      val value = entries(i).getValue

      if key.idConst.isInstanceOf[KeyRefConstraint] then
        val keys = keyValues.get(referenceScope.get(key))
        val keyrefs = value.toArray(new Array[KeyValue](value.size()))

        var j = 0
        while j < keyrefs.length do
          if keys == null || !keys.contains(keyrefs(j)) then
            reportError(
              keyrefs(j).locator,
              null,
              IDConstraintChecker.ERR_UNDEFINED_KEY,
              Array(key.idConst.namespaceURI, key.idConst.localName)
            )
          j += 1
      i += 1

  @throws(classOf[SAXException])
  override protected def onNextAcceptorReady(sti: StartTagInfo, next: Acceptor): Unit =
    // call matchers
    val len = matchers.size()
    var i = 0
    while i < len do
      val m = matchers.get(i)
      m.startElement(sti.namespaceURI, sti.localName)
      i += 1

    // introduce newly found identity constraints.
    val e = next.getOwnerType
    if e.isInstanceOf[ElementDeclExp.XSElementExp] then
      val exp = e.asInstanceOf[ElementDeclExp.XSElementExp]
      if exp.identityConstraints != null then
        val m = exp.identityConstraints.size()
        var j = 0
        while j < m do
          add(new SelectorMatcher(this, exp.identityConstraints.get(j), sti.namespaceURI, sti.localName))
          j += 1

        j = 0
        while j < m do
          val c = exp.identityConstraints.get(j)
          if c.isInstanceOf[KeyRefConstraint] then
            val keyScope = getActiveScope(c.asInstanceOf[KeyRefConstraint].key)
            referenceScope.put(getActiveScope(c), keyScope)
          j += 1

  @throws(classOf[SAXException])
  override protected def feedAttribute(
    child: Acceptor,
    uri: String,
    localName: String,
    qName: String,
    value: String
  ): Array[Datatype] =
    val result = super.feedAttribute(child, uri, localName, qName, value)
    val dt = if result == null || result.length == 0 then null else result(0)

    val len = matchers.size()
    var i = 0
    while i < len do
      matchers.get(i).onAttribute(uri, localName, value, dt)
      i += 1

    result

  @throws(classOf[SAXException])
  override def characters(buf: Array[Char], start: Int, len: Int): Unit =
    super.characters(buf, start, len)
    val m = matchers.size()
    var i = 0
    while i < m do
      matchers.get(i).characters(buf, start, len)
      i += 1

  @throws(classOf[SAXException])
  override def endElement(namespaceUri: String, localName: String, qName: String): Unit =
    super.endElement(namespaceUri, localName, qName)

    val lastType = getLastCharacterType
    val dt = if lastType == null || lastType.length == 0 then null else lastType(0)

    var i = matchers.size() - 1
    while i >= 0 do
      matchers.get(i).endElement(dt)
      i -= 1

  /** reports an error. */
  @throws(classOf[SAXException])
  def reportError(ei: ErrorInfo, propKey: String, args: Array[AnyRef]): Unit =
    reportError(getLocator, ei, propKey, args)

  @throws(classOf[SAXException])
  def reportError(loc: Locator, ei: ErrorInfo, propKey: String, args: Array[AnyRef]): Unit =
    hadError = true
    errorHandler.error(new ValidityViolation(loc, IDConstraintChecker.localizeMessage(propKey, args), ei))

object IDConstraintChecker:
  val ERR_UNMATCHED_KEY_FIELD: String = "IdentityConstraint.UnmatchedKeyField"
  val ERR_NOT_UNIQUE: String          = "IdentityConstraint.NotUnique"
  val ERR_NOT_UNIQUE_DIAG: String     = "IdentityConstraint.NotUnique.Diag"
  val ERR_DOUBLE_MATCH: String        = "IdentityConstraint.DoubleMatch"
  val ERR_UNDEFINED_KEY: String       = "IdentityConstraint.UndefinedKey"

  private val messages: Map[String, String] = Map(
    ERR_UNMATCHED_KEY_FIELD -> "field #{2} of the key constraint \"{1}\" is not found",
    ERR_NOT_UNIQUE          -> "this element does not satisfy the uniqueness imposed by \"{1}\"",
    ERR_NOT_UNIQUE_DIAG     -> "The same key value is used",
    ERR_DOUBLE_MATCH        -> "more than one elements/attributes match the field #{2} of the identity constraint \"{1}\"",
    ERR_UNDEFINED_KEY       -> "keyref \"{1}\" does not have a corresponding key"
  )

  def localizeMessage(propertyName: String, arg: AnyRef): String =
    localizeMessage(propertyName, Array(arg))

  def localizeMessage(propertyName: String, args: Array[AnyRef]): String =
    val template = messages.getOrElse(propertyName, propertyName)
    REDocumentDeclaration.format(template, args)
