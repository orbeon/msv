package org.orbeon.msv.verifier

import java.util.Set as JSet
import org.orbeon.msv.datatype.xsd.StringType
import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.IDContextProvider2
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.{DatatypeRef, StartTagInfo, StringRef}
import org.xml.sax.{Attributes, ErrorHandler, SAXException}

/**
 * SAX ContentHandler that verifies incoming SAX event stream.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class Verifier(protected val docDecl: DocumentDeclaration, protected var errorHandler: ErrorHandler)
  extends AbstractVerifier with IVerifier:

  protected var current: Acceptor = scala.compiletime.uninitialized

  private final class Context(
    val previous: Context,
    val acceptor: Acceptor,
    val stringCareLevel: Int,
    var panicLevel: Int
  )

  private var stack: Context = scala.compiletime.uninitialized
  private var stringCareLevel: Int = Acceptor.STRING_STRICT
  private var text: java.lang.StringBuffer = new java.lang.StringBuffer()

  def getErrorHandler: ErrorHandler = errorHandler
  final def setErrorHandler(handler: ErrorHandler): Unit = this.errorHandler = handler

  protected var hadError: Boolean = false
  private var isFinished: Boolean = false

  private val sti: StartTagInfo = new StartTagInfo(null, null, null, null, null.asInstanceOf[IDContextProvider2])

  final def isValid: Boolean = !hadError && isFinished

  protected var panicLevel: Int = 0
  private var initialPanicLevel: Int = Verifier.DEFAULT_PANIC_LEVEL

  final def setPanicMode(usePanicMode: Boolean): Unit =
    initialPanicLevel = if usePanicMode then Verifier.DEFAULT_PANIC_LEVEL else 0

  private val characterType: DatatypeRef = new DatatypeRef()
  def getLastCharacterType: Array[Datatype] = characterType.types

  @throws(classOf[SAXException])
  protected def verifyText(): Unit =
    characterType.types = null
    stringCareLevel match
      case Acceptor.STRING_PROHIBITED =>
        val len = text.length()
        var i = 0
        var loop = true
        while i < len && loop do
          val ch = text.charAt(i)
          if ch != ' ' && ch != '\t' && ch != '\r' && ch != '\n' then
            onError(null.asInstanceOf[StringRef], Verifier.localizeMessage(Verifier.ERR_UNEXPECTED_TEXT, null), new ErrorInfo.BadText(text))
            loop = false
          i += 1

      case Acceptor.STRING_STRICT =>
        val txt = text.toString
        if !current.onText2(txt, this, null, characterType) then
          val err = new StringRef()
          characterType.types = null
          current.onText2(txt, this, err, characterType)
          onError(err, Verifier.localizeMessage(Verifier.ERR_UNEXPECTED_TEXT, null), new ErrorInfo.BadText(text))

      case Acceptor.STRING_IGNORE =>
        if text.length() != 0 then throw new Error()
        return

      case _ => throw new Error()

    if text.length() != 0 then
      text = new java.lang.StringBuffer()

  @throws(classOf[SAXException])
  override def startElement(namespaceUri0: String, localName0: String, qName0: String, atts: Attributes): Unit =
    var namespaceUri = namespaceUri0
    var localName = localName0
    var qName = qName0

    if namespaceUri == null then namespaceUri = ""
    if localName == null || localName.length == 0 then localName = qName
    if qName == null || qName.length == 0 then qName = localName

    super.startElement(namespaceUri, localName, qName, atts)

    if Debug.debug then
      println(s"\n-- startElement($qName)${locator.getLineNumber}:${locator.getColumnNumber}")

    verifyText()

    stack = new Context(stack, current, stringCareLevel, panicLevel)
    sti.reinit(namespaceUri, localName, qName, atts, this)

    var next = current.createChildAcceptor(sti, null)
    panicLevel = Math.max(panicLevel - 1, 0)

    if next == null then
      if Debug.debug then println("-- no children accepted: error recovery")
      val ref = new StringRef()
      next = current.createChildAcceptor(sti, ref)
      val vv = onError(
        ref,
        Verifier.localizeMessage(Verifier.ERR_UNEXPECTED_STARTTAG, Array[AnyRef](qName)),
        new ErrorInfo.BadTagName(sti)
      )
      if next == null then
        if Debug.debug then println("-- unable to recover")
        throw new ValidationUnrecoverableException(vv)

    onNextAcceptorReady(sti, next)

    val len = atts.getLength
    var i = 0
    while i < len do
      feedAttribute(next, atts.getURI(i), atts.getLocalName(i), atts.getQName(i), atts.getValue(i))
      i += 1

    if !next.onEndAttributes(sti, null) then
      if Debug.debug then println("-- required attributes missing: error recovery")
      val ref = new StringRef()
      next.onEndAttributes(sti, ref)
      onError(
        ref,
        Verifier.localizeMessage(Verifier.ERR_MISSING_ATTRIBUTE, Array[AnyRef](qName)),
        new ErrorInfo.MissingAttribute(sti)
      )

    stack.panicLevel = panicLevel
    stringCareLevel = next.getStringCareLevel
    if stringCareLevel == Acceptor.STRING_IGNORE then
      characterType.types = Array(StringType.theInstance)
    current = next

  @throws(classOf[SAXException])
  protected def onNextAcceptorReady(sti: StartTagInfo, nextAcceptor: Acceptor): Unit = ()

  private val attributeType: DatatypeRef = new DatatypeRef()

  @throws(classOf[SAXException])
  protected def feedAttribute(child: Acceptor, uri0: String, localName0: String, qName0: String, value: String): Array[Datatype] =
    var uri = uri0
    var localName = localName0
    var qName = qName0

    if uri == null then uri = ""
    if localName == null || localName.length == 0 then localName = qName
    if qName == null || qName.length == 0 then qName = localName

    if qName.startsWith("xmlns:") || qName == "xmlns" then
      return Array.empty[Datatype]

    if Debug.debug then println(s"-- processing attribute: @$qName")

    attributeType.types = null
    if !child.onAttribute2(uri, localName, qName, value, this, null, attributeType) then
      if Debug.debug then println("-- bad attribute: error recovery")
      val ref = new StringRef()
      child.onAttribute2(uri, localName, qName, value, this, ref, null)
      onError(
        ref,
        Verifier.localizeMessage(Verifier.ERR_UNEXPECTED_ATTRIBUTE, Array[AnyRef](qName)),
        new ErrorInfo.BadAttribute(sti, qName, uri, localName, value)
      )

    attributeType.types

  @throws(classOf[SAXException])
  override def endElement(namespaceUri0: String, localName0: String, qName0: String): Unit =
    var namespaceUri = namespaceUri0
    var localName = localName0
    var qName = qName0

    if namespaceUri == null then namespaceUri = ""
    if localName == null || localName.length == 0 then localName = qName
    if qName == null || qName.length == 0 then qName = localName

    if Debug.debug then
      println(s"\n-- endElement($qName)${locator.getLineNumber}:${locator.getColumnNumber}")

    verifyText()

    if !current.isAcceptState(null) && panicLevel == 0 then
      val errRef = new StringRef()
      current.isAcceptState(errRef)
      onError(
        errRef,
        Verifier.localizeMessage(Verifier.ERR_UNCOMPLETED_CONTENT, Array[AnyRef](qName)),
        new ErrorInfo.IncompleteContentModel(qName, namespaceUri, localName)
      )

    val child = current
    current = stack.acceptor
    stringCareLevel = stack.stringCareLevel
    panicLevel = Math.max(panicLevel, stack.panicLevel)
    stack = stack.previous

    if !current.stepForward(child, null) then
      val ref = new StringRef()
      current.stepForward(child, ref)
      onError(ref, Verifier.localizeMessage(Verifier.ERR_UNEXPECTED_ELEMENT, Array[AnyRef](qName)), null)
    else
      panicLevel = Math.max(panicLevel - 1, 0)

    super.endElement(namespaceUri, localName, qName)

  @throws(classOf[SAXException])
  protected def onError(ref: StringRef, defaultMsg: String, ei: ErrorInfo): ValidityViolation =
    if ref == null || ref.str == null then onError(defaultMsg, ei)
    else onError(ref.str, ei)

  @throws(classOf[SAXException])
  protected def onError(msg: String, ei: ErrorInfo): ValidityViolation =
    val vv = new ValidityViolation(locator, msg, ei)
    hadError = true
    if errorHandler != null && panicLevel == 0 then
      errorHandler.error(vv)
    panicLevel = initialPanicLevel
    vv

  def getCurrentElementType: AnyRef = current.getOwnerType

  @throws(classOf[SAXException])
  def characters(buf: Array[Char], start: Int, len: Int): Unit =
    if stringCareLevel != Acceptor.STRING_IGNORE then
      text.append(buf, start, len)

  @throws(classOf[SAXException])
  def ignorableWhitespace(buf: Array[Char], start: Int, len: Int): Unit =
    if stringCareLevel != Acceptor.STRING_IGNORE && stringCareLevel != Acceptor.STRING_PROHIBITED then
      text.append(buf, start, len)

  override protected def init(): Unit =
    super.init()
    hadError = false
    isFinished = false
    text = new java.lang.StringBuffer()
    stack = null
    if duplicateIds != null then duplicateIds.clear()

  @throws(classOf[SAXException])
  def startDocument(): Unit =
    init()
    current = docDecl.createAcceptor()

  @throws(classOf[SAXException])
  def endDocument(): Unit =
    if performIDcheck then
      if !ids.keySet().containsAll(idrefs) then
        hadError = true
        val itr = idrefs.iterator()
        while itr.hasNext do
          val idref = itr.next()
          if !ids.keySet().contains(idref) then
            onError(Verifier.localizeMessage(Verifier.ERR_UNSOLD_IDREF, Array[AnyRef](idref)), null)
      if duplicateIds != null then
        val itr = duplicateIds.iterator()
        while itr.hasNext do
          val id = itr.next()
          onError(Verifier.localizeMessage(Verifier.ERR_DUPLICATE_ID, Array[AnyRef](id.asInstanceOf[AnyRef])), null)
    isFinished = true

  private var duplicateIds: JSet[String] = scala.compiletime.uninitialized

  def onDuplicateId(id: String): Unit =
    if duplicateIds == null then duplicateIds = new java.util.HashSet[String]()
    duplicateIds.add(id)

object Verifier:
  val DEFAULT_PANIC_LEVEL: Int = 3

  val ERR_UNEXPECTED_TEXT: String      = "Verifier.Error.UnexpectedText"
  val ERR_UNEXPECTED_ATTRIBUTE: String = "Verifier.Error.UnexpectedAttribute"
  val ERR_MISSING_ATTRIBUTE: String    = "Verifier.Error.MissingAttribute"
  val ERR_UNEXPECTED_STARTTAG: String  = "Verifier.Error.UnexpectedStartTag"
  val ERR_UNCOMPLETED_CONTENT: String  = "Verifier.Error.UncompletedContent"
  val ERR_UNEXPECTED_ELEMENT: String   = "Verifier.Error.UnexpectedElement"
  val ERR_UNSOLD_IDREF: String         = "Verifier.Error.UnsoldIDREF"
  val ERR_DUPLICATE_ID: String         = "Verifier.Error.DuplicateId"

  private val messages: Map[String, String] = Map(
    ERR_UNEXPECTED_TEXT      -> "unexpected character literal",
    ERR_UNEXPECTED_ATTRIBUTE -> "unexpected attribute \"{0}\"",
    ERR_UNEXPECTED_STARTTAG  -> "unexpected start tag \"{0}\".",
    ERR_MISSING_ATTRIBUTE    -> "some attributes are missing for the element \"{0}\".",
    ERR_UNCOMPLETED_CONTENT  -> "Content of element \"{0}\" is incomplete",
    ERR_UNSOLD_IDREF         -> "\"{0}\" is referenced by an IDREF, but not defined.",
    ERR_DUPLICATE_ID         -> "\"{0}\" is used as an ID value more than once.",
    ERR_UNEXPECTED_ELEMENT   -> "unexpected element \"{0}\""
  )

  def format(formatStr: String, args: Array[AnyRef]): String =
    if args == null || args.isEmpty then formatStr
    else
      var res = formatStr
      var i = 0
      while i < args.length do
        val s = if args(i) != null then args(i).toString else "null"
        res = res.replace("{" + i + "}", s)
        i += 1
      res

  def localizeMessage(propertyName: String, args: Array[AnyRef]): String =
    val template = messages.getOrElse(propertyName, propertyName)
    format(template, args)
