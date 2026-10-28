package org.orbeon.msv.reader

import java.io.IOException
import java.util.{ArrayList as JArrayList, Iterator as JIterator, List as JList, Map as JMap}
import javax.xml.parsers.{ParserConfigurationException, SAXParserFactory}
import org.xml.sax.{InputSource, Locator, SAXException, SAXParseException, XMLReader}
import org.xml.sax.helpers.{LocatorImpl, XMLFilterImpl}
import org.orbeon.msv.datatype.xsd.*
import org.orbeon.msv.grammar.*
import org.orbeon.msv.reader.datatype.xsd.XSDatatypeExp
import org.orbeon.msv.util.{StartTagInfo, Uri}
import org.orbeon.msv.verifier.regexp.StringToken

/**
 * base implementation of grammar readers that read grammar from SAX2 stream.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class GrammarReader protected (
  controller0: GrammarReaderController,
  val parserFactory: SAXParserFactory,
  val pool: ExpressionPool,
  initialState: State
) extends XMLFilterImpl with IDContextProvider2:

  if !parserFactory.isNamespaceAware then
    throw new IllegalArgumentException("parser factory must be namespace-aware")

  val controller: Controller = new Controller(controller0)

  /** document Locator that is given by XML reader */
  var locator: Locator = scala.compiletime.uninitialized

  pushState(initialState, null, null)

  /**
   * gets the parsed AGM.
   * 
   * Should any error happens, this method should returns null.
   */
  def getResultAsGrammar: Grammar

  /** checks if given element is that of the grammar elements. */
  protected[msv] def isGrammarElement(tag: StartTagInfo): Boolean

  var prefixResolver: GrammarReader.PrefixResolver = GrammarReader.basePrefixResolver

  override def startPrefixMapping(prefix: String, uri: String): Unit =
    prefixResolver = new GrammarReader.ChainPrefixResolver(prefix, uri, prefixResolver)
    super.startPrefixMapping(prefix, uri)

  override def endPrefixMapping(prefix: String): Unit =
    prefixResolver = prefixResolver.asInstanceOf[GrammarReader.ChainPrefixResolver].previous
    super.endPrefixMapping(prefix)

  /**
   * Iterates Map.Entry objects which has the prefix as key and
   * the namespace URI as value.
   */
  def iterateInscopeNamespaces(): JIterator[JMap.Entry[String, String]] =
    new JIterator[JMap.Entry[String, String]]:
      private var resolver = proceed(prefixResolver)

      def next(): JMap.Entry[String, String] =
        val cpr = resolver.asInstanceOf[GrammarReader.ChainPrefixResolver]
        resolver = proceed(cpr.previous)
        new JMap.Entry[String, String]:
          def getKey: String = cpr.prefix
          def getValue: String = cpr.uri
          def setValue(o: String): String = throw new UnsupportedOperationException()

      def hasNext: Boolean = resolver.isInstanceOf[GrammarReader.ChainPrefixResolver]

      private def proceed(resolver0: GrammarReader.PrefixResolver): GrammarReader.PrefixResolver =
        var r = resolver0
        while true do
          if !r.isInstanceOf[GrammarReader.ChainPrefixResolver] then return r
          val cpr = r.asInstanceOf[GrammarReader.ChainPrefixResolver]
          if resolveNamespacePrefix(cpr.prefix) == cpr.uri then return r
          r = cpr.previous
        r

      override def remove(): Unit = throw new UnsupportedOperationException()

  /**
   * Resolves a QName into a pair of (namespace URI,local name).
   * Therefore this method returns null if it fails to process QName.
   */
  def splitQName(qName: String): Array[String] =
    val idx = qName.indexOf(':')
    if idx < 0 then
      var ns = prefixResolver.resolve("")
      if ns == null then ns = ""
      Array(ns, qName, qName, "")
    else
      val prefix = qName.substring(0, idx)
      val uri = prefixResolver.resolve(prefix)
      if uri == null then null
      else Array(uri, qName.substring(idx + 1), qName, prefix)

  /**
   * intercepts an expression made by ExpressionState
   * before it is passed to the parent state.
   */
  protected[msv] def interceptExpression(state: State, exp: Expression): Expression = exp

  /**
   * tries to obtain a DataType object by resolving obsolete names.
   * this method is useful for backward compatibility purpose.
   */
  def getBackwardCompatibleType(typeName: String): XSDatatype =
    var dt: XSDatatype = null
    typeName match
      case "uriReference"  => dt = AnyURIType.theInstance
      case "number"        => dt = NumberType.theInstance
      case "timeDuration"  => dt = DurationType.theInstance
      case "CDATA"         => dt = NormalizedStringType.theInstance
      case "year"          => dt = GYearType.theInstance
      case "yearMonth"     => dt = GYearMonthType.theInstance
      case "month"         => dt = GMonthType.theInstance
      case "monthDay"      => dt = GMonthDayType.theInstance
      case "day"           => dt = GDayType.theInstance
      case _               =>

    if dt != null then
      reportWarning(GrammarReader.WRN_DEPRECATED_TYPENAME, typeName, dt.displayName())

    dt

  // parsing and related services
  private class InclusionContext(
    val prefixResolver: GrammarReader.PrefixResolver,
    val locator: Locator,
    val systemId: String,
    val previousContext: InclusionContext
  )

  private var pendingIncludes: InclusionContext = scala.compiletime.uninitialized

  private def pushInclusionContext(): Unit =
    pendingIncludes = new InclusionContext(
      prefixResolver,
      locator,
      if locator != null then locator.getSystemId else null,
      pendingIncludes
    )
    prefixResolver = GrammarReader.basePrefixResolver
    locator = null

  private def popInclusionContext(): Unit =
    prefixResolver = pendingIncludes.prefixResolver
    locator = pendingIncludes.locator
    pendingIncludes = pendingIncludes.previousContext

  @throws(classOf[AbortException])
  def resolveLocation(sourceState: State, uri0: String): InputSource =
    var uri = uri0
    try
      uri = combineURI(sourceState.getBaseURI, uri)
      val source = controller.resolveEntity(null, uri)
      if source == null then new InputSource(uri)
      else source
    catch
      case e: IOException =>
        controller.error(e, locator)
        throw AbortException.theInstance
      case e: SAXException =>
        controller.error(e, locator)
        throw AbortException.theInstance

  final def combineURI(baseURI: String, relativeURI: String): String =
    Uri.resolve(baseURI, relativeURI)

  @deprecated final def combineURL(baseURI: String, relativeURI: String): String =
    Uri.resolve(baseURI, relativeURI)

  @throws(classOf[AbortException])
  def switchSource(sourceState: State, url: String, newState: State): Unit =
    if url.indexOf('#') >= 0 then
      reportError(GrammarReader.ERR_FRAGMENT_IDENTIFIER, url)
      throw AbortException.theInstance
    switchSource(resolveLocation(sourceState, url), newState)

  def switchSource(source: InputSource, newState: State): Unit =
    val url = source.getSystemId
    var ic = pendingIncludes
    while ic != null do
      if ic.systemId != null && ic.systemId == url then
        var s = ""
        var i = pendingIncludes
        while i != ic do
          s = i.systemId + " > " + s
          i = i.previousContext
        s = url + " > " + s + url
        reportError(GrammarReader.ERR_RECURSIVE_INCLUDE, s)
        return
      ic = ic.previousContext

    pushInclusionContext()
    val currentState = getCurrentState
    try
      pushState(newState, null, null)
      _parse(source, currentState.getLocation)
    finally
      super.setContentHandler(currentState)
      popInclusionContext()

  override def parse(source: String): Unit = _parse(source, null)
  override def parse(source: InputSource): Unit = _parse(source, null)

  final def _parse(source: AnyRef, errorSource: Locator): Unit =
    try
      val reader = parserFactory.newSAXParser().getXMLReader
      reader.setContentHandler(this)
      reader.setErrorHandler(controller)
      reader.setEntityResolver(controller)
      source match
        case is: InputSource => reader.parse(is)
        case s: String       => reader.parse(s)
        case _               =>
    catch
      case e: ParserConfigurationException => controller.error(e, errorSource)
      case e: IOException                  => controller.error(e, errorSource)
      case e: SAXParseException            => controller.error(e)
      case e: SAXException                 => controller.error(e, errorSource)

  class BackwardReferenceMap:
    private val impl: JMap[AnyRef, JArrayList[Locator]] = new java.util.HashMap[AnyRef, JArrayList[Locator]]()

    def memorizeLink(target: AnyRef): Unit =
      val list =
        if impl.containsKey(target) then impl.get(target)
        else
          val l = new JArrayList[Locator]()
          impl.put(target, l)
          l
      list.add(new LocatorImpl(locator))

    def getReferer(target: AnyRef): Array[Locator] =
      if impl.containsKey(target) then
        val lst = impl.get(target)
        val locs = new Array[Locator](lst.size())
        lst.toArray(locs)
        locs
      else null

  val backwardReference: BackwardReferenceMap = new BackwardReferenceMap()

  private val declaredLocations: JMap[AnyRef, Locator] = new java.util.HashMap[AnyRef, Locator]()

  def setDeclaredLocationOf(o: AnyRef): Unit =
    declaredLocations.put(o, new LocatorImpl(locator))

  def getDeclaredLocationOf(o: AnyRef): Locator =
    declaredLocations.get(o)

  def detectUndefinedOnes(container: ReferenceContainer, errMsg: String): Unit =
    val itr = container.iterator()
    while itr.hasNext do
      val ref = itr.next()
      if !ref.isDefined then
        reportError(backwardReference.getReferer(ref), errMsg, Array[AnyRef](ref.name))
        ref.exp = Expression.nullSet

  def pushState(newState: State, parentState: State, startTag: StartTagInfo): Unit =
    super.setContentHandler(newState)
    newState.init(this, parentState, startTag)

  def popState(): Unit =
    val currentState = getCurrentState
    if currentState.getParentState != null then
      super.setContentHandler(currentState.getParentState)
    else
      super.setContentHandler(new org.xml.sax.helpers.DefaultHandler())

  final def getCurrentState: State = super.getContentHandler.asInstanceOf[State]

  def createExpressionChildState(parent: State, tag: StartTagInfo): State

  override def setDocumentLocator(loc: Locator): Unit =
    super.setDocumentLocator(loc)
    this.locator = loc

  def resolveNamespacePrefix(prefix: String): String =
    prefixResolver.resolve(prefix)

  def isUnparsedEntity(entityName: String): Boolean = true
  def isNotation(notationName: String): Boolean = true
  def getBaseUri: String = getCurrentState.getBaseURI

  final def onID(dt: org.orbeon.msv.relaxng.datatype.Datatype, token: StringToken): Unit = ()

  // back patching
  private val backPatchJobs: JList[GrammarReader.BackPatch] = new JArrayList[GrammarReader.BackPatch]()
  private val delayedBackPatchJobs: JList[XSDatatypeExp] = new JArrayList[XSDatatypeExp]()

  final def addBackPatchJob(job: GrammarReader.BackPatch): Unit =
    backPatchJobs.add(job)

  final def addBackPatchJob(job: XSDatatypeExp): Unit =
    delayedBackPatchJobs.add(job)

  final def runBackPatchJob(): Unit =
    val oldLoc = locator
    runBackPatchJob(backPatchJobs)
    runBackPatchJob(delayedBackPatchJobs.asInstanceOf[JList[GrammarReader.BackPatch]])
    locator = oldLoc

  private final def runBackPatchJob(jobs: JList[GrammarReader.BackPatch]): Unit =
    val itr = jobs.iterator()
    while itr.hasNext do
      val job = itr.next()
      locator = job.getOwnerState.getLocation
      job.patch()

  // error reporting
  final def reportError(propertyName: String): Unit =
    reportError(propertyName, null, null, null)

  final def reportError(propertyName: String, arg1: AnyRef): Unit =
    reportError(propertyName, Array(arg1), null, null)

  final def reportError(propertyName: String, arg1: AnyRef, arg2: AnyRef): Unit =
    reportError(propertyName, Array(arg1, arg2), null, null)

  final def reportError(propertyName: String, arg1: AnyRef, arg2: AnyRef, arg3: AnyRef): Unit =
    reportError(propertyName, Array(arg1, arg2, arg3), null, null)

  final def reportError(nestedException: Exception, propertyName: String): Unit =
    reportError(propertyName, null, nestedException, null)

  final def reportError(nestedException: Exception, propertyName: String, arg1: AnyRef): Unit =
    reportError(propertyName, Array(arg1), nestedException, null)

  final def reportError(locs: Array[Locator], propertyName: String, args: Array[AnyRef]): Unit =
    reportError(propertyName, args, null, locs)

  final def reportWarning(propertyName: String): Unit =
    reportWarning(propertyName, null, null)

  final def reportWarning(propertyName: String, arg1: AnyRef): Unit =
    reportWarning(propertyName, Array(arg1), null)

  final def reportWarning(propertyName: String, arg1: AnyRef, arg2: AnyRef): Unit =
    reportWarning(propertyName, Array(arg1, arg2), null)

  private def prepareLocation(param: Array[Locator]): Array[Locator] =
    if param != null then
      var cnt = 0
      var i = 0
      while i < param.length do
        if param(i) != null then cnt += 1
        i += 1
      if param.length == cnt then return param
      val locs = new Array[Locator](cnt)
      cnt = 0
      i = 0
      while i < param.length do
        if param(i) != null then
          locs(cnt) = param(i)
          cnt += 1
        i += 1
      locs
    else if locator != null then Array(locator)
    else Array.empty[Locator]

  final def reportError(propertyName: String, args: Array[AnyRef], nestedException: Exception, errorLocations: Array[Locator]): Unit =
    controller.error(
      prepareLocation(errorLocations),
      localizeMessage(propertyName, args),
      nestedException
    )

  final def reportWarning(propertyName: String, args: Array[AnyRef], locations: Array[Locator]): Unit =
    controller.warning(
      prepareLocation(locations),
      localizeMessage(propertyName, args)
    )

  protected def localizeMessage(propertyName: String, args: Array[AnyRef]): String

object GrammarReader:

  def createParserFactory(): SAXParserFactory =
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    factory

  trait PrefixResolver:
    def resolve(prefix: String): String

  val basePrefixResolver: PrefixResolver = new PrefixResolver:
    def resolve(prefix: String): String =
      if prefix == "xml" then "http://www.w3.org/XML/1998/namespace"
      else null

  class ChainPrefixResolver(val prefix: String, val uri: String, val previous: PrefixResolver) extends PrefixResolver:
    def resolve(p: String): String =
      if p == prefix then uri
      else previous.resolve(p)

  trait BackPatch:
    def patch(): Unit
    def getOwnerState: State

  val ERR_MALPLACED_ELEMENT = "GrammarReader.MalplacedElement"
  val ERR_CHARACTERS = "GrammarReader.Characters"
  val ERR_DISALLOWED_ATTRIBUTE = "GrammarReader.DisallowedAttribute"
  val ERR_MISSING_ATTRIBUTE = "GrammarReader.MissingAttribute"
  val ERR_BAD_ATTRIBUTE_VALUE = "GrammarReader.BadAttributeValue"
  val ERR_MISSING_ATTRIBUTE_2 = "GrammarReader.MissingAttribute.2"
  val ERR_CONFLICTING_ATTRIBUTES = "GrammarReader.ConflictingAttribute"
  val ERR_RECURSIVE_INCLUDE = "GrammarReader.RecursiveInclude"
  val ERR_FRAGMENT_IDENTIFIER = "GrammarReader.FragmentIdentifier"
  val ERR_UNDEFINED_DATATYPE = "GrammarReader.UndefinedDataType"
  val ERR_DATATYPE_ALREADY_DEFINED = "GrammarReader.DataTypeAlreadyDefined"
  val ERR_MISSING_CHILD_EXPRESSION = "GrammarReader.Abstract.MissingChildExpression"
  val ERR_MORE_THAN_ONE_CHILD_EXPRESSION = "GrammarReader.Abstract.MoreThanOneChildExpression"
  val ERR_MORE_THAN_ONE_CHILD_TYPE = "GrammarReader.Abstract.MoreThanOneChildType"
  val ERR_MISSING_CHILD_TYPE = "GrammarReader.Abstract.MissingChildType"
  val ERR_ILLEGAL_FINAL_VALUE = "GrammarReader.IllegalFinalValue"
  val ERR_RUNAWAY_EXPRESSION = "GrammarReader.Abstract.RunAwayExpression"
  val ERR_MISSING_TOPLEVEL = "GrammarReader.Abstract.MissingTopLevel"
  val WRN_MAYBE_WRONG_NAMESPACE = "GrammarReader.Warning.MaybeWrongNamespace"
  val WRN_DEPRECATED_TYPENAME = "GrammarReader.Warning.DeprecatedTypeName"
  val ERR_BAD_TYPE = "GrammarReader.BadType"
  val ERR_RECURSIVE_DATATYPE = "GrammarReader.RecursiveDatatypeDefinition"
