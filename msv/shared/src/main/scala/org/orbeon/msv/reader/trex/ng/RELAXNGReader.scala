package org.orbeon.msv.reader.trex.ng

import java.util.{ArrayList as JArrayList, HashMap, HashSet, Iterator as JIterator, List as JList, Map as JMap, Set as JSet}
import javax.xml.parsers.SAXParserFactory
import org.orbeon.msv.datatype.ErrorDatatypeLibrary
import org.orbeon.msv.grammar.{Expression, ExpressionPool, ReferenceExp}
import org.orbeon.msv.grammar.trex.TREXGrammar
import org.orbeon.msv.reader.*
import org.orbeon.msv.reader.trex.*
import org.orbeon.msv.util.{LightStack, StartTagInfo, Uri}
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeException, DatatypeLibrary, DatatypeLibraryFactory}
import org.xml.sax.{Attributes, InputSource, Locator, SAXException}

/**
 * reads RELAX NG grammar from SAX2 and constructs abstract grammar model.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RELAXNGReader(
  _controller: GrammarReaderController,
  parserFactory: SAXParserFactory,
  stateFactory: RELAXNGReader.StateFactory,
  pool: ExpressionPool
) extends TREXBaseReader(_controller, parserFactory, pool, stateFactory, new RootState()):

  def this(controller: GrammarReaderController, parserFactory: SAXParserFactory) =
    this(controller, parserFactory, new RELAXNGReader.StateFactory(), new ExpressionPool())

  def this(controller: GrammarReaderController) =
    this(controller, GrammarReader.createParserFactory())

  protected override def localizeMessage(propertyName: String, args: Array[AnyRef]): String =
    val format = RELAXNGReader.messages.getOrElse(propertyName, super.localizeMessage(propertyName, args))
    if args == null || args.isEmpty then format
    else
      var res = format
      for i <- args.indices do
        val str = if args(i) != null then args(i).toString else "null"
        res = res.replace("{" + i + "}", str)
      res.replace("''", "'")

  def getGrammar: TREXGrammar = grammar

  private val refExpParseInfos: JMap[ReferenceExp, RELAXNGReader.RefExpParseInfo] =
    new HashMap[ReferenceExp, RELAXNGReader.RefExpParseInfo]()

  def getRefExpParseInfo(exp: ReferenceExp): RELAXNGReader.RefExpParseInfo =
    var r = refExpParseInfos.get(exp)
    if r == null then
      r = new RELAXNGReader.RefExpParseInfo()
      refExpParseInfos.put(exp, r)
    r

  var currentNamedPattern: RELAXNGReader.RefExpParseInfo = null
  var directReference: Boolean = true

  private var datatypeLibraryFactory: DatatypeLibraryFactory = new DefaultDatatypeLibraryFactory()

  def getDatatypeLibraryFactory: DatatypeLibraryFactory = datatypeLibraryFactory
  def setDatatypeLibraryFactory(factory: DatatypeLibraryFactory): Unit =
    this.datatypeLibraryFactory = factory

  def getStateFactory: RELAXNGReader.StateFactory =
    sfactory.asInstanceOf[RELAXNGReader.StateFactory]

  protected[msv] override def isGrammarElement(tag: StartTagInfo): Boolean =
    RELAXNGReader.RELAXNGNamespace == tag.namespaceURI ||
    "http://relaxng.org/ns/structure/0.9" == tag.namespaceURI

  override def createNameClassChildState(parent: State, tag: StartTagInfo): State =
    if tag.localName == "name" then sfactory.nsName(parent, tag)
    else if tag.localName == "anyName" then sfactory.nsAnyName(parent, tag)
    else if tag.localName == "nsName" then sfactory.nsNsName(parent, tag)
    else if tag.localName == "choice" then sfactory.nsChoice(parent, tag)
    else null

  override def createExpressionChildState(parent: State, tag: StartTagInfo): State =
    if tag.localName == "text" then getStateFactory.text(parent, tag)
    else if tag.localName == "data" then getStateFactory.data(parent, tag)
    else if tag.localName == "value" then getStateFactory.value(parent, tag)
    else if tag.localName == "list" then getStateFactory.list(parent, tag)
    else if tag.localName == "externalRef" then getStateFactory.externalRef(parent, tag)
    else if tag.localName == "parentRef" then getStateFactory.parentRef(parent, tag)
    else super.createExpressionChildState(parent, tag)

  def resolveDataType(localName: String): Datatype =
    try
      getCurrentDatatypeLibrary.createDatatype(localName)
    catch
      case dte: DatatypeException =>
        reportError(RELAXNGReader.ERR_UNDEFINED_DATATYPE_1, localName, dte.getMessage)
        org.orbeon.msv.datatype.xsd.StringType.theInstance

  def resolveDataTypeLibrary(uri: String): DatatypeLibrary =
    try
      val lib = datatypeLibraryFactory.createDatatypeLibrary(uri)
      if lib != null then return lib
      reportError(TREXBaseReader.ERR_UNKNOWN_DATATYPE_VOCABULARY, uri)
    catch
      case e: Throwable =>
        reportError(RELAXNGReader.ERR_UNKNOWN_DATATYPE_VOCABULARY_1, uri, e.toString)
    ErrorDatatypeLibrary.theInstance

  private class AbortLoopException extends Exception

  private def checkRunawayExpression(
    node: ReferenceExp,
    items: JList[ReferenceExp],
    visitedExps: JSet[ReferenceExp]
  ): Unit =
    if !visitedExps.add(node) then return
    items.add(node)

    val directItr = getRefExpParseInfo(node).directRefs.iterator()
    while directItr.hasNext do
      val child = directItr.next()
      var idx = items.lastIndexOf(child)
      if idx != -1 then
        var s = ""
        val locs = new JArrayList[Locator]()
        while idx < items.size() do
          val e = items.get(idx)
          if e.name != null then
            if s.nonEmpty then s += " > "
            s += e.name
            val loc = getDeclaredLocationOf(e)
            if loc != null then locs.add(loc)
          idx += 1

        s += " > " + child.name
        reportError(
          locs.toArray(new Array[Locator](locs.size())),
          GrammarReader.ERR_RUNAWAY_EXPRESSION,
          Array[AnyRef](s)
        )
        throw new AbortLoopException()

      checkRunawayExpression(child, items, visitedExps)

    val empty = new JArrayList[ReferenceExp]()
    val indirectItr = getRefExpParseInfo(node).indirectRefs.iterator()
    while indirectItr.hasNext do
      checkRunawayExpression(indirectItr.next(), empty, visitedExps)

    items.remove(items.size() - 1)

  val restrictionChecker: RestrictionChecker = new RestrictionChecker(this)

  override def wrapUp(): Unit =
    try
      checkRunawayExpression(grammar, new JArrayList[ReferenceExp](), new HashSet[ReferenceExp]())
    catch
      case _: AbortLoopException =>

    if !controller.hadError then
      grammar.visit(new TREXSequencedStringChecker(this, true))

    if !controller.hadError then
      restrictionChecker.check()

  private var datatypeLib: DatatypeLibrary = resolveDataTypeLibrary("")
  var datatypeLibURI: String = ""

  def getCurrentDatatypeLibrary: DatatypeLibrary =
    if datatypeLib == null then
      datatypeLib = resolveDataTypeLibrary(datatypeLibURI)
      if datatypeLib == null then throw new Error()
    datatypeLib

  private val dtLibStack: LightStack = new LightStack()
  private val dtLibURIStack: LightStack = new LightStack()

  override def resolveNamespacePrefix(prefix: String): String =
    if prefix == "" then targetNamespace
    else super.resolveNamespacePrefix(prefix)

  override def startDocument(): Unit =
    dtLibStack.push(datatypeLib)
    dtLibURIStack.push(datatypeLibURI)
    datatypeLib = resolveDataTypeLibrary("")
    datatypeLibURI = ""
    super.startDocument()

  override def endDocument(): Unit =
    super.endDocument()
    datatypeLib = dtLibStack.pop().asInstanceOf[DatatypeLibrary]
    datatypeLibURI = dtLibURIStack.pop().asInstanceOf[String]

  override def startElement(a: String, b: String, c: String, d: Attributes): Unit =
    dtLibStack.push(datatypeLib)
    dtLibURIStack.push(datatypeLibURI)
    val dtLibIdx = d.getIndex("datatypeLibrary")
    if dtLibIdx != -1 then
      datatypeLibURI = d.getValue(dtLibIdx)
      datatypeLib = null
      if !Uri.isAbsolute(datatypeLibURI) then
        reportError(RELAXNGReader.ERR_NOT_ABSOLUTE_URI, datatypeLibURI)
      if datatypeLibURI.indexOf('#') >= 0 then
        reportError(GrammarReader.ERR_FRAGMENT_IDENTIFIER, datatypeLibURI)

    super.startElement(a, b, c, d)

  override def endElement(a: String, b: String, c: String): Unit =
    super.endElement(a, b, c)
    datatypeLib = dtLibStack.pop().asInstanceOf[DatatypeLibrary]
    datatypeLibURI = dtLibURIStack.pop().asInstanceOf[String]

object RELAXNGReader:
  val RELAXNGNamespace: String = "http://relaxng.org/ns/structure/1.0"

  def parse(grammarURL: String, factory: SAXParserFactory, controller: GrammarReaderController): TREXGrammar =
    val reader = new RELAXNGReader(controller, factory)
    reader.parse(grammarURL)
    reader.getResult()

  def parse(grammar: InputSource, factory: SAXParserFactory, controller: GrammarReaderController): TREXGrammar =
    val reader = new RELAXNGReader(controller, factory)
    reader.parse(grammar)
    reader.getResult()

  def parse(grammarURL: String, controller: GrammarReaderController): TREXGrammar =
    parse(grammarURL, GrammarReader.createParserFactory(), controller)

  def parse(grammar: InputSource, controller: GrammarReaderController): TREXGrammar =
    parse(grammar, GrammarReader.createParserFactory(), controller)

  class RefExpParseInfo:
    var haveHead: Boolean = false
    var combineMethod: String = null
    var redefinition: RefExpParseInfo.RedefinitionStatus = RefExpParseInfo.notBeingRedefined

    def set(rhs: RefExpParseInfo): Unit =
      this.haveHead = rhs.haveHead
      this.combineMethod = rhs.combineMethod
      this.redefinition = rhs.redefinition

    val directRefs: JList[ReferenceExp] = new JArrayList[ReferenceExp]()
    val indirectRefs: JList[ReferenceExp] = new JArrayList[ReferenceExp]()

  object RefExpParseInfo:
    sealed trait RedefinitionStatus
    val notBeingRedefined: RedefinitionStatus = new RedefinitionStatus {}
    val originalNotFoundYet: RedefinitionStatus = new RedefinitionStatus {}
    val originalFound: RedefinitionStatus = new RedefinitionStatus {}

  class StateFactory extends TREXBaseReader.StateFactory:
    override def nsAnyName(parent: State, tag: StartTagInfo): State = new NGNameState.AnyNameState()
    override def nsNsName(parent: State, tag: StartTagInfo): State = new NGNameState.NsNameState()
    def nsExcept(parent: State, tag: StartTagInfo): State = new NameClassChoiceState()

    def text(parent: State, tag: StartTagInfo): State = new TerminalState(Expression.anyString)
    def data(parent: State, tag: StartTagInfo): State = new DataState()
    def dataParam(parent: State, tag: StartTagInfo): State = new DataParamState()
    def value(parent: State, tag: StartTagInfo): State = new ValueState()
    def list(parent: State, tag: StartTagInfo): State = new ListState()
    def define(parent: State, tag: StartTagInfo): State = new org.orbeon.msv.reader.trex.ng.DefineState()
    override def start(parent: State, tag: StartTagInfo): State = new StartState()
    def redefine(parent: State, tag: StartTagInfo): State = new org.orbeon.msv.reader.trex.ng.DefineState()
    def redefineStart(parent: State, tag: StartTagInfo): State = new StartState()
    override def includeGrammar(parent: State, tag: StartTagInfo): State = new IncludeMergeState()
    def externalRef(parent: State, tag: StartTagInfo): State = new IncludePatternState()
    override def divInGrammar(parent: State, tag: StartTagInfo): State = new DivInGrammarState()
    def dataExcept(parent: State, tag: StartTagInfo): State = new ChoiceState()
    override def attribute(parent: State, tag: StartTagInfo): State = new AttributeState()
    override def element(parent: State, tag: StartTagInfo): State = new ElementState()
    override def grammar(parent: State, tag: StartTagInfo): State = new GrammarState()
    override def ref(parent: State, tag: StartTagInfo): State = new RefState(false)
    def parentRef(parent: State, tag: StartTagInfo): State = new RefState(true)

  val ERR_BAD_FACET = "RELAXNGReader.BadFacet"
  val ERR_INVALID_PARAMETERS = "RELAXNGReader.InvalidParameters"
  val ERR_BAD_DATA_VALUE = "RELAXNGReader.BadDataValue"
  val ERR_UNDEFINED_KEY = "RELAXNGReader.UndefinedKey"
  val ERR_UNDEFINED_DATATYPE_1 = "RELAXNGReader.UndefinedDataType1"
  val ERR_INCONSISTENT_KEY_TYPE = "RELAXNGReader.InconsistentKeyType"
  val ERR_INCONSISTENT_COMBINE = "RELAXNGReader.InconsistentCombine"
  val ERR_REDEFINING_UNDEFINED = "RELAXNGReader.RedefiningUndefined"
  val ERR_UNKNOWN_DATATYPE_VOCABULARY_1 = "RELAXNGReader.UnknownDatatypeVocabulary1"
  val ERR_MULTIPLE_EXCEPT = "RELAXNGReader.MultipleExcept"
  val ERR_NOT_ABSOLUTE_URI = "RELAXNGReader.NotAbsoluteURI"
  val ERR_INFOSET_URI_ATTRIBUTE = "RELAXNGReader.InfosetUriAttribute"
  val ERR_XMLNS_ATTRIBUTE = "RELAXNGReader.XmlnsAttribute"
  val ERR_NAKED_INFINITE_ATTRIBUTE_NAMECLASS = "RELAXNGReader.NakedInfiniteAttributeNameClass"

  val messages: Map[String, String] = Map(
    "TREXGrammarReader.InterleavedAnyString" -> "<interleave> cannot contain <text/> in more than one branch",
    "RELAXNGReader.BadFacet" -> "cannot set parameter {0} to this datatype: {1}",
    "RELAXNGReader.InvalidParameters" -> "invalid parameter setting: {0}",
    "RELAXNGReader.BadDataValue" -> "invalid value for the type \"{0}\": {1}",
    "RELAXNGReader.UndefinedKey" -> "reference to the undefined key \"{0}\"",
    "RELAXNGReader.UndefinedDataType1" -> "datatype \"{0}\" is not defined: {1}",
    "RELAXNGReader.InconsistentKeyType" -> "types of all keys/keyrefs \"{0}\" must be the same",
    "RELAXNGReader.InconsistentCombine" -> "different combine methods are used for the same pattern \"{0}\"",
    "RELAXNGReader.RedefiningUndefined" -> "pattern \"{0}\" is redefined but it is not defined in the included file.",
    "RELAXNGReader.UnknownDatatypeVocabulary1" -> "\"{0}\" is not a recognized data type vocabulary: {1}",
    "RELAXNGReader.MultipleExcept" -> "only one <except> is allowed",
    "RELAXNGReader.NotAbsoluteURI" -> "\"{0}\" is not an absolute URI",
    "RELAXNGReader.AttributeInExcept" -> "<data> cannot contain <attribute>",
    "RELAXNGReader.ElementInExcept" -> "<data> cannot contain <element>",
    "RELAXNGReader.ListInExcept" -> "<data> cannot contain <list>",
    "RELAXNGReader.TextInExcept" -> "<data> cannot contain <text>",
    "RELAXNGReader.EmptyInExcept" -> "<data> cannot contain <empty>",
    "RELAXNGReader.SequenceInExcept" -> "<data> cannot contain <group>",
    "RELAXNGReader.InterleaveInExcept" -> "<data> cannot contain <interleave>",
    "RELAXNGReader.OneOrMoreInExcept" -> "<data> cannot contain <oneOrMore>/<zeroOrMore>",
    "RELAXNGReader.RepeatedGroupedAttribute" -> "<attribute> is grouped and that group is repeated",
    "RELAXNGReader.ElementInAttribute" -> "<attribute> cannot contain <element>",
    "RELAXNGReader.AttributeInAttribute" -> "<attribute> cannot contain <attribute>",
    "RELAXNGReader.AttributeInList" -> "<list> cannot contain <attribute>",
    "RELAXNGReader.ElementInList" -> "<list> cannot contain <element>",
    "RELAXNGReader.ListInList" -> "<list> cannot contain <list>",
    "RELAXNGReader.TextInList" -> "<list> cannot contain <text>. Use <data type=\"string\"/>.",
    "RELAXNGReader.AttributeInStart" -> "<start> cannot contain <attribute>",
    "RELAXNGReader.ListInStart" -> "<start> cannot contain <list>",
    "RELAXNGReader.TextInStart" -> "<start> cannot contain <text>",
    "RELAXNGReader.EmptyInStart" -> "<start> cannot contain <empty>",
    "RELAXNGReader.SequenceInStart" -> "<start> cannot contain <group>",
    "RELAXNGReader.InterleaveInStart" -> "<start> cannot contain <interleave>",
    "RELAXNGReader.DataInStart" -> "<start> cannot contain <data>",
    "RELAXNGReader.OneOrMoreInStart" -> "<start> cannot contain <oneOrMore>/<zeroOrMore>",
    "RELAXNGReader.DataInInterleaveInList" -> "<data> is not allowed inside <interleave> inside <list>",
    "RELAXNGReader.ValueInInterleaveInList" -> "<value> is not allowed inside <interleave> inside <list>",
    "RELAXNGReader.AnyNameInAnyName" -> "<anyName> cannot contain <anyName>",
    "RELAXNGReader.AnyNameInNsName" -> "<nsName> cannot contain <anyName>",
    "RELAXNGReader.NsNameInNsName" -> "<nsName> cannot contain <nsName>",
    "RELAXNGReader.DuplicateAttributes" -> "more than one attribute accepts the same name \"{0}\"",
    "RELAXNGReader.DuplicateElements" -> "more than one branch of <interleave> accepts the element \"{0}\"",
    "RELAXNGReader.InfosetUriAttribute" -> "Namespace URI \"http://www.w3.org/2000/xmlns\" is reserved and cannot be used",
    "RELAXNGReader.XmlnsAttribute" -> "Attribute cannot have \"xmlns\" as its name",
    "RELAXNGReader.NakedInfiniteAttributeNameClass" -> "Attribute with <nsName>/<anyName> must be wrapped by <zeroOrMore>/<oneOrMore>"
  )
