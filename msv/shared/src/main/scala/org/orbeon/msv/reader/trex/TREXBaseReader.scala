package org.orbeon.msv.reader.trex

import javax.xml.parsers.SAXParserFactory
import org.orbeon.msv.grammar.{Expression, ExpressionPool, Grammar}
import org.orbeon.msv.grammar.trex.TREXGrammar
import org.orbeon.msv.reader.*
import org.orbeon.msv.util.{LightStack, StartTagInfo}
import org.xml.sax.Attributes

/**
 * reads TREX grammar from SAX2 and constructs abstract grammar model.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class TREXBaseReader(
  _controller: GrammarReaderController,
  parserFactory: SAXParserFactory,
  pool: ExpressionPool,
  val sfactory: TREXBaseReader.StateFactory,
  rootState: State
) extends GrammarReader(_controller, parserFactory, pool, rootState):

  protected def localizeMessage(propertyName: String, args: Array[AnyRef]): String =
    val format = TREXBaseReader.messages.getOrElse(propertyName, propertyName)
    if args == null || args.isEmpty then format
    else
      var res = format
      for i <- args.indices do
        val str = if args(i) != null then args(i).toString else "null"
        res = res.replace("{" + i + "}", str)
      res.replace("''", "'")

  /** grammar object currently being loaded. */
  var grammar: TREXGrammar = scala.compiletime.uninitialized

  /** obtains parsed grammar object only if parsing was successful. */
  final def getResult(): TREXGrammar =
    if controller.hadError then null
    else grammar

  def getResultAsGrammar: Grammar = getResult()

  /** stack that stores value of ancestor 'ns' attribute. */
  private val nsStack: LightStack = new LightStack()
  /** target namespace: currently active 'ns' attribute */
  var targetNamespace: String = ""
  final def getTargetNamespace(): String = targetNamespace

  /** stack that stores value of ancestor 'prefix' attribute. */
  private val prefixStack: LightStack = new LightStack()
  /** target prefix: currently active 'prefix' attribute */
  var targetPrefix: String = ""
  final def getTargetPrefix(): String = targetPrefix

  def createNameClassChildState(parent: State, tag: StartTagInfo): State =
    if tag.localName == "name" then sfactory.nsName(parent, tag)
    else if tag.localName == "anyName" then sfactory.nsAnyName(parent, tag)
    else if tag.localName == "nsName" then sfactory.nsNsName(parent, tag)
    else if tag.localName == "not" then sfactory.nsNot(parent, tag)
    else if tag.localName == "difference" then sfactory.nsDifference(parent, tag)
    else if tag.localName == "choice" then sfactory.nsChoice(parent, tag)
    else null

  override def createExpressionChildState(parent: State, tag: StartTagInfo): State =
    if tag.localName == "element" then sfactory.element(parent, tag)
    else if tag.localName == "attribute" then sfactory.attribute(parent, tag)
    else if tag.localName == "group" then sfactory.group(parent, tag)
    else if tag.localName == "interleave" then sfactory.interleave(parent, tag)
    else if tag.localName == "choice" then sfactory.choice(parent, tag)
    else if tag.localName == "optional" then sfactory.optional(parent, tag)
    else if tag.localName == "zeroOrMore" then sfactory.zeroOrMore(parent, tag)
    else if tag.localName == "oneOrMore" then sfactory.oneOrMore(parent, tag)
    else if tag.localName == "mixed" then sfactory.mixed(parent, tag)
    else if tag.localName == "ref" then sfactory.ref(parent, tag)
    else if tag.localName == "empty" then sfactory.empty(parent, tag)
    else if tag.localName == "notAllowed" then sfactory.notAllowed(parent, tag)
    else if tag.localName == "grammar" then sfactory.grammar(parent, tag)
    else null

  def wrapUp(): Unit = ()

  override def startElement(a: String, b: String, c: String, d: Attributes): Unit =
    // handle attribute 'prefix' propagation
    prefixStack.push(targetPrefix)
    val p = c.indexOf(':')
    if p != -1 then
      targetPrefix = c.substring(0, p)

    // handle 'ns' attribute propagation
    nsStack.push(targetNamespace)
    val nsIdx = d.getIndex("ns")
    if nsIdx != -1 then
      targetNamespace = d.getValue(nsIdx)

    super.startElement(a, b, c, d)

  override def endElement(a: String, b: String, c: String): Unit =
    super.endElement(a, b, c)
    targetNamespace = nsStack.pop().asInstanceOf[String]
    targetPrefix = prefixStack.pop().asInstanceOf[String]

object TREXBaseReader:
  abstract class StateFactory:
    def nsName(parent: State, tag: StartTagInfo): State = new NameClassNameState()
    def nsAnyName(parent: State, tag: StartTagInfo): State = new NameClassAnyNameState()
    def nsNsName(parent: State, tag: StartTagInfo): State = new NameClassNsNameState()
    def nsNot(parent: State, tag: StartTagInfo): State = new NameClassNotState()
    def nsDifference(parent: State, tag: StartTagInfo): State = new NameClassDifferenceState()
    def nsChoice(parent: State, tag: StartTagInfo): State = new NameClassChoiceState()

    def element(parent: State, tag: StartTagInfo): State = new ElementState()
    def attribute(parent: State, tag: StartTagInfo): State = new AttributeState()
    def group(parent: State, tag: StartTagInfo): State = new SequenceState()
    def interleave(parent: State, tag: StartTagInfo): State = new InterleaveState()
    def choice(parent: State, tag: StartTagInfo): State = new ChoiceState()
    def optional(parent: State, tag: StartTagInfo): State = new OptionalState()
    def zeroOrMore(parent: State, tag: StartTagInfo): State = new ZeroOrMoreState()
    def oneOrMore(parent: State, tag: StartTagInfo): State = new OneOrMoreState()
    def mixed(parent: State, tag: StartTagInfo): State = new MixedState()
    def empty(parent: State, tag: StartTagInfo): State = new TerminalState(Expression.epsilon)
    def notAllowed(parent: State, tag: StartTagInfo): State = new TerminalState(Expression.nullSet)
    def includeGrammar(parent: State, tag: StartTagInfo): State = new IncludeMergeState()
    def grammar(parent: State, tag: StartTagInfo): State = new GrammarState()
    def start(parent: State, tag: StartTagInfo): State = new StartState()
    def define(parent: State, tag: StartTagInfo): State
    def ref(parent: State, tag: StartTagInfo): State =
      new RefState("true" == tag.getAttribute("parent"))
    def divInGrammar(parent: State, tag: StartTagInfo): State = null

    def createGrammar(pool: ExpressionPool, parent: TREXGrammar): TREXGrammar =
      new TREXGrammar(pool, parent)

    def includedGrammar(): State =
      new RootMergedGrammarState()

  // error messages
  val ERR_MISSING_CHILD_NAMECLASS = "TREXGrammarReader.MissingChildNameClass"
  val ERR_MORE_THAN_ONE_NAMECLASS = "TREXGrammarReader.MoreThanOneNameClass"
  val ERR_UNDECLARED_PREFIX = "TREXGrammarReader.UndeclaredPrefix"
  val ERR_UNDEFINED_PATTERN = "TREXGrammarReader.UndefinedPattern"
  val ERR_UNKNOWN_DATATYPE_VOCABULARY = "TREXGrammarReader.UnknownDataTypeVocabulary"
  val ERR_BAD_COMBINE = "TREXGrammarReader.BadCombine"
  val ERR_COMBINE_MISSING = "TREXGrammarReader.CombineMissing"
  val WRN_COMBINE_IGNORED = "TREXGrammarReader.Warning.CombineIgnored"
  val WRN_OBSOLETED_XMLSCHEMA_NAMSPACE = "TREXGrammarReader.Warning.ObsoletedXMLSchemaNamespace"
  val ERR_DUPLICATE_DEFINITION = "TREXGrammarReader.DuplicateDefinition"
  val ERR_NONEXISTENT_PARENT_GRAMMAR = "TREXGrammarReader.NonExistentParentGrammar"
  val ERR_INTERLEAVED_STRING = "TREXGrammarReader.InterleavedString"
  val ERR_SEQUENCED_STRING = "TREXGrammarReader.SequencedString"
  val ERR_REPEATED_STRING = "TREXGrammarReader.RepeatedString"
  val ERR_INTERLEAVED_ANYSTRING = "TREXGrammarReader.InterleavedAnyString"

  val messages: Map[String, String] = Map(
    "GrammarReader.Abstract.MissingChildExpression" -> "child pattern is missing",
    "GrammarReader.Abstract.MoreThanOneChildExpression" -> "more than one child pattern is specified where only one is allowed",
    "GrammarReader.Abstract.RunAwayExpression" -> "the following patterns form an infinite recursion ( {0} )",
    "GrammarReader.Abstract.MissingTopLevel" -> "<start> declaration was not found.",
    "GrammarReader.MissingAttribute" -> "attribute \"{1}\" is missing from element \"{0}\"",
    "TREXGrammarReader.MissingChildNameClass" -> "name attribute or name class is required",
    "TREXGrammarReader.MoreThanOneNameClass" -> "more than one name class is specified",
    "TREXGrammarReader.UndeclaredPrefix" -> "namespace prefix \"{0}\" is not declared",
    "TREXGrammarReader.UndefinedPattern" -> "reference to the undefined pattern \"{0}\"",
    "TREXGrammarReader.UnknownDataTypeVocabulary" -> "\"{0}\" is not a recognized data type vocabulary",
    "TREXGrammarReader.FailedToParseDataType" -> "failed to parse datatype",
    "TREXGrammarReader.BadCombine" -> "\"{0}\" is a bad value for the \"combine\" attribute",
    "TREXGrammarReader.CombineMissing" -> "pattern \"{0}\" is already defined. Specify \"combine\" attribute to combine patterns, or change pattern name to avoid conflict.",
    "TREXGrammarReader.Warning.CombineIgnored" -> "combine attribute is specified, but no predecessor exists.",
    "TREXGrammarReader.Warning.ObsoletedXMLSchemaNamespace" -> "\"{0}\" is the old namespace URI. Please use \"http://www.w3.org/2001/XMLSchema\"",
    "TREXGrammarReader.DuplicateDefinition" -> "pattern \"{0}\" is defined more than once in the same file.",
    "TREXGrammarReader.NonExistentParentGrammar" -> "reference to the nonexistent parent grammar.",
    "TREXGrammarReader.InterleavedString" -> "\"string\" or datatype element is used within <interleave> element.",
    "TREXGrammarReader.SequencedString" -> "\"string\" or datatype element is used within <group> element.",
    "TREXGrammarReader.RepeatedString" -> "\"string\" or datatype element cannot be repeated.",
    "TREXGrammarReader.InterleavedAnyString" -> "<interleave> cannot contain <text/> in more than one branch"
  )
