package org.orbeon.msv.reader.trex.ng.comp

import java.util.{HashMap, Map as JMap}
import javax.xml.parsers.SAXParserFactory
import org.orbeon.msv.grammar.{AttributeExp, ExpressionPool}
import org.orbeon.msv.grammar.relaxng.RELAXNGGrammar
import org.orbeon.msv.grammar.trex.TREXGrammar
import org.orbeon.msv.reader.{GrammarReader, GrammarReaderController, State}
import org.orbeon.msv.reader.trex.ng.RELAXNGReader
import org.orbeon.msv.util.{LightStack, StartTagInfo}
import org.xml.sax.{Attributes, InputSource, Locator}

/**
 * reads RELAX NG grammar with DTD compatibility annotation
 * and constructs abstract grammar model.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RELAXNGCompReader(
  _controller: GrammarReaderController,
  parserFactory: SAXParserFactory,
  stateFactory: RELAXNGCompReader.StateFactory,
  pool: ExpressionPool
) extends RELAXNGReader(_controller, parserFactory, stateFactory, pool):

  def this(controller: GrammarReaderController, parserFactory: SAXParserFactory, pool: ExpressionPool) =
    this(controller, parserFactory, new RELAXNGCompReader.StateFactory(), pool)

  def this(controller: GrammarReaderController) =
    this(controller, GrammarReader.createParserFactory(), new ExpressionPool())

  private val defaultedAttributes: JMap[AttributeExp, String] = new HashMap[AttributeExp, String]()
  private val lastRNGElement: LightStack = new LightStack()
  private var inAnnotation: Boolean = false

  lastRNGElement.push(null)

  def addDefaultValue(exp: AttributeExp, value: String): Unit =
    setDeclaredLocationOf(exp)
    if defaultedAttributes.put(exp, value) != null then
      throw new Error("attribute already processed")

  protected override def localizeMessage(propertyName: String, args: Array[AnyRef]): String =
    val format = RELAXNGCompReader.messages.getOrElse(propertyName, super.localizeMessage(propertyName, args))
    if args == null || args.isEmpty then format
    else
      var res = format
      for i <- args.indices do
        val str = if args(i) != null then args(i).toString else "null"
        res = res.replace("{" + i + "}", str)
      res.replace("''", "'")

  override def wrapUp(): Unit =
    super.wrapUp()
    if !controller.hadError then
      new DefAttCompatibilityChecker(this, defaultedAttributes).test()
      new IDCompatibilityChecker(this).test()

  override def startElement(uri: String, local: String, qname: String, atts: Attributes): Unit =
    super.startElement(uri, local, qname, atts)

    if inAnnotation then
      reportWarning(RELAXNGCompReader.CERR_ANN_CHILD_ELEMENT, null, Array[Locator](locator))
      grammar.asInstanceOf[RELAXNGGrammar].isAnnotationCompatible = false

    if uri == RELAXNGCompReader.AnnotationNamespace && local == "annotation" then
      for i <- 0 until atts.getLength do
        val attUri = atts.getURI(i)
        if attUri == "" || attUri == RELAXNGCompReader.AnnotationNamespace || attUri == RELAXNGReader.RELAXNGNamespace then
          reportWarning(
            RELAXNGCompReader.CERR_ANN_INVALID_ATTRIBUTE,
            Array[AnyRef](atts.getQName(i)),
            Array[Locator](locator)
          )
          grammar.asInstanceOf[RELAXNGGrammar].isAnnotationCompatible = false

      if lastRNGElement.size() != 0 then
        val top = lastRNGElement.top()
        if top != null && top != "value" && top != "param" && top != "name" then
          reportWarning(
            RELAXNGCompReader.CERR_ANN_MISPLACED,
            Array[AnyRef](top),
            Array[Locator](locator)
          )
          grammar.asInstanceOf[RELAXNGGrammar].isAnnotationCompatible = false

      inAnnotation = true

    lastRNGElement.push(null)

  override def endElement(uri: String, local: String, qname: String): Unit =
    super.endElement(uri, local, qname)
    inAnnotation = false
    lastRNGElement.pop()
    if uri == RELAXNGReader.RELAXNGNamespace then
      lastRNGElement.pop()
      lastRNGElement.push(local)

object RELAXNGCompReader:
  val AnnotationNamespace: String = "http://relaxng.org/ns/compatibility/annotations/1.0"

  def parse(grammarURL: String, controller: GrammarReaderController): TREXGrammar =
    val reader = new RELAXNGCompReader(controller)
    reader.parse(grammarURL)
    reader.getResult()

  def parse(grammar: InputSource, controller: GrammarReaderController): TREXGrammar =
    val reader = new RELAXNGCompReader(controller)
    reader.parse(grammar)
    reader.getResult()

  class StateFactory extends RELAXNGReader.StateFactory:
    override def attribute(parent: State, tag: StartTagInfo): State = new CompAttributeState()
    override def createGrammar(pool: ExpressionPool, parent: TREXGrammar): TREXGrammar =
      new RELAXNGGrammar(pool, parent)

  val CERR_ANN_CHILD_ELEMENT = "RELAXNGReader.Compatibility.Annotation.ChildElement"
  val CERR_ANN_MISPLACED = "RELAXNGReader.Compatibility.Annotation.Misplaced"
  val CERR_ANN_INVALID_ATTRIBUTE = "RELAXNGReader.Compatibility.Annotation.InvalidAttribute"

  val messages: Map[String, String] = Map(
    "RELAXNGReader.Compatibility.Annotation.ChildElement" -> "the annotation element cannot have any child element.",
    "RELAXNGReader.Compatibility.Annotation.InvalidAttribute" -> "the annotation element cannot have the \"{0}\" attribute.",
    "RELAXNGReader.Compatibility.Annotation.Misplaced" -> "the annotation element cannot be written after the \"{0}\" element.",
    "RELAXNGReader.Compatibility.ID.MalplacedIDType" -> "The \"{0}\" type has {1} semantics and therefore it must be placed directly under an attribute pattern.",
    "RELAXNGReader.Compatibility.ID.IDTypeWithNonSimpleAttName" -> "The \"{0}\" type has {1} semantics and therefore its parent attribute pattern must have a simple name.",
    "RELAXNGReader.Compatibility.ID.IDTypeWithNonSimpleElementName" -> "The \"{0}\" type has {1} semantics and therefore the enclosing element pattern must have a simple name.",
    "RELAXNGReader.Compatibility.ID.Competing" -> "The \"{0}\" type has {1} semantics but a competing attribute of another element doesn't have the same type.",
    "RELAXNGReader.Compatibility.ID.Competing2" -> "The \"{1}\" type of the \"{0}\" attribute has ID semantics but a competing attribute doesn't have the same type.",
    "RELAXNGReader.Compatibility.DefaultValue.NameIsNotSimple" -> "For an attribute to have a default value, its name must be simple.",
    "RELAXNGReader.Compatibility.DefaultValue.Invalid" -> "The specified default value \"{0}\" does not match the content model of the attribute.",
    "RELAXNGReader.Compatibility.DefaultValue.NotOptional" -> "For an attribute to have a default value, it must be optional.",
    "RELAXNGReader.Compatibility.DefaultValue.Repeatable" -> "For an attribute to have a default value, it cannot be enclosed by <oneOrMore> nor <zeroOrMore>.",
    "RELAXNGReader.Compatibility.DefaultValue.ComplexElementName" -> "For an attribute to have a default value, the name of its parent element must be simple.",
    "RELAXNGReader.Compatibility.DefaultValue.DifferentValues" -> "The \"{3}\" attribute of the \"{2}\" element has two different default values (\"{0}\" and \"{1}\").",
    "RELAXNGReader.Compatibility.DefaultValue.ContextDependentType" -> "An attribute with a default value cannot contain the \"{0}\" datatype.",
    "RELAXNGReader.Compatibility.DefaultValue.CompetingElements" -> "There are more than one patterns that can match the \"{0}\" element, and some of them have attribute default values and some of them don't. For the schema to be compatible, there must be a functional mapping from element and attribute names to default values."
  )
