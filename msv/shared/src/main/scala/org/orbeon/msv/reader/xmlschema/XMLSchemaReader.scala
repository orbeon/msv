package org.orbeon.msv.reader.xmlschema

import java.util.{Map as JMap, Set as JSet, HashMap as JHashMap, HashSet as JHashSet}
import javax.xml.parsers.SAXParserFactory
import scala.compiletime.uninitialized
import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.xml.sax.{EntityResolver, InputSource, Locator}
import org.orbeon.msv.datatype.xsd.{DatatypeFactory, StringType, XSDatatype}
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.trex.ElementPattern
import org.orbeon.msv.grammar.xmlschema.*
import org.orbeon.msv.reader.*
import org.orbeon.msv.reader.datatype.xsd.*
import org.orbeon.msv.util.StartTagInfo

/**
 * Parses XML representation of XML Schema and constructs AGM.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class XMLSchemaReader(
  _controller: GrammarReaderController,
  parserFactory: SAXParserFactory,
  val sfactory: XMLSchemaReader.StateFactory,
  pool: ExpressionPool
) extends GrammarReader(_controller, parserFactory, pool, new RootState(sfactory.schemaHead(null)))
  with XSDatatypeResolver:

  def this(controller: GrammarReaderController, parserFactory: SAXParserFactory, pool: ExpressionPool) =
    this(controller, parserFactory, new XMLSchemaReader.StateFactory(), pool)

  def this(controller: GrammarReaderController, parserFactory: SAXParserFactory) =
    this(controller, parserFactory, new ExpressionPool())

  def this(controller: GrammarReaderController) =
    this(controller, GrammarReader.createParserFactory())

  val xsiSchemaLocationExp: ReferenceExp =
    val exp = new ReferenceExp(XMLSchemaReader.XMLSchemaSchemaLocationAttributes)
    exp.exp = pool.createSequence(
      pool.createOptional(
        pool.createAttribute(
          new SimpleNameClass(XMLSchemaSchema.XMLSchemaInstanceNamespace, "schemaLocation")
        )
      ),
      pool.createOptional(
        pool.createAttribute(
          new SimpleNameClass(XMLSchemaSchema.XMLSchemaInstanceNamespace, "noNamespaceSchemaLocation")
        )
      )
    )
    exp

  val grammar: XMLSchemaGrammar = new XMLSchemaGrammar(pool)
  val xsdSchema: XMLSchemaSchema = new XMLSchemaSchema(XMLSchemaReader.XMLSchemaNamespace, grammar)

  val complexUrType: ComplexTypeExp =
    val e = new ElementPattern(NameClass.ALL, Expression.nullSet)
    e.contentModel = pool.createMixed(
      pool.createZeroOrMore(
        pool.createChoice(
          pool.createAttribute(NameClass.ALL),
          e
        )
      )
    )
    val cur = xsdSchema.complexTypes.getOrCreate("anyType")
    cur.body.exp = e.contentModel
    cur.complexBaseType = cur
    cur.derivationMethod = ComplexTypeExp.RESTRICTION
    cur

  var attributeFormDefault: String = uninitialized
  var elementFormDefault: String = uninitialized
  var finalDefault: String = uninitialized
  var blockDefault: String = uninitialized

  var currentSchema: XMLSchemaSchema = uninitialized

  val parsedFiles: JMap[String, JSet[String]] = new JHashMap[String, JSet[String]]()
  private val definedSchemata: JSet[XMLSchemaSchema] = new JHashSet[XMLSchemaSchema]()

  def markSchemaAsDefined(schema: XMLSchemaSchema): Unit =
    definedSchemata.add(schema)

  def isSchemaDefined(schema: XMLSchemaSchema): Boolean =
    definedSchemata.contains(schema)

  def getResult: XMLSchemaGrammar =
    if controller.hadError then null
    else grammar

  def getResultAsGrammar: Grammar = getResult

  def getOrCreateSchema(namespaceURI: String): XMLSchemaSchema =
    val g = grammar.getByNamespace(namespaceURI).asInstanceOf[XMLSchemaSchema]
    if g != null then return g

    val newG = new XMLSchemaSchema(namespaceURI, grammar)
    backwardReference.memorizeLink(newG)
    newG

  def createExpressionChildState(parent: State, tag: StartTagInfo): State =
    if tag.localName == "element" then
      if tag.containsAttribute("ref") then sfactory.elementRef(parent, tag)
      else sfactory.elementDecl(parent, tag)
    else if tag.localName == "any" then
      sfactory.any(parent, tag)
    else
      createModelGroupState(parent, tag)

  def createModelGroupState(parent: State, tag: StartTagInfo): State =
    tag.localName match
      case "all"      => sfactory.all(parent, tag)
      case "choice"   => sfactory.choice(parent, tag)
      case "sequence" => sfactory.sequence(parent, tag)
      case "group"    => sfactory.group(parent, tag)
      case _          => null

  def createAttributeState(parent: State, tag: StartTagInfo): State =
    tag.localName match
      case "attribute"      => sfactory.attribute(parent, tag)
      case "anyAttribute"   => sfactory.anyAttribute(parent, tag)
      case "attributeGroup" => sfactory.attributeGroup(parent, tag)
      case _                => null

  def createFacetState(parent: State, tag: StartTagInfo): State =
    if FacetState.facetNames.contains(tag.localName) then sfactory.facets(parent, tag)
    else null

  private var issuedOldNamespaceWarning: Boolean = false

  override protected[msv] def isGrammarElement(tag: StartTagInfo): Boolean =
    if !isSchemaNamespace(tag.namespaceURI) then false
    else if tag.localName == "annotation" then false
    else true

  def resolveNamespaceOfAttributeDecl(formValue: String): String =
    resolveNamespaceOfDeclaration(formValue, attributeFormDefault)

  def resolveNamespaceOfElementDecl(formValue: String): String =
    resolveNamespaceOfDeclaration(formValue, elementFormDefault)

  private def resolveNamespaceOfDeclaration(formValue: String, defaultValue: String): String =
    if "qualified" == formValue then currentSchema.targetNamespace
    else if "unqualified" == formValue then ""
    else if formValue != null then
      reportError(GrammarReader.ERR_BAD_ATTRIBUTE_VALUE, "form", formValue)
      "$$recover$$"
    else defaultValue

  def resolveBuiltinDataType(typeLocalName: String): XSDatatype =
    try DatatypeFactory.getTypeByName(typeLocalName)
    catch case _: DatatypeException => null

  def resolveBuiltinSimpleType(typeLocalName: String): SimpleTypeExp =
    try
      val dt = DatatypeFactory.getTypeByName(typeLocalName)
      val sexp = xsdSchema.simpleTypes.getOrCreate(typeLocalName)
      if !sexp.isDefined then
        sexp.set(new XSDatatypeExp(dt, pool))
      sexp
    catch case _: DatatypeException => null

  def isSchemaNamespace(ns: String): Boolean =
    if ns == XMLSchemaReader.XMLSchemaNamespace then true
    else if ns == XMLSchemaReader.XMLSchemaNamespace_old then
      if !issuedOldNamespaceWarning then
        reportWarning(XMLSchemaReader.WRN_OBSOLETED_NAMESPACE, null)
      issuedOldNamespaceWarning = true
      true
    else false

  def resolveXSDatatype(typeQName: String): XSDatatypeExp =
    val r = splitQName(typeQName)
    if r == null then
      reportError(XMLSchemaReader.ERR_UNDECLARED_PREFIX, typeQName)
      return new XSDatatypeExp(StringType.theInstance, pool)

    if isSchemaNamespace(r(0)) then
      val dt = resolveBuiltinDataType(r(1))
      if dt != null then return new XSDatatypeExp(dt, pool)

    val sexp = getOrCreateSchema(r(0)).simpleTypes.getOrCreate(r(1))
    backwardReference.memorizeLink(sexp)

    new XSDatatypeExp(
      r(0),
      r(1),
      this,
      new XSDatatypeExp.Renderer:
        def render(context: XSDatatypeExp.RenderingContext): XSDatatype =
          if sexp.getType != null then
            sexp.getType.getType(context)
          else
            StringType.theInstance
    )

  def resolveQNameRef(tag: StartTagInfo, attName: String, resolver: XMLSchemaReader.RefResolver): Expression =
    val refQName = tag.getAttribute(attName)
    if refQName == null then
      reportError(GrammarReader.ERR_MISSING_ATTRIBUTE, tag.qName, attName)
      return null

    val r = splitQName(refQName)
    if r == null then
      reportError(XMLSchemaReader.ERR_UNDECLARED_PREFIX, refQName)
      return null

    val e = resolver.get(getOrCreateSchema(r(0)))._getOrCreate(r(1))
    backwardReference.memorizeLink(e)
    e

  var chameleonTargetNamespace: String = null

  override def splitQName(qName: String): Array[String] =
    val r = super.splitQName(qName)
    if r == null then r
    else
      if r(0).isEmpty && chameleonTargetNamespace != null then
        r(0) = chameleonTargetNamespace
      r

  override protected[msv] def interceptExpression(state: State, exp: Expression): Expression =
    state match
      case _: SequenceState | _: ChoiceState | _: InterleaveState | _: AnyElementState |
           _: ElementDeclState | _: ElementRefState | _: GroupState =>
        processOccurs(state.getStartTag, exp)
      case _ =>
        exp

  def processOccurs(startTag: StartTagInfo, item: Expression): Expression =
    val minOccurs = startTag.getAttribute("minOccurs")
    var minOccursValue = 1
    if minOccurs != null then
      try
        minOccursValue = minOccurs.toInt
        if minOccursValue < 0 then throw new NumberFormatException()
      catch
        case _: NumberFormatException =>
          reportError(GrammarReader.ERR_BAD_ATTRIBUTE_VALUE, "minOccurs", minOccurs)
          minOccursValue = 1

    val maxOccurs = startTag.getAttribute("maxOccurs")
    var maxOccursValue = 0

    if maxOccurs == null then
      if minOccursValue > 1 then
        reportError(XMLSchemaReader.ERR_MAXOCCURS_IS_NECESSARY)
      maxOccursValue = 1
    else if maxOccurs == "unbounded" then
      maxOccursValue = -1
    else
      try
        maxOccursValue = maxOccurs.toInt
        if maxOccursValue < 0 || maxOccursValue < minOccursValue then
          throw new NumberFormatException()
      catch
        case _: NumberFormatException =>
          reportError(GrammarReader.ERR_BAD_ATTRIBUTE_VALUE, "maxOccurs", maxOccurs)
          maxOccursValue = 1

    processOccurs(item, minOccursValue, maxOccursValue)

  def processOccurs(item: Expression, minOccurs: Int, maxOccurs: Int): Expression =
    val precise = _processOccurs(item, minOccurs, maxOccurs)
    if maxOccurs == 1 then return precise
    if maxOccurs == -1 && minOccurs <= 1 then return precise
    new OccurrenceExp(precise, maxOccurs, minOccurs, item)

  private def _processOccurs(item: Expression, minOccurs: Int, maxOccurs: Int): Expression =
    var exp: Expression = Expression.epsilon
    var i = 0
    while i < minOccurs do
      exp = pool.createSequence(item, exp)
      i += 1

    if maxOccurs == -1 then
      if minOccurs == 1 then pool.createOneOrMore(item)
      else pool.createSequence(exp, pool.createZeroOrMore(item))
    else
      var tmp: Expression = Expression.epsilon
      var j = minOccurs
      while j < maxOccurs do
        tmp = pool.createOptional(pool.createSequence(item, tmp))
        j += 1
      pool.createSequence(exp, tmp)

  protected[xmlschema] def switchSource(sourceState: State, newRootState: State): Unit =
    val schemaLocation = sourceState.getStartTag.getAttribute("schemaLocation")
    if schemaLocation == null then
      reportError(GrammarReader.ERR_MISSING_ATTRIBUTE, sourceState.getStartTag.qName, "schemaLocation")
    else
      switchSource(sourceState, schemaLocation, newRootState)

  var doDuplicateDefinitionCheck: Boolean = true

  protected[xmlschema] def wrapUp(): Unit =
    markSchemaAsDefined(xsdSchema)

    var grammarTopLevel: Expression = Expression.nullSet
    var itr = grammar.iterateSchemas()
    while itr.hasNext do
      val schema = itr.next().asInstanceOf[XMLSchemaSchema]

      if !isSchemaDefined(schema) then
        reportError(
          backwardReference.getReferer(schema),
          XMLSchemaReader.ERR_UNDEFINED_SCHEMA,
          Array[AnyRef](schema.targetNamespace)
        )
        return

      detectUndefinedOnes(schema.attributeDecls, XMLSchemaReader.ERR_UNDEFINED_ATTRIBUTE_DECL)
      detectUndefinedOnes(schema.attributeGroups, XMLSchemaReader.ERR_UNDEFINED_ATTRIBUTE_GROUP)
      detectUndefinedOnes(schema.complexTypes, XMLSchemaReader.ERR_UNDEFINED_COMPLEX_TYPE)
      detectUndefinedOnes(schema.elementDecls, XMLSchemaReader.ERR_UNDEFINED_ELEMENT_DECL)
      detectUndefinedOnes(schema.groupDecls, XMLSchemaReader.ERR_UNDEFINED_GROUP)
      detectUndefinedOnes(schema.simpleTypes, XMLSchemaReader.ERR_UNDEFINED_SIMPLE_TYPE)

      var exp: Expression = Expression.nullSet
      val elems = schema.elementDecls.getAll
      var i = 0
      while i < elems.length do
        exp = pool.createChoice(exp, elems(i))
        i += 1

      schema.topLevel = exp
      grammarTopLevel = pool.createChoice(grammarTopLevel, exp)

    grammar.topLevel = grammarTopLevel
    runBackPatchJob()

    val recursiveSubstBuffer = new JHashSet[ElementDeclExp]()

    itr = grammar.iterateSchemas()
    while itr.hasNext do
      val schema = itr.next().asInstanceOf[XMLSchemaSchema]
      val elems = schema.elementDecls.getAll
      var i = 0
      while i < elems.length do
        val e = elems(i).asInstanceOf[ElementDeclExp]
        recursiveSubstBuffer.clear()

        if !controller.hadError then
          var c = e.substitutionAffiliation
          var loop = true
          while c != null && loop do
            if !recursiveSubstBuffer.add(c) then
              reportError(
                Array[Locator](getDeclaredLocationOf(c), getDeclaredLocationOf(e)),
                XMLSchemaReader.ERR_RECURSIVE_SUBSTITUTION_GROUP,
                Array[AnyRef](c.name, e.name)
              )
              loop = false
            else
              if isSubstitutable(c, e) then
                if org.orbeon.msv.driver.textui.Debug.debug then
                  println(s"${c.name}<-${e.name}")
                c.substitutions.exp = pool.createChoice(c.substitutions.exp, e.body)
              else
                if org.orbeon.msv.driver.textui.Debug.debug then
                  println(s"${c.name}<-X-${e.name}")
              c = c.substitutionAffiliation
        i += 1

    if controller.hadError then return

    RunAwayExpressionChecker.check(this, grammar.topLevel)

    if !controller.hadError then
      AttributeWildcardComputer.compute(this, grammar.topLevel)

  private trait Type:
    def getDerivationMethod: Int
    def getBlockValue: Int
    def getBaseType: Type
    def getCore: AnyRef

  private def getType(exp: XMLSchemaTypeExp): Type =
    exp match
      case cexp: ComplexTypeExp =>
        new Type:
          def getDerivationMethod: Int = cexp.derivationMethod
          def getBlockValue: Int = cexp.block
          def getBaseType: Type =
            if cexp.complexBaseType != null then getType(cexp.complexBaseType)
            else if cexp.simpleBaseType != null then getType(cexp.simpleBaseType.getCreatedType())
            else getType(complexUrType)
          def getCore: AnyRef = cexp
      case sexp: SimpleTypeExp =>
        getType(sexp.getDatatype)

  private def getType(dt: XSDatatype): Type =
    if dt == null then throw new Error()
    new Type:
      def getDerivationMethod: Int = ComplexTypeExp.RESTRICTION
      def getBlockValue: Int = 0
      def getBaseType: Type =
        val base = dt.getBaseType()
        if base == null then getType(complexUrType)
        else getType(base)
      def getCore: AnyRef = dt

  private def isSubstitutable(c: ElementDeclExp, d: ElementDeclExp): Boolean =
    if c.isSubstitutionBlocked then return false

    val cType = getType(c.getTypeDefinition)
    var dType = getType(d.getTypeDefinition)

    var constraint = c.block
    var derivationMethod = 0

    while true do
      if dType.getCore eq cType.getCore then
        return (constraint & derivationMethod) == 0

      derivationMethod |= dType.getDerivationMethod
      constraint |= dType.getBlockValue

      if dType.getCore eq complexUrType then
        reportError(
          Array[Locator](getDeclaredLocationOf(c), getDeclaredLocationOf(d)),
          XMLSchemaReader.ERR_UNRELATED_TYPES_IN_SUBSTITUTIONGROUP,
          Array[AnyRef](c.name, d.name)
        )
        return false

      dType = dType.getBaseType

    false

  protected def localizeMessage(propertyName: String, args: Array[AnyRef]): String =
    val format = XMLSchemaReader.messages.getOrElse(propertyName, propertyName)
    if args == null || args.isEmpty then format
    else
      var res = format
      for i <- args.indices do
        val str = if args(i) != null then args(i).toString else "null"
        res = res.replace(s"{$i}", str)
      res.replace("''", "'")

object XMLSchemaReader:

  export GrammarReader.{
    ERR_MALPLACED_ELEMENT,
    ERR_CHARACTERS,
    ERR_DISALLOWED_ATTRIBUTE,
    ERR_MISSING_ATTRIBUTE,
    ERR_BAD_ATTRIBUTE_VALUE,
    ERR_MISSING_ATTRIBUTE_2,
    ERR_CONFLICTING_ATTRIBUTES,
    ERR_RECURSIVE_INCLUDE,
    ERR_FRAGMENT_IDENTIFIER,
    ERR_UNDEFINED_DATATYPE,
    ERR_DATATYPE_ALREADY_DEFINED,
    ERR_MISSING_CHILD_EXPRESSION,
    ERR_MORE_THAN_ONE_CHILD_EXPRESSION,
    ERR_MORE_THAN_ONE_CHILD_TYPE,
    ERR_MISSING_CHILD_TYPE,
    ERR_ILLEGAL_FINAL_VALUE,
    ERR_RUNAWAY_EXPRESSION,
    ERR_MISSING_TOPLEVEL,
    WRN_MAYBE_WRONG_NAMESPACE,
    WRN_DEPRECATED_TYPENAME,
    ERR_BAD_TYPE,
    ERR_RECURSIVE_DATATYPE
  }

  val XMLSchemaNamespace: String = "http://www.w3.org/2001/XMLSchema"
  val XMLSchemaNamespace_old: String = "http://www.w3.org/2000/10/XMLSchema"

  val XMLSchemaSchemaLocationAttributes: String =
    "____internal_XML_schema_SchemaLocation_attributes"

  def parse(grammarURL: String, factory: SAXParserFactory, controller: GrammarReaderController): XMLSchemaGrammar =
    val reader = new XMLSchemaReader(controller, factory)
    reader.parse(grammarURL)
    reader.getResult

  def parse(grammar: InputSource, factory: SAXParserFactory, controller: GrammarReaderController): XMLSchemaGrammar =
    val reader = new XMLSchemaReader(controller, factory)
    reader.parse(grammar)
    reader.getResult

  trait RefResolver:
    def get(schema: XMLSchemaSchema): ReferenceContainer

  class StateFactory:
    def schemaHead(expectedNamespace: String): State =
      new SchemaState(expectedNamespace)

    def schemaIncluded(parent: State, expectedNamespace: String): State =
      new SchemaIncludedState(expectedNamespace)

    def simpleType(parent: State, tag: StartTagInfo): State = new SimpleTypeState()
    def all(parent: State, tag: StartTagInfo): State = new InterleaveState()
    def choice(parent: State, tag: StartTagInfo): State = new ChoiceState(true)
    def sequence(parent: State, tag: StartTagInfo): State = new SequenceState(true)
    def group(parent: State, tag: StartTagInfo): State = new GroupState()
    def complexTypeDecl(parent: State, tag: StartTagInfo): State = new ComplexTypeDeclState()
    def attribute(parent: State, tag: StartTagInfo): State = new AttributeState()
    def attributeGroup(parent: State, tag: StartTagInfo): State = new AttributeGroupState()
    def elementDecl(parent: State, tag: StartTagInfo): State = new ElementDeclState()
    def elementRef(parent: State, tag: StartTagInfo): State = new ElementRefState()
    def any(parent: State, tag: StartTagInfo): State = new AnyElementState()
    def anyAttribute(parent: State, tag: StartTagInfo): State = new AnyAttributeState()
    def include(parent: State, tag: StartTagInfo): State = new IncludeState()
    def import_(parent: State, tag: StartTagInfo): State = new ImportState()
    def redefine(parent: State, tag: StartTagInfo): State = new RedefineState()
    def notation(parent: State, tag: StartTagInfo): State = new IgnoreState()
    def facets(parent: State, tag: StartTagInfo): State = new FacetState()
    def unique(parent: State, tag: StartTagInfo): State = new IdentityConstraintState()
    def key(parent: State, tag: StartTagInfo): State = new IdentityConstraintState()
    def keyref(parent: State, tag: StartTagInfo): State = new IdentityConstraintState()

    def complexContent(parent: State, tag: StartTagInfo, decl: ComplexTypeExp): State =
      new ComplexContentState(decl)

    def complexRst(parent: State, tag: StartTagInfo, decl: ComplexTypeExp): State =
      new ComplexContentBodyState(decl, false)

    def complexExt(parent: State, tag: StartTagInfo, decl: ComplexTypeExp): State =
      new ComplexContentBodyState(decl, true)

    def simpleContent(parent: State, tag: StartTagInfo, decl: ComplexTypeExp): State =
      new SimpleContentState(decl)

    def simpleRst(parent: State, tag: StartTagInfo, decl: ComplexTypeExp): State =
      new SimpleContentRestrictionState(decl)

    def simpleExt(parent: State, tag: StartTagInfo, decl: ComplexTypeExp): State =
      new SimpleContentExtensionState(decl)

  val ERR_MAXOCCURS_IS_NECESSARY: String = "XMLSchemaReader.MaxOccursIsNecessary"
  val ERR_UNIMPLEMENTED_FEATURE: String = "XMLSchemaReader.UnimplementedFeature"
  val ERR_UNDECLARED_PREFIX: String = "XMLSchemaReader.UndeclaredPrefix"
  val ERR_INCONSISTENT_TARGETNAMESPACE: String = "XMLSchemaReader.InconsistentTargetNamespace"
  val ERR_IMPORTING_SAME_NAMESPACE: String = "XMLSchemaReader.ImportingSameNamespace"
  val ERR_DUPLICATE_SCHEMA_DEFINITION: String = "XMLSchemaReader.DuplicateSchemaDefinition"
  val ERR_UNDEFINED_ELEMENTTYPE: String = "XMLSchemaReader.UndefinedElementType"
  val ERR_UNDEFINED_ATTRIBUTE_DECL: String = "XMLSchemaReader.UndefinedAttributeDecl"
  val ERR_UNDEFINED_ATTRIBUTE_GROUP: String = "XMLSchemaReader.UndefinedAttributeGroup"
  val ERR_UNDEFINED_COMPLEX_TYPE: String = "XMLSchemaReader.UndefinedComplexType"
  val ERR_UNDEFINED_SIMPLE_TYPE: String = "XMLSchemaReader.UndefinedSimpleType"
  val ERR_UNDEFINED_COMPLEX_OR_SIMPLE_TYPE: String = "XMLSchemaReader.UndefinedComplexOrSimpleType"
  val ERR_UNDEFINED_ELEMENT_DECL: String = "XMLSchemaReader.UndefinedElementDecl"
  val ERR_UNDEFINED_GROUP: String = "XMLSchemaReader.UndefinedGroup"
  val ERR_UNDEFINED_SCHEMA: String = "XMLSchemaReader.UndefinedSchema"
  val WRN_UNSUPPORTED_ANYELEMENT: String = "XMLSchemaReader.Warning.UnsupportedAnyElement"
  val WRN_OBSOLETED_NAMESPACE: String = "XMLSchemaReader.Warning.ObsoletedNamespace"
  val ERR_UNDEFINED_OR_FORWARD_REFERENCED_TYPE: String = "XMLSchemaReader.UndefinedOrForwardReferencedType"
  val ERR_REDEFINE_UNDEFINED: String = "XMLSchemaReader.RedefineUndefined"
  val ERR_DUPLICATE_ATTRIBUTE_DEFINITION: String = "XMLSchemaReader.DuplicateAttributeDefinition"
  val ERR_DUPLICATE_COMPLEXTYPE_DEFINITION: String = "XMLSchemaReader.DuplicateComplexTypeDefinition"
  val ERR_DUPLICATE_ATTRIBUTE_GROUP_DEFINITION: String = "XMLSchemaReader.DuplicateAttributeGroupDefinition"
  val ERR_DUPLICATE_GROUP_DEFINITION: String = "XMLSchemaReader.DuplicateGroupDefinition"
  val ERR_DUPLICATE_ELEMENT_DEFINITION: String = "XMLSchemaReader.DuplicateElementDefinition"
  val ERR_DUPLICATE_IDENTITY_CONSTRAINT_DEFINITION: String = "XMLSchemaReader.DuplicateIdentityConstraintDefinition"
  val ERR_BAD_XPATH: String = "XMLSchemaReader.BadXPath"
  val ERR_UNDEFINED_KEY: String = "XMLSchemaReader.UndefinedKey"
  val ERR_INVALID_BASETYPE_FOR_SIMPLECONTENT: String = "XMLSchemaReader.InvalidBasetypeForSimpleContent"
  val ERR_KEY_FIELD_NUMBER_MISMATCH: String = "XMLSchemaReader.KeyFieldNumberMismatch"
  val ERR_KEYREF_REFERRING_NON_KEY: String = "XMLSchemaReader.KeyrefReferringNonKey"
  val ERR_UNRELATED_TYPES_IN_SUBSTITUTIONGROUP: String = "XMLSchemaReader.UnrelatedTypesInSubstitutionGroup"
  val ERR_RECURSIVE_SUBSTITUTION_GROUP: String = "XMLSchemaReader.RecursiveSubstitutionGroup"
  val ERR_FIXED_AND_DEFAULT: String = "XMLSchemaReader.FixedAndDefault"
  val WRN_IMPLICIT_URTYPE_FOR_ELEMENT: String = "XMLSchemaReader.Warning.ImplicitUrTypeForElement"

  private val messages: Map[String, String] = Map(
    // GrammarReader messages
    "GrammarReader.MalplacedElement" -> "element \"{0}\" is not allowed here",
    "GrammarReader.BadType" -> "{0}",
    "GrammarReader.Characters" -> "character data not allowed: {0}",
    "GrammarReader.DisallowedAttribute" -> "\"{1}\" attribute is not allowed for \"{0}\" element.",
    "GrammarReader.MissingAttribute" -> "\"{1}\" attribute is required for \"{0}\" element, but is not specified",
    "GrammarReader.MissingAttribute.2" -> "\"{1}\" or \"{2}\" attribute is required for \"{0}\" element, but none is specified",
    "GrammarReader.BadAttributeValue" -> "\"{1}\" is invalid value for \"{0}\" attribute",
    "GrammarReader.ConflictingAttribute" -> "\"{0}\" and \"{1}\" are mutually exclusive, so cannot be specified at the same time.",
    "GrammarReader.Abstract.MoreThanOneChildType" -> "more than one child type is specified",
    "GrammarReader.Abstract.MissingChildType" -> "no child type is specified",
    "GrammarReader.IllegalFinalValue" -> "unrecognized value \"{0}\" in final attribute.",
    "GrammarReader.RecursiveInclude" -> "recursive inclusion is detected ( {0} )",
    "GrammarReader.FragmentIdentifier" -> "URL \"{0}\" contains a fragment identifier.",
    "GrammarReader.UndefinedDataType" -> "data type \"{0}\" is not defined",
    "GrammarReader.DataTypeAlreadyDefined" -> "data type \"{0}\" is already defined",
    "GrammarReader.Warning.MaybeWrongNamespace" -> "perhaps namespace URI \"{0}\" is incorrect?",
    "GrammarReader.Warning.DeprecatedTypeName" -> "Datatype \"{0}\" is obsolete name. Please use \"{1}\" instead.",
    "GrammarReader.RecursiveDatatypeDefinition" -> "Recursive datatype definition.",
    // Abstract grammar messages
    "GrammarReader.Abstract.MissingChildExpression" -> "child model group is missing",
    "GrammarReader.Abstract.MoreThanOneChildExpression" -> "more than one model group is specified whereas only one is allowed",
    "GrammarReader.Abstract.RunAwayExpression" -> "the following model groups form a infinite recursion ( {0} )",
    "GrammarReader.Abstract.MissingTopLevel" -> "this can't be used because XML Schema does not have notion of the root element",
    // XML Schema reader messages
    "XMLSchemaReader.MaxOccursIsNecessary" -> "maxOccurs attribute is required when minOccurs is more than 1",
    "XMLSchemaReader.UnimplementedFeature" -> "Unimplemented feature: \"{0}\"",
    "XMLSchemaReader.UndeclaredPrefix" -> "namespace prefix \"{0}\" is not declared",
    "XMLSchemaReader.InconsistentTargetNamespace" -> "schema''s target namespace is \"{0}\", although \"{1}\" is expected",
    "XMLSchemaReader.ImportingSameNamespace" -> "importing a schema of the same namespace",
    "XMLSchemaReader.DuplicateSchemaDefinition" -> "namespace \"{0}\" is already defined by a schema",
    "XMLSchemaReader.UndefinedElementType" -> "type \"{0}\" is not a simple type nor a complex type",
    "XMLSchemaReader.UndefinedAttributeDecl" -> "reference to the undefined attribute declaration: \"{0}\"",
    "XMLSchemaReader.UndefinedAttributeGroup" -> "reference to the undefined attribute group: \"{0}\"",
    "XMLSchemaReader.UndefinedComplexType" -> "reference to the undefined complex type: \"{0}\"",
    "XMLSchemaReader.UndefinedSimpleType" -> "reference to the undefined simple type: \"{0}\"",
    "XMLSchemaReader.UndefinedElementDecl" -> "reference to the undefined element declaration: \"{0}\"",
    "XMLSchemaReader.UndefinedGroup" -> "reference to the undefined group: \"{0}\"",
    "XMLSchemaReader.UndefinedComplexOrSimpleType" -> "reference to the undefined simple or complex type: \"{0}\"",
    "XMLSchemaReader.UndefinedKey" -> "reference to the undefined key: \"{0}\"",
    "XMLSchemaReader.UndefinedSchema" -> "namespace \"{0}\" is referenced but no schema definition of this namespace was found.",
    "XMLSchemaReader.InvalidBasetypeForSimpleContent" -> "the base type \"{0}\" cannot be used as a base type for a complex type with simple content.",
    "XMLSchemaReader.Warning.UnsupportedAnyElement" -> "\"{0}\" mode anyElement is not supported and therefore replaced by \"skip\" mode.",
    "XMLSchemaReader.Warning.ObsoletedNamespace" -> "\"http://www.w3.org/2000/10/XMLSchema\" is obsolete as XML Schema namespace. use \"http://www.w3.org/2001/XMLSchema\" instead.",
    "XMLSchemaReader.UndefinedOrForwardReferencedType" -> "\"{0}\" is undefined or forward-referenced. Forward reference of simple type is not supported.",
    "XMLSchemaReader.RedefineUndefined" -> "attempt to redefine undefined component \"{0}\".",
    "XMLSchemaReader.DuplicateAttributeDefinition" -> "attribute \"{0}\" is already defined.",
    "XMLSchemaReader.DuplicateComplexTypeDefinition" -> "complex type \"{0}\" is already defined.",
    "XMLSchemaReader.DuplicateAttributeGroupDefinition" -> "attribute group \"{0}\" is already defined.",
    "XMLSchemaReader.DuplicateGroupDefinition" -> "model group \"{0}\" is already defined.",
    "XMLSchemaReader.DuplicateElementDefinition" -> "element \"{0}\" is already defined.",
    "XMLSchemaReader.DuplicateIdentityConstraintDefinition" -> "identity constraint \"{0}\" is already defined.",
    "XMLSchemaReader.BadXPath" -> "bad XPath \"{0}\".",
    "XMLSchemaReader.KeyrefReferringNonKey" -> "\"{0}\" is not a key.",
    "XMLSchemaReader.KeyFieldNumberMismatch" -> "The specified key \"{0}\" has {1} fields but this keyref \"{2}\" has {3} fields.",
    "XMLSchemaReader.UnrelatedTypesInSubstitutionGroup" -> "The element \"{0}\" is designated as the substitution afiiliation of \"{1}\", but they have unrelated types.",
    "XMLSchemaReader.RecursiveSubstitutionGroup" -> "The element \"{0}\" and {1}\" forms cyclic substitution relationship.",
    "XMLSchemaReader.FixedAndDefault" -> "Can not have both 'fixed' and 'default' value.",
    "XMLSchemaReader.Warning.ImplicitUrTypeForElement" -> "\"anyType\" is implicitly used as the content model of this element. Is this your intention? If so, please consider to write it explicitly as type=\"anyType\"."
  )
