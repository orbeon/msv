/*
 * @(#)$Id: XSDatatypeImpl.java,v 1.25 2005/03/31 23:19:11 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeException, DatatypeStreamingValidator, ValidationContext}
import org.orbeon.msv.relaxng.datatype.helpers.StreamingValidatorImpl

import java.util.ArrayList

/**
 * Base implementation for XSDatatype interface.
 * 
 * <p>
 * This class should be considered as the implementation-detail, and 
 * applications should not access this class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class XSDatatypeImpl(
  val namespaceUri: String,
  val typeName: String,
  val whiteSpace: WhiteSpaceProcessor
) extends XSDatatype:

  def getNamespaceUri: String = namespaceUri
  def getName: String = typeName

  final def createValue(lexicalValue: String, context: ValidationContext): AnyRef =
    _createValue(whiteSpace.process(lexicalValue), context)

  /** Converts a whitespace-processed lexical value into the corresponding value object */
  def _createValue(content: String, context: ValidationContext): AnyRef

  final def checkValid(content: String, context: ValidationContext): Unit =
    _checkValid(whiteSpace.process(content), context)

  /** Actual 'meat' of the checkValid method */
  @throws(classOf[DatatypeException])
  def _checkValid(content: String, context: ValidationContext): Unit

  final def createJavaObject(literal: String, context: ValidationContext): AnyRef =
    _createJavaObject(whiteSpace.process(literal), context)

  def _createJavaObject(literal: String, context: ValidationContext): AnyRef

  final def isValid(literal0: String, context: ValidationContext): Boolean =
    val literal = whiteSpace.process(literal0)
    if needValueCheck() then
      _createValue(literal, context) != null
    else
      checkFormat(literal, context)

  def isAlwaysValid(): Boolean = false

  def createStreamingValidator(context: ValidationContext): DatatypeStreamingValidator =
    new StreamingValidatorImpl(this, context)

  def checkFormat(literal: String, context: ValidationContext): Boolean
  def needValueCheck(): Boolean = false

  /**
   * Gets the facet object that restricts the specified facet.
   *
   * @return null if no such facet object exists.
   */
  def getFacetObject(facetName: String): DataTypeWithFacet =
    val dt = getBaseType()
    if dt != null then dt.getFacetObject(facetName)
    else null

  def getApplicableFacetNames(): Array[String] =
    val vec = new ArrayList[String]()
    val facetNames = Array(
      XSDatatype.FACET_ENUMERATION,
      XSDatatype.FACET_FRACTIONDIGITS,
      XSDatatype.FACET_TOTALDIGITS,
      XSDatatype.FACET_LENGTH,
      XSDatatype.FACET_MINLENGTH,
      XSDatatype.FACET_MAXLENGTH,
      XSDatatype.FACET_MAXEXCLUSIVE,
      XSDatatype.FACET_MINEXCLUSIVE,
      XSDatatype.FACET_MAXINCLUSIVE,
      XSDatatype.FACET_MININCLUSIVE,
      XSDatatype.FACET_PATTERN,
      XSDatatype.FACET_WHITESPACE
    )

    var i = 0
    while i < facetNames.length do
      if isFacetApplicable(facetNames(i)) == XSDatatype.APPLICABLE then
        vec.add(facetNames(i))
      i += 1

    vec.toArray(new Array[String](vec.size()))

  /** Gets the concrete type object of the restriction chain. */
  def getConcreteType(): ConcreteType

  final def sameValue(o1: AnyRef, o2: AnyRef): Boolean =
    if o1 == null || o2 == null then false
    else o1.equals(o2)

  final def valueHashCode(o: AnyRef): Int =
    if o == null then 0 else o.hashCode()

  final def isDerivedTypeOf(baseType: XSDatatype, restrictionAllowed: Boolean): Boolean =
    XSDatatypeImpl.isDerivedTypeOf(baseType, this, restrictionAllowed)

  def getAncestorBuiltinType(): XSDatatype =
    var dt: XSDatatype = this
    while XSDatatype.XMLSCHEMA_NSURI != dt.getNamespaceUri do
      dt = dt.getBaseType()
    dt

  /**
   * A property for RELAX NG DTD compatibility datatypes.
   * `ID_TYPE_NULL` is returned by default.
   */
  def getIdType(): Int = Datatype.ID_TYPE_NULL

  /**
   * A property for RELAX NG DTD compatibility datatypes.
   * Context-independent by default.
   */
  def isContextDependent(): Boolean = false

object XSDatatypeImpl:

  val serializedValueChecker: ValidationContext = new ValidationContext:
    def isNotation(s: String): Boolean = true
    def isUnparsedEntity(s: String): Boolean = true
    def resolveNamespacePrefix(ns: String): String = "abc"
    def getBaseUri: String = null

  private val messages: Map[String, String] = Map(
    "BadTypeException.InvalidItemType" ->
      "derivation from type \"{0}\" by list is prohibited.",
    "BadTypeException.InvalidMemberType" ->
      "derivation from type \"{0}\" by union is prohibited.",
    "BadTypeException.InvalidBaseType" ->
      "derivation from type \"{0}\" by restriction is prohibited.",
    "BadTypeException.FacetMustBeNonNegativeInteger" ->
      "facet \"{0}\" must be an integer greater than or equal to 0",
    "BadTypeException.FacetMustBePositiveInteger" ->
      "facet \"{0}\" must be an integer greater than zero",
    "BadTypeException.OverridingFixedFacet" ->
      "facet \"{0}\" is specified as fixed in the base type.",
    "BadTypeException.EmptyUnion" ->
      "union type is derived with no member type",
    "PatternFacet.ParseError" ->
      "specified pattern is invalid: {0}",
    "InconsistentFacets.1" ->
      "values of the {0} facet and the {1} facet are inconsistent",
    "InconsistentFacets.2" ->
      "values of the {0} facet is inconsistent with the {2} facet of \"{1}\" type, which is used as the base type.",
    "XAndYAreExclusive" ->
      "The facets {0} and {1} are mutually exclusive",
    "LoosenedFacet" ->
      "the value of the {0} facet is looser than the same facet on the \"{1}\" type.",
    "EnumerationFacet.InvalidValueForThisType" ->
      "the specified value \"{0}\" is not a/an {1}",
    "WhiteSpaceProcessor.InvalidWhiteSpaceValue" ->
      "\"{0}\" is not a valid whiteSpace facet value: it must be preserve, replace, or collapse",
    "PrecisionScaleFacet.ScaleIsGraterThanPrecision" ->
      "totalDigits must be less than or equal to fractionDigits",
    "BadTypeException.DuplicateFacet" ->
      "facet {0} is specified more than once",
    "BadTypeException.NotApplicableFacet" ->
      "facet {0} is not applicable to this datatype",
    "DataTypeErrorDiagnosis.InappropriateForType" ->
      "\"{0}\" does not satisfy the \"{1}\" type",
    "DataTypeErrorDiagnosis.TooMuchPrecision" ->
      "the value has {0} digits, where precision must be within {1}.",
    "DataTypeErrorDiagnosis.TooMuchScale" ->
      "the value has {0} fractional digits, where scale must be within {1}.",
    "DataTypeErrorDiagnosis.Enumeration" ->
      "the value is not a member of the enumeration.",
    "DataTypeErrorDiagnosis.Enumeration.Arg" ->
      "the value is not a member of the enumeration: {0}",
    "DataTypeErrorDiagnosis.OutOfRange" ->
      "the value is out of the range ({0} specifies {1}).",
    "DataTypeErrorDiagnosis.Length" ->
      "the length of the value is {0}, but the required length is {1}.",
    "DataTypeErrorDiagnosis.MinLength" ->
      "the length of the value is {0}, but the required minimum is {1}.",
    "DataTypeErrorDiagnosis.MaxLength" ->
      "the length of the value is {0}, but the required maximum is {1}.",
    "DataTypeErrorDiagnosis.Pattern.1" ->
      "the value does not match the regular expression \"{0}\".",
    "DataTypeErrorDiagnosis.Pattern.Many" ->
      "the value does not match any of the regular expressions."
  )

  def format(pattern: String, args: Array[AnyRef]): String =
    if args == null || args.isEmpty then pattern
    else
      var res = pattern
      var i = 0
      while i < args.length do
        val s = if args(i) != null then args(i).toString else "null"
        res = res.replace("{" + i + "}", s)
        i += 1
      res

  def localize(prop: String, args: Array[AnyRef]): String =
    val template = messages.getOrElse(prop, prop)
    format(template, args)

  def localize(prop: String): String = localize(prop, null.asInstanceOf[Array[AnyRef]])
  def localize(prop: String, arg1: AnyRef): String = localize(prop, Array(arg1))
  def localize(prop: String, arg1: AnyRef, arg2: AnyRef): String = localize(prop, Array(arg1, arg2))
  def localize(prop: String, arg1: AnyRef, arg2: AnyRef, arg3: AnyRef): String = localize(prop, Array(arg1, arg2, arg3))

  /**
   * An implementation of "Type Derivation OK (Simple)" of the spec.
   */
  def isDerivedTypeOf(base: XSDatatype, derived0: XSDatatype, restrictionAllowed: Boolean): Boolean =
    if base == derived0 then return true
    if !restrictionAllowed then return false
    if base == SimpleURType.theInstance then return true

    if base.getVariety() == XSDatatype.VARIETY_UNION then
      var t: XSDatatype = base
      while !t.isInstanceOf[UnionType] do
        t = t.getBaseType()
      val memberTypes = t.asInstanceOf[UnionType].memberTypes
      var i = 0
      while i < memberTypes.length do
        if isDerivedTypeOf(memberTypes(i), derived0, restrictionAllowed) then
          return true
        i += 1

    var derived = derived0
    while derived != null && derived != SimpleURType.theInstance do
      if base == derived then
        return true
      derived = derived.getBaseType()

    false

  val ERR_INAPPROPRIATE_FOR_TYPE: String = "DataTypeErrorDiagnosis.InappropriateForType"
  val ERR_TOO_MUCH_PRECISION: String     = "DataTypeErrorDiagnosis.TooMuchPrecision"
  val ERR_TOO_MUCH_SCALE: String         = "DataTypeErrorDiagnosis.TooMuchScale"
  val ERR_ENUMERATION: String            = "DataTypeErrorDiagnosis.Enumeration"
  val ERR_ENUMERATION_WITH_ARG: String   = "DataTypeErrorDiagnosis.Enumeration.Arg"
  val ERR_OUT_OF_RANGE: String           = "DataTypeErrorDiagnosis.OutOfRange"
  val ERR_LENGTH: String                 = "DataTypeErrorDiagnosis.Length"
  val ERR_MINLENGTH: String              = "DataTypeErrorDiagnosis.MinLength"
  val ERR_MAXLENGTH: String              = "DataTypeErrorDiagnosis.MaxLength"
  val ERR_PATTERN_1: String              = "DataTypeErrorDiagnosis.Pattern.1"
  val ERR_PATTERN_MANY: String           = "DataTypeErrorDiagnosis.Pattern.Many"

  val ERR_INVALID_ITEMTYPE: String           = "BadTypeException.InvalidItemType"
  val ERR_INVALID_MEMBER_TYPE: String        = "BadTypeException.InvalidMemberType"
  val ERR_INVALID_BASE_TYPE: String          = "BadTypeException.InvalidBaseType"
  val ERR_INVALID_WHITESPACE_VALUE: String   = "WhiteSpaceProcessor.InvalidWhiteSpaceValue"
  val ERR_PARSE_ERROR: String                = "PatternFacet.ParseError"

  val ERR_INVALID_VALUE_FOR_THIS_TYPE: String        = "EnumerationFacet.InvalidValueForThisType"
  val ERR_FACET_MUST_BE_NON_NEGATIVE_INTEGER: String = "BadTypeException.FacetMustBeNonNegativeInteger"
  val ERR_FACET_MUST_BE_POSITIVE_INTEGER: String     = "BadTypeException.FacetMustBePositiveInteger"
  val ERR_OVERRIDING_FIXED_FACET: String             = "BadTypeException.OverridingFixedFacet"
  val ERR_INCONSISTENT_FACETS_1: String              = "InconsistentFacets.1"
  val ERR_INCONSISTENT_FACETS_2: String              = "InconsistentFacets.2"
  val ERR_X_AND_Y_ARE_EXCLUSIVE: String              = "XAndYAreExclusive"
  val ERR_LOOSENED_FACET: String                     = "LoosenedFacet"
  val ERR_SCALE_IS_GREATER_THAN_PRECISION: String    = "PrecisionScaleFacet.ScaleIsGraterThanPrecision"
  val ERR_DUPLICATE_FACET: String                    = "BadTypeException.DuplicateFacet"
  val ERR_NOT_APPLICABLE_FACET: String               = "BadTypeException.NotApplicableFacet"
  val ERR_EMPTY_UNION: String                        = "BadTypeException.EmptyUnion"
