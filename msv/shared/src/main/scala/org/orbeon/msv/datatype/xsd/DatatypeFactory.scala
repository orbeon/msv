/*
 * @(#)$Id: DatatypeFactory.java,v 1.43 2005/03/31 23:19:09 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.DatatypeException

import java.util.{Collection => JCollection, HashMap, Map => JMap}

/**
 * Datatype object factory.
 *
 * <p>
 * Applications should use this class to get and derive DataType objects.
 * All methods are static.
 * 
 * <p>
 * Derivation by restriction should be done by using {@link TypeIncubator}.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
object DatatypeFactory:

  @throws(classOf[DatatypeException])
  def deriveByList(nsUri: String, newTypeName: String, itemType: XSDatatype): XSDatatype =
    if itemType.isInstanceOf[ErrorType] then itemType
    else new ListType(nsUri, newTypeName, itemType.asInstanceOf[XSDatatypeImpl])

  @deprecated("", "")
  @throws(classOf[DatatypeException])
  def deriveByList(newTypeName: String, itemType: XSDatatype): XSDatatype =
    deriveByList("", newTypeName, itemType)

  @throws(classOf[DatatypeException])
  def deriveByUnion(nsUri: String, newTypeName: String, memberTypes: Array[XSDatatype]): XSDatatype =
    var i = 0
    while i < memberTypes.length do
      if memberTypes(i).isInstanceOf[ErrorType] then return memberTypes(i)
      i += 1

    new UnionType(nsUri, newTypeName, memberTypes)

  @deprecated("", "")
  @throws(classOf[DatatypeException])
  def deriveByUnion(newTypeName: String, memberTypes: Array[XSDatatype]): XSDatatype =
    deriveByUnion("", newTypeName, memberTypes)

  @deprecated("", "")
  @throws(classOf[DatatypeException])
  def deriveByUnion(newTypeName: String, memberTypes: JCollection[?]): XSDatatype =
    deriveByUnion("", newTypeName, memberTypes)

  @throws(classOf[DatatypeException])
  def deriveByUnion(nsUri: String, newTypeName: String, memberTypes: JCollection[?]): XSDatatype =
    val m = new Array[XSDatatype](memberTypes.size())
    val itr = memberTypes.iterator()
    var i = 0
    while itr.hasNext do
      m(i) = itr.next().asInstanceOf[XSDatatype]
      i += 1
    deriveByUnion(nsUri, newTypeName, m)

  private def add(m: JMap[String, XSDatatypeImpl], dt: XSDatatypeImpl): Unit =
    val name = dt.getName
    if name == null then
      throw new IllegalArgumentException("anonymous type")
    if m.containsKey(name) then
      throw new IllegalArgumentException("multiple definition: " + name)
    m.put(name, dt)

  @throws(classOf[DatatypeException])
  def getTypeByName(dataTypeName: String): XSDatatype = synchronized {
    var dt = builtinType.get(dataTypeName)
    if dt != null then return dt

    try
      dataTypeName match
        case "float"              => add(builtinType, FloatType.theInstance)
        case "double"             => add(builtinType, DoubleType.theInstance)
        case "duration"           => add(builtinType, DurationType.theInstance)
        case "dateTime"           => add(builtinType, DateTimeType.theInstance)
        case "time"               => add(builtinType, TimeType.theInstance)
        case "date"               => add(builtinType, DateType.theInstance)
        case "gYearMonth"         => add(builtinType, GYearMonthType.theInstance)
        case "gYear"              => add(builtinType, GYearType.theInstance)
        case "gMonthDay"          => add(builtinType, GMonthDayType.theInstance)
        case "gDay"               => add(builtinType, GDayType.theInstance)
        case "gMonth"             => add(builtinType, GMonthType.theInstance)
        case "hexBinary"          => add(builtinType, HexBinaryType.theInstance)
        case "base64Binary"       => add(builtinType, Base64BinaryType.theInstance)
        case "anyURI"             => add(builtinType, AnyURIType.theInstance)
        case "ENTITY"             => add(builtinType, EntityType.theInstance)
        case "language"           => add(builtinType, LanguageType.theInstance)
        case "ID"                 => add(builtinType, IDType.theInstance)
        case "IDREF"              => add(builtinType, IDREFType.theInstance)
        case "IDREFS"             => add(builtinType, createBuiltinList("IDREFS", IDREFType.theInstance))
        case "ENTITIES"           => add(builtinType, createBuiltinList("ENTITIES", EntityType.theInstance))
        case "NMTOKENS"           => add(builtinType, createBuiltinList("NMTOKENS", NmtokenType.theInstance))
        case "NOTATION"           => add(builtinType, new StringType("NOTATION", WhiteSpaceProcessor.theCollapse, false))
        case "nonPositiveInteger" => add(builtinType, NonPositiveIntegerType.theInstance)
        case "unsignedLong"       => add(builtinType, UnsignedLongType.theInstance)
        case "unsignedInt"        => add(builtinType, UnsignedIntType.theInstance)
        case "unsignedShort"      => add(builtinType, UnsignedShortType.theInstance)
        case "unsignedByte"       => add(builtinType, UnsignedByteType.theInstance)
        case "anySimpleType"      => add(builtinType, SimpleURType.theInstance)
        case _                    => ()
    catch
      case _: DatatypeException =>
        throw new Error()

    dt = builtinType.get(dataTypeName)
    if dt != null then return dt

    throw new DatatypeException("undefined type name: " + dataTypeName)
  }

  private def createBuiltinList(name: String, item: XSDatatypeImpl): XSDatatypeImpl =
    val ti = new TypeIncubator(new ListType(null, null, item))
    ti.addFacet("minLength", "1", false, null)
    new Proxy(XSDatatype.XMLSCHEMA_NSURI, name, ti.derive(null, null)):
      protected def readResolve(): AnyRef =
        try
          DatatypeFactory.getTypeByName(getName)
        catch
          case e: DatatypeException =>
            throw new InternalError(e.getMessage)

  private val builtinType: JMap[String, XSDatatypeImpl] = createInitialBuiltinTypesMap()

  private def createInitialBuiltinTypesMap(): JMap[String, XSDatatypeImpl] =
    val m = new HashMap[String, XSDatatypeImpl]()
    add(m, StringType.theInstance)
    add(m, BooleanType.theInstance)
    add(m, NumberType.theInstance)
    add(m, QnameType.theInstance)
    add(m, NormalizedStringType.theInstance)
    add(m, TokenType.theInstance)
    add(m, NmtokenType.theInstance)
    add(m, NameType.theInstance)
    add(m, NcnameType.theInstance)
    add(m, IntegerType.theInstance)
    add(m, NegativeIntegerType.theInstance)
    add(m, LongType.theInstance)
    add(m, IntType.theInstance)
    add(m, ShortType.theInstance)
    add(m, ByteType.theInstance)
    add(m, NonNegativeIntegerType.theInstance)
    add(m, PositiveIntegerType.theInstance)
    m
