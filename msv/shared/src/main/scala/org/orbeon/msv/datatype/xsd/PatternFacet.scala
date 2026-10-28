/*
 * @(#)$Id: PatternFacet.java,v 1.30 2005/04/01 00:00:28 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.xsd.regex.{RegExp, RegExpFactory}
import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

import java.io.{IOException, ObjectInputStream}
import org.orbeon.msv.datatype.regexp.ParseException

/**
 * "pattern" facet validator.
 * 
 * "pattern" is a constraint facet which is applied against lexical space.
 * See http://www.w3.org/TR/xmlschema-2/#dt-pattern for the spec
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class PatternFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  facets: TypeIncubator
) extends DataTypeWithLexicalConstraintFacet(
  nsUri,
  typeName,
  baseType,
  XSDatatype.FACET_PATTERN,
  facets.isFixed(XSDatatype.FACET_PATTERN)
):

  val patterns: Array[String] =
    val regExps = facets.getVector(XSDatatype.FACET_PATTERN)
    val arr = new Array[String](regExps.size())
    var i = 0
    while i < regExps.size() do
      arr(i) = regExps.get(i).asInstanceOf[String]
      i += 1
    arr

  @transient private var exps: Array[RegExp] = scala.compiletime.uninitialized

  def getRegExps(): Array[RegExp] = exps

  try
    compileRegExps()
  catch
    case pe: ParseException =>
      throw new DatatypeException(
        XSDatatypeImpl.localize(XSDatatypeImpl.ERR_PARSE_ERROR, pe.getMessage)
      )

  @throws(classOf[ParseException])
  private def compileRegExps(): Unit =
    exps = new Array[RegExp](patterns.length)
    val factory = RegExpFactory.createFactory()
    var i = 0
    while i < exps.length do
      exps(i) = factory.compile(patterns(i))
      i += 1

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit =
    if checkLexicalConstraint(content) then return

    if exps.length == 1 then
      throw new DatatypeException(
        DatatypeException.UNKNOWN,
        XSDatatypeImpl.localize(XSDatatypeImpl.ERR_PATTERN_1, patterns(0))
      )
    else
      throw new DatatypeException(
        DatatypeException.UNKNOWN,
        XSDatatypeImpl.localize(XSDatatypeImpl.ERR_PATTERN_MANY)
      )

  protected final def checkLexicalConstraint(literal: String): Boolean =
    synchronized {
      var i = 0
      while i < exps.length do
        if exps(i).matches(literal) then return true
        i += 1
    }
    false

  @throws(classOf[IOException])
  @throws(classOf[ClassNotFoundException])
  private def readObject(stream: ObjectInputStream): Unit =
    stream.defaultReadObject()
    try
      compileRegExps()
    catch
      case e: ParseException =>
        throw new IOException(e.getMessage)
