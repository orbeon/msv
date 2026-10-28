package org.orbeon.msv.verifier.regexp

import java.util.ArrayList
import org.orbeon.msv.datatype.xsd.{StringType, WhiteSpaceProcessor}
import org.orbeon.msv.grammar.{DataExp, Expression, IDContextProvider2, ListExp, ValueExp}
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.DatatypeRef

/**
 * chunk of string.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class StringToken(
  val resCalc: ResidualCalculator,
  val literal: String,
  val context: IDContextProvider2,
  var refType: DatatypeRef
) extends Token {

  val ignorable: Boolean = literal.trim.length == 0
  protected var saturated: Boolean = false

  def this(resCalc: ResidualCalculator, literal: String, context: IDContextProvider2) =
    this(resCalc, literal, context, null)

  def this(docDecl: REDocumentDeclaration, literal: String, context: IDContextProvider2) =
    this(docDecl.resCalc, literal, context, null)

  def this(docDecl: REDocumentDeclaration, literal: String, context: IDContextProvider2, refType: DatatypeRef) =
    this(docDecl.resCalc, literal, context, refType)

  if (ignorable && refType != null)
    refType.types = StringToken.ignoredType

  private def tokenize(content: String): Array[String] = {
    val list = new ArrayList[String]()
    val len = content.length
    var i = 0
    while (i < len) {
      while (i < len && WhiteSpaceProcessor.isWhiteSpace(content.charAt(i))) {
        i += 1
      }
      val start = i
      while (i < len && !WhiteSpaceProcessor.isWhiteSpace(content.charAt(i))) {
        i += 1
      }
      if (start < i) {
        list.add(content.substring(start, i))
      }
    }
    list.toArray(new Array[String](list.size()))
  }

  override def `match`(exp: DataExp): Boolean = {
    if (!exp.dt.isValid(literal, context))
      return false

    if (exp.except ne Expression.nullSet) {
      if (resCalc.calcResidual(exp.except, this).isEpsilonReducible)
        return false
    }

    if (refType != null)
      assignType(exp.dt)

    if (exp.dt.getIdType() != Datatype.ID_TYPE_NULL && context != null)
      context.onID(exp.dt, this)

    true
  }

  override def `match`(exp: ValueExp): Boolean = {
    val thisValue = exp.dt.createValue(literal, context)
    if (!exp.dt.sameValue(thisValue, exp.value))
      return false

    if (refType != null)
      assignType(exp.dt)

    if (exp.dt.getIdType() != Datatype.ID_TYPE_NULL && context != null)
      context.onID(exp.dt, this)

    true
  }

  override def `match`(exp: ListExp): Boolean = {
    val tokens = tokenize(literal)
    var residual = exp.exp

    var dtRef: DatatypeRef = null
    var childTypes: Array[Datatype] = null
    var cnt = 0

    if (this.refType != null) {
      dtRef = new DatatypeRef()
      childTypes = new Array[Datatype](tokens.length)
    }

    var i = 0
    while (i < tokens.length) {
      val child = createChildStringToken(tokens(i), dtRef)
      residual = resCalc.calcResidual(residual, child)

      if (residual eq Expression.nullSet)
        return false

      if (dtRef != null) {
        if (dtRef.types == null) {
          saturated = true
          refType.types = null
          dtRef = null
        } else {
          if (dtRef.types.length != 1)
            throw new Error()

          childTypes(cnt) = dtRef.types(0)
          cnt += 1
        }
      }
      i += 1
    }

    if (!residual.isEpsilonReducible)
      return false

    if (childTypes != null) {
      if (saturated)
        refType.types = null
      else
        refType.types = childTypes
      saturated = true
    }

    true
  }

  protected def createChildStringToken(literal: String, dtRef: DatatypeRef): StringToken =
    new StringToken(resCalc, literal, context, dtRef)

  override def matchAnyString(): Boolean = {
    if (refType != null)
      assignType(StringType.theInstance)
    true
  }

  private def assignType(dt: Datatype): Unit = {
    if (saturated) {
      if (refType.types != null && (refType.types(0) != dt || refType.types.length != 1))
        refType.types = null
    } else {
      refType.types = Array(dt)
      saturated = true
    }
  }

  override def isIgnorable: Boolean = ignorable
}

object StringToken {
  private val ignoredType: Array[Datatype] = new Array[Datatype](0)
}
