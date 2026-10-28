package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.*
import org.orbeon.msv.util.{StartTagInfo, StringPair}
import scala.compiletime.uninitialized

/**
 * creates "combined child content expression" and gathers "elements of concern"
 * and its "attribute-pruned" content model.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class CombinedChildContentExpCreator(protected val pool: ExpressionPool) extends ExpressionVisitorVoid:
  import CombinedChildContentExpCreator.*

  // these variables are set each time 'get' method is called
  private var tagInfo: StartTagInfo = uninitialized
  /**
   * matched elements. the buffer is usually bigger than numElements,
   * but only first numElements items are valid result.
   */
  private var result: Array[ElementExp] = new Array[ElementExp](4)
  private var numElements: Int = 0
  private var checkTagName: Boolean = false

  private var content: Expression = uninitialized
  private var continuation: Expression = uninitialized
  private var foundConcur: Boolean = false

  /**
   * computes a combined child content pattern and its continuation, with error recovery.
   */
  def get(combinedPattern: Expression, info: StartTagInfo, checkTagName: Boolean): ExpressionPair =
    if Debug.debug then
      var i = 0
      while i < result.length do
        result(i) = null
        i += 1

    numElements = 0
    continueGet(combinedPattern, info, checkTagName)

  final def continueGet(combinedPattern: Expression, info: StartTagInfo, checkTagName: Boolean): ExpressionPair =
    foundConcur = false
    this.tagInfo = info
    this.checkTagName = checkTagName
    combinedPattern.visit(this)

    // when more than one element of concern is found,
    // continuation cannot be used.
    if numElements != 1 then
      continuation = null
    new ExpressionPair(content, continuation)

  /** computes a combined child content pattern and (,if possible,) its continuation. */
  def get(combinedPattern: Expression, info: StartTagInfo): ExpressionPair =
    var sp: StringPair = null

    // check the cache
    if combinedPattern.verifierTag != null then
      val ot = combinedPattern.verifierTag.asInstanceOf[OptimizationTag]
      sp = new StringPair(info.namespaceURI, info.localName)
      val cache = ot.transitions.get(sp).asInstanceOf[OptimizationTag.OwnerAndCont]

      if cache != null then
        // cache hit
        numElements = 1
        result(0) = cache.owner
        return new ExpressionPair(cache.owner.contentModel.getExpandedExp(pool), cache.continuation)

    val r = get(combinedPattern, info, true)

    if numElements == 1 then
      // only one element matchs this tag name. cache this result
      var ot = combinedPattern.verifierTag.asInstanceOf[OptimizationTag]
      if ot == null then
        ot = new OptimizationTag()
        combinedPattern.verifierTag = ot

      if sp == null then
        sp = new StringPair(info.namespaceURI, info.localName)

      ot.transitions.put(sp, new OptimizationTag.OwnerAndCont(result(0), r.continuation))

    r

  /** obtains matched elements. */
  final def getMatchedElements(): Array[ElementExp] = result

  /** gets the number of matched elements. */
  final def numMatchedElements(): Int = numElements

  /** checks if the result of 'get' method is not the union of all elements of concern. */
  final def isComplex(): Boolean = foundConcur

  def onConcur(exp: ConcurExp): Unit =
    foundConcur = true
    exp.exp1.visit(this)
    val content1 = content
    val continuation1 = continuation

    exp.exp2.visit(this)

    content = pool.createConcur(content, content1)
    continuation = pool.createConcur(continuation, continuation1)

  def onInterleave(exp: InterleaveExp): Unit =
    exp.exp1.visit(this)
    if content == Expression.nullSet then
      exp.exp2.visit(this)
      continuation = pool.createInterleave(continuation, exp.exp1)
      return

    val content1 = content
    val continuation1 = continuation

    exp.exp2.visit(this)

    if content == Expression.nullSet then
      content = content1
      continuation = pool.createInterleave(continuation1, exp.exp2)
      return

    // now the situation is something like (A,X)^(A,Y).
    // both accepts this token. So continuation will be X^(A,Y).
    content = pool.createChoice(content, content1)
    continuation = pool.createInterleave(continuation1, exp.exp2)

  def onElement(exp: ElementExp): Unit =
    // if tag name is invalid, then remove this element from candidate.
    if checkTagName && !exp.getNameClass.accepts(tagInfo.namespaceURI, tagInfo.localName) then
      continuation = Expression.nullSet
      content = continuation
      return

    // check result and see if the same element is already registered.
    var i = 0
    while i < numElements do
      if result(i) == exp then
        content = exp.contentModel.getExpandedExp(pool)
        continuation = Expression.epsilon
        return
      i += 1

    // create a new result object
    if numElements == result.length then
      // expand the buffer if it's full.
      val buf = new Array[ElementExp](result.length * 2)
      System.arraycopy(result, 0, buf, 0, result.length)
      result = buf

    result(numElements) = exp
    numElements += 1

    content = exp.contentModel.getExpandedExp(pool)
    continuation = Expression.epsilon

  def onOneOrMore(exp: OneOrMoreExp): Unit =
    exp.exp.visit(this)
    continuation = pool.createSequence(continuation, pool.createZeroOrMore(exp.exp))

  def onMixed(exp: MixedExp): Unit =
    exp.exp.visit(this)
    continuation = pool.createMixed(continuation)

  def onAttribute(exp: AttributeExp): Unit =
    continuation = Expression.nullSet
    content = continuation

  def onEpsilon(): Unit =
    continuation = Expression.nullSet
    content = continuation

  def onNullSet(): Unit =
    continuation = Expression.nullSet
    content = continuation

  def onAnyString(): Unit =
    continuation = Expression.nullSet
    content = continuation

  def onData(exp: DataExp): Unit =
    continuation = Expression.nullSet
    content = continuation

  def onValue(exp: ValueExp): Unit =
    continuation = Expression.nullSet
    content = continuation

  def onList(exp: ListExp): Unit =
    continuation = Expression.nullSet
    content = continuation

  def onRef(exp: ReferenceExp): Unit = exp.exp.visit(this)
  def onOther(exp: OtherExp): Unit = exp.exp.visit(this)

  def onChoice(exp: ChoiceExp): Unit =
    exp.exp1.visit(this)
    val content1 = content
    val continuation1 = continuation

    exp.exp2.visit(this)

    content = pool.createChoice(content, content1)
    continuation = pool.createChoice(continuation, continuation1)

  def onSequence(exp: SequenceExp): Unit =
    exp.exp1.visit(this)
    continuation = pool.createSequence(continuation, exp.exp2)

    if !exp.exp1.isEpsilonReducible then
      return

    val content1 = content
    val continuation1 = continuation

    exp.exp2.visit(this)

    if content == Expression.nullSet then
      content = content1
      continuation = continuation1
      return

    if content1 == Expression.nullSet then
      return

    content = pool.createChoice(content, content1)
    continuation = pool.createChoice(continuation1, continuation)

object CombinedChildContentExpCreator:
  class ExpressionPair(val content: Expression, val continuation: Expression)
