package org.orbeon.msv.verifier.regexp

import java.util.{Collections, Comparator, HashSet as JHashSet, List as JList, Set as JSet, Vector as JVector}
import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.util.{ExpressionPrinter, IDContextProviderWrapper}
import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.orbeon.msv.util.{DatatypeRef, StartTagInfo, StringRef}
import org.orbeon.msv.verifier.Acceptor

/**
 * {@link Acceptor} implementation.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ExpressionAcceptor(
  protected val docDecl: REDocumentDeclaration,
  private var expression: Expression,
  protected val ignoreUndeclaredAttributes: Boolean
) extends Acceptor:

  def getExpression: Expression = expression

  def createChildAcceptor(tag: StartTagInfo, errRef: StringRef): Acceptor =
    val cccc = docDecl.cccec

    val e = cccc.get(expression, tag)
    if e.content != Expression.nullSet then
      if Debug.debug then
        println(s"accept start tag <${tag.qName}>. combined content pattern is")
        println(ExpressionPrinter.printContentModel(e.content))
        if e.continuation != null then
          println("continuation is:\n" + ExpressionPrinter.printContentModel(e.continuation))
        else
          println("no continuation")

      return createAcceptor(e.content, e.continuation, cccc.getMatchedElements(), cccc.numMatchedElements())

    if errRef == null then
      return null

    errRef.str = diagnoseBadTagName(tag)
    if errRef.str == null then
      errRef.str = docDecl.localizeMessage(REDocumentDeclaration.DIAG_BAD_TAGNAME_GENERIC, tag.qName)

    createRecoveryAcceptors()

  protected def createAcceptor(
    contentModel: Expression,
    continuation: Expression,
    primitives: Array[ElementExp],
    numPrimitives: Int
  ): Acceptor

  @deprecated
  final def onAttribute(
    namespaceURI: String,
    localName: String,
    qName: String,
    value: String,
    context: IDContextProvider,
    refErr: StringRef,
    refType: DatatypeRef
  ): Boolean =
    onAttribute2(namespaceURI, localName, qName, value, IDContextProviderWrapper.create(context), refErr, refType)

  final def onAttribute2(
    namespaceURI: String,
    localName: String,
    qName: String,
    value: String,
    context: IDContextProvider2,
    refErr: StringRef,
    refType: DatatypeRef
  ): Boolean =
    docDecl.attToken.reinit(namespaceURI, localName, qName, new StringToken(docDecl, value, context, refType))
    onAttribute(docDecl.attToken, refErr)

  protected def onAttribute(token: AttributeToken, refErr: StringRef): Boolean =
    val r = docDecl.attFeeder.feed(this.expression, token, ignoreUndeclaredAttributes)

    if r != Expression.nullSet then
      expression = r
      if Debug.debug then
        println("-- residual after :" + ExpressionPrinter.printContentModel(r))
      return true

    if refErr == null then
      return false

    val rtoken = token.createRecoveryAttToken()
    val rRec = docDecl.attFeeder.feed(this.expression, rtoken, ignoreUndeclaredAttributes)

    if rRec == Expression.nullSet then
      if this.expression == Expression.nullSet then
        refErr.str = docDecl.localizeMessage(REDocumentDeclaration.DIAG_CONTENT_MODEL_IS_NULLSET, null.asInstanceOf[Array[AnyRef]])
      else
        refErr.str = docDecl.localizeMessage(REDocumentDeclaration.DIAG_UNDECLARED_ATTRIBUTE, token.qName)
      true
    else
      refErr.str = diagnoseBadAttributeValue(rtoken)
      if refErr.str == null then
        refErr.str = docDecl.localizeMessage(REDocumentDeclaration.DIAG_BAD_ATTRIBUTE_VALUE_GENERIC, token.qName)
      this.expression = rRec
      true

  def onEndAttributes(sti: StartTagInfo, refErr: StringRef): Boolean =
    val r = docDecl.attPruner.prune(this.expression)
    if r != Expression.nullSet then
      this.expression = r
      return true

    if refErr == null then
      return false

    if this.expression == Expression.nullSet then
      refErr.str = docDecl.localizeMessage(REDocumentDeclaration.DIAG_CONTENT_MODEL_IS_NULLSET, null.asInstanceOf[Array[AnyRef]])
    else
      refErr.str = diagnoseMissingAttribute(sti)
      if refErr.str == null then
        refErr.str = docDecl.localizeMessage(REDocumentDeclaration.DIAG_MISSING_ATTRIBUTE_GENERIC, sti.qName)

    this.expression = this.expression.visit(docDecl.attRemover)
    true

  protected def stepForward(token: Token, errRef: StringRef): Boolean =
    val residual = docDecl.resCalc.calcResidual(expression, token)

    if Debug.debug then
      println(s"residual of stepForward($token)")
      print(ExpressionPrinter.printContentModel(expression))
      print("   ->   ")
      println(ExpressionPrinter.printContentModel(residual))

    if residual == Expression.nullSet then
      if errRef != null then
        token match
          case st: StringToken =>
            errRef.str = diagnoseUnexpectedLiteral(st)
          case _ =>
      return false

    expression = residual
    true

  @deprecated
  final def onText(literal: String, context: IDContextProvider, refErr: StringRef, refType: DatatypeRef): Boolean =
    onText2(literal, IDContextProviderWrapper.create(context), refErr, refType)

  def onText2(literal: String, provider: IDContextProvider2, refErr: StringRef, refType: DatatypeRef): Boolean =
    stepForward(new StringToken(docDecl, literal, provider, refType), refErr)

  final def stepForwardByContinuation(continuation: Expression, errRef: StringRef): Boolean =
    if continuation != Expression.nullSet then
      if Debug.debug then
        println("stepForwardByCont. :  " + ExpressionPrinter.printContentModel(continuation))
      expression = continuation
      return true

    if errRef == null then false
    else false

  def isAcceptState(errRef: StringRef): Boolean =
    if errRef == null then
      expression.isEpsilonReducible
    else
      if expression.isEpsilonReducible then true
      else
        errRef.str = diagnoseUncompletedContent()
        false

  def getStringCareLevel: Int =
    var ot = expression.verifierTag.asInstanceOf[OptimizationTag]
    if ot == null then
      ot = new OptimizationTag()
      expression.verifierTag = ot

    if ot.stringCareLevel == OptimizationTag.STRING_NOTCOMPUTED then
      ot.stringCareLevel = StringCareLevelCalculator.calc(expression)

    ot.stringCareLevel

  private def createRecoveryAcceptors(): Acceptor =
    val cccc = docDecl.cccec
    val combinedEoC = cccc.get(expression, null, false)
    val eocr = docDecl.resCalc.calcResidual(expression, AnyElementToken.theInstance)
    val continuation = docDecl.pool.createChoice(expression, eocr)
    val contentModel = combinedEoC.content

    if Debug.debug then
      println("content model of recovery acceptor:" + ExpressionPrinter.printContentModel(contentModel))
      println("continuation of recovery acceptor:" + ExpressionPrinter.printSmallest(continuation))

    createAcceptor(contentModel, continuation, null, 0)

  private def concatenateMessages(items: JList[String], more: Boolean, separatorStr: String, moreStr: String): String =
    var r = ""
    val sep = docDecl.localizeMessage(separatorStr, null.asInstanceOf[Array[AnyRef]])
    Collections.sort(items, new Comparator[String] {
      def compare(o1: String, o2: String): Int = o1.compareTo(o2)
    })
    var i = 0
    while i < items.size() do
      if r.length != 0 then r += sep
      r += items.get(i)
      i += 1
    if more then
      r += docDecl.localizeMessage(moreStr, null.asInstanceOf[Array[AnyRef]])
    r

  private def concatenateMessages(items: JSet[String], more: Boolean, separatorStr: String, moreStr: String): String =
    concatenateMessages(new JVector[String](items), more, separatorStr, moreStr)

  private def getDiagnosisFromTypedString(exp: DataOrValueExp, value: StringToken): String =
    try
      exp.getType.checkValid(value.literal, value.context)
      null
    catch
      case e: DatatypeException => e.getMessage

  private def diagnoseBadTagName(sti: StartTagInfo): String =
    val cccc = docDecl.cccec
    val r = cccc.get(expression, sti, false).content

    if r == Expression.nullSet then
      return docDecl.localizeMessage(REDocumentDeclaration.DIAG_ELEMENT_NOT_ALLOWED, sti.qName)

    if cccc.isComplex() then
      return null

    val s = new JHashSet[String]()
    var more = false
    var wrongNamespace: String = null

    val eocs = cccc.getMatchedElements()
    val len = cccc.numMatchedElements()
    var i = 0
    while i < len do
      if eocs(i).contentModel.getExpandedExp(docDecl.pool) != Expression.nullSet then
        val nc = eocs(i).getNameClass
        nc match
          case snc: SimpleNameClass =>
            if snc.localName == sti.localName then
              wrongNamespace = snc.namespaceURI
            s.add(docDecl.localizeMessage(REDocumentDeclaration.DIAG_SIMPLE_NAMECLASS, nc.toString))
          case nsnc: NamespaceNameClass =>
            s.add(docDecl.localizeMessage(REDocumentDeclaration.DIAG_NAMESPACE_NAMECLASS, nsnc.namespaceURI))
          case notnc: NotNameClass =>
            notnc.child match
              case nsnc: NamespaceNameClass =>
                s.add(docDecl.localizeMessage(REDocumentDeclaration.DIAG_NOT_NAMESPACE_NAMECLASS, nsnc.namespaceURI))
              case _ =>
                more = true
          case _ =>
            more = true
      i += 1

    if s.size() == 0 then return null

    if wrongNamespace != null then
      if s.size() == 1 then
        docDecl.localizeMessage(REDocumentDeclaration.DIAG_BAD_TAGNAME_WRONG_NAMESPACE, sti.localName, wrongNamespace)
      else
        docDecl.localizeMessage(REDocumentDeclaration.DIAG_BAD_TAGNAME_PROBABLY_WRONG_NAMESPACE, sti.localName, wrongNamespace)
    else
      docDecl.localizeMessage(
        REDocumentDeclaration.DIAG_BAD_TAGNAME_WRAPUP,
        sti.qName,
        concatenateMessages(s, more, REDocumentDeclaration.DIAG_BAD_TAGNAME_SEPARATOR, REDocumentDeclaration.DIAG_BAD_TAGNAME_MORE)
      )

  private def diagnoseBadAttributeValue(rtoken: AttributeRecoveryToken): String =
    val constraint = rtoken.getFailedExp
    constraint match
      case tse: DataOrValueExp =>
        if tse.getType == org.orbeon.msv.grammar.relax.NoneType.theInstance then
          return docDecl.localizeMessage(REDocumentDeclaration.DIAG_UNDECLARED_ATTRIBUTE, rtoken.qName)
        val dtMsg = getDiagnosisFromTypedString(tse, rtoken.value)
        if dtMsg == null then null
        else docDecl.localizeMessage(REDocumentDeclaration.DIAG_BAD_ATTRIBUTE_VALUE_DATATYPE, rtoken.qName, dtMsg)

      case ch: ChoiceExp =>
        val items = new JHashSet[String]()
        var more = false
        val children = ch.getChildren
        var i = 0
        while i < children.length do
          children(i) match
            case vexp: ValueExp =>
              items.add(vexp.value.toString)
            case _ =>
              more = true
          i += 1

        if items.size() == 0 then null
        else
          docDecl.localizeMessage(
            REDocumentDeclaration.DIAG_BAD_ATTRIBUTE_VALUE_WRAPUP,
            rtoken.qName,
            concatenateMessages(items, more, REDocumentDeclaration.DIAG_BAD_ATTRIBUTE_VALUE_SEPARATOR, REDocumentDeclaration.DIAG_BAD_ATTRIBUTE_VALUE_MORE)
          )

      case _ => null

  private def diagnoseMissingAttribute(sti: StartTagInfo): String =
    var e = expression.visit(docDecl.attPicker)
    if e.isEpsilonReducible then throw new Error()

    val s = new JHashSet[String]()
    var more = false

    while e.isInstanceOf[ChoiceExp] do
      val ch = e.asInstanceOf[ChoiceExp]
      val nc = ch.exp2.asInstanceOf[AttributeExp].nameClass
      nc match
        case snc: SimpleNameClass => s.add(nc.toString)
        case _                    => more = true
      e = ch.exp1

    if e == Expression.nullSet then return null
    if !e.isInstanceOf[AttributeExp] then throw new Error(e.toString)

    val nc = e.asInstanceOf[AttributeExp].nameClass
    nc match
      case snc: SimpleNameClass => s.add(nc.toString)
      case _                    => more = true

    if s.size() == 0 then return null

    if s.size() == 1 && !more then
      docDecl.localizeMessage(REDocumentDeclaration.DIAG_MISSING_ATTRIBUTE_SIMPLE, sti.qName, s.iterator().next())
    else
      docDecl.localizeMessage(
        REDocumentDeclaration.DIAG_MISSING_ATTRIBUTE_WRAPUP,
        sti.qName,
        concatenateMessages(s, more, REDocumentDeclaration.DIAG_MISSING_ATTRIBUTE_SEPARATOR, REDocumentDeclaration.DIAG_MISSING_ATTRIBUTE_MORE)
      )

  private def diagnoseUnexpectedLiteral(token: StringToken): String =
    val srt = new StringRecoveryToken(token)
    val recoveryResidual = docDecl.resCalc.calcResidual(expression, srt)

    if recoveryResidual == Expression.nullSet then
      return docDecl.localizeMessage(REDocumentDeclaration.DIAG_STRING_NOT_ALLOWED, token.literal.trim)

    expression = docDecl.pool.createChoice(expression, recoveryResidual)

    if srt.failedExps.size() == 1 then
      val texp = srt.failedExps.iterator().next().asInstanceOf[DataOrValueExp]
      try
        texp.getType.checkValid(srt.literal, srt.context)
        texp match
          case vexp: ValueExp =>
            if !vexp.dt.sameValue(vexp.value, vexp.dt.createValue(srt.literal, srt.context)) then
              return docDecl.localizeMessage(REDocumentDeclaration.DIAG_BAD_LITERAL_INCORRECT_VALUE, vexp.value.toString, token.literal.trim)
          case _ =>
        null
      catch
        case de: DatatypeException =>
          if de.getMessage != null then de.getMessage
          else docDecl.localizeMessage(REDocumentDeclaration.DIAG_BAD_LITERAL_GENERIC, token.literal.trim)
    else
      val items = new JHashSet[String]()
      var more = false
      val itr = srt.failedExps.iterator()
      while itr.hasNext do
        val texp = itr.next().asInstanceOf[DataOrValueExp]
        texp match
          case vexp: ValueExp => items.add(vexp.value.toString)
          case _              => more = true

      if items.size() == 0 then null
      else
        docDecl.localizeMessage(
          REDocumentDeclaration.DIAG_BAD_LITERAL_WRAPUP,
          concatenateMessages(items, more, REDocumentDeclaration.DIAG_BAD_LITERAL_SEPARATOR, REDocumentDeclaration.DIAG_BAD_LITERAL_MORE),
          token.literal.trim
        )

  protected def diagnoseUncompletedContent(): String =
    val cccc = docDecl.cccec
    cccc.get(expression, null, false)

    val s = new JHashSet[String]()
    var more = false

    val eocs = cccc.getMatchedElements()
    val len = cccc.numMatchedElements()
    var i = 0
    while i < len do
      val nc = eocs(i).getNameClass
      nc match
        case snc: SimpleNameClass =>
          s.add(docDecl.localizeMessage(REDocumentDeclaration.DIAG_SIMPLE_NAMECLASS, nc.toString))
        case nsnc: NamespaceNameClass =>
          s.add(docDecl.localizeMessage(REDocumentDeclaration.DIAG_NAMESPACE_NAMECLASS, nsnc.namespaceURI))
        case notnc: NotNameClass =>
          notnc.child match
            case nsnc: NamespaceNameClass =>
              s.add(docDecl.localizeMessage(REDocumentDeclaration.DIAG_NOT_NAMESPACE_NAMECLASS, nsnc.namespaceURI))
            case _ =>
              more = true
        case _ =>
          more = true
      i += 1

    if s.size() == 0 then null
    else
      docDecl.localizeMessage(
        REDocumentDeclaration.DIAG_UNCOMPLETED_CONTENT_WRAPUP,
        null.asInstanceOf[AnyRef],
        concatenateMessages(s, more, REDocumentDeclaration.DIAG_UNCOMPLETED_CONTENT_SEPARATOR, REDocumentDeclaration.DIAG_UNCOMPLETED_CONTENT_MORE)
      )
