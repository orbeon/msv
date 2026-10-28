package org.orbeon.msv.reader.datatype.xsd

import java.util.{ArrayList as JArrayList, Collection as JCollection, Iterator as JIterator, List as JList}
import org.xml.sax.Locator
import org.orbeon.msv.datatype.xsd.{DatatypeFactory, FinalComponent, StringType, TypeIncubator, XSDatatype, XSDatatypeImpl}
import org.orbeon.msv.grammar.{ExpressionPool, ReferenceExp}
import org.orbeon.msv.reader.{GrammarReader, State}
import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

/**
 * A wrapper of XSDatatype that serves as an expression
 * and encapsulates lazy-constructed datatypes.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class XSDatatypeExp private (
  nsUri: String,
  typeName: String
) extends ReferenceExp(typeName) with GrammarReader.BackPatch:

  private val namespaceUri: String = nsUri
  private var dt: XSDatatype = scala.compiletime.uninitialized
  private var pool: ExpressionPool = scala.compiletime.uninitialized
  @transient private var ownerState: State = scala.compiletime.uninitialized
  @transient private var renderer: XSDatatypeExp.Renderer = scala.compiletime.uninitialized

  /** Creates this object from existing XSDatatype. */
  def this(dt: XSDatatype, _pool: ExpressionPool) =
    this(dt.getNamespaceUri, dt.getName)
    this.dt = dt
    this.pool = _pool
    this.ownerState = null
    this.renderer = null
    this.exp = _pool.createData(dt)

  /** Creates lazily created datatype. */
  def this(nsUri: String, typeName: String, reader: GrammarReader, _renderer: XSDatatypeExp.Renderer) =
    this(nsUri, typeName)
    this.dt = null
    this.ownerState = reader.getCurrentState
    this.renderer = _renderer
    this.pool = reader.pool
    reader.addBackPatchJob(this)

  def createIncubator(): XSTypeIncubator =
    if isLateBind then
      new LazyTypeIncubator(this, ownerState.reader)
    else
      new XSTypeIncubator:
        private val core = new TypeIncubator(dt)

        def addFacet(name: String, value: String, fixed: Boolean, context: ValidationContext): Unit =
          core.addFacet(name, value, fixed, context)

        def derive(uri: String, localName: String): XSDatatypeExp =
          new XSDatatypeExp(core.derive(uri, localName), pool)

  def getCreatedType(): XSDatatype =
    if dt == null then throw new IllegalStateException()
    dt

  def getType(context0: XSDatatypeExp.RenderingContext): XSDatatype =
    if dt != null then return dt

    val context = if context0 == null then new XSDatatypeExp.RenderingContext() else context0

    if context.callStack.contains(this) then
      val locs = new JArrayList[Locator]()
      var i = 0
      while i < context.callStack.size() do
        locs.add(context.callStack.get(i).ownerState.getLocation)
        i += 1
      ownerState.reader.reportError(
        locs.toArray(new Array[Locator](locs.size())),
        GrammarReader.ERR_RECURSIVE_DATATYPE,
        null
      )
      return StringType.theInstance

    context.callStack.add(this)

    try
      dt = renderer.render(context)
    catch
      case e: DatatypeException =>
        ownerState.reader.reportError(
          GrammarReader.ERR_BAD_TYPE,
          Array[AnyRef](e),
          e,
          Array(ownerState.getLocation)
        )
        dt = StringType.theInstance

    context.callStack.remove(context.callStack.size() - 1)

    if dt == null then throw new Error()

    this.exp = pool.createData(dt)
    dt

  def patch(): Unit = getType(null)

  def getOwnerState: State = ownerState

  final def isLateBind: Boolean = dt == null

  def getClone(): XSDatatypeExp =
    val t = new XSDatatypeExp(this.namespaceUri, this.name)
    t.redefine(this)
    t

  def redefine(rhs: XSDatatypeExp): Unit =
    this.exp = rhs.exp
    this.dt = rhs.dt
    this.pool = rhs.pool
    this.ownerState = rhs.ownerState
    this.renderer = rhs.renderer

    if ownerState != null then
      ownerState.reader.addBackPatchJob(this)

  def createFinalizedType(finalValue: Int, reader: GrammarReader): XSDatatypeExp =
    if finalValue == 0 then return this

    if !isLateBind then
      new XSDatatypeExp(new FinalComponent(dt.asInstanceOf[XSDatatypeImpl], finalValue), pool)
    else
      new XSDatatypeExp(this.namespaceUri, this.name, reader, new XSDatatypeExp.Renderer:
        def render(context: XSDatatypeExp.RenderingContext): XSDatatype =
          new FinalComponent(getType(context).asInstanceOf[XSDatatypeImpl], finalValue)
      )

object XSDatatypeExp:

  trait Renderer:
    @throws(classOf[DatatypeException])
    def render(context: RenderingContext): XSDatatype

  class RenderingContext:
    private[xsd] val callStack: JList[XSDatatypeExp] = new JArrayList[XSDatatypeExp]()

  @throws(classOf[DatatypeException])
  def makeList(nsUri: String, typeName: String, itemType: XSDatatypeExp, reader: GrammarReader): XSDatatypeExp =
    if !itemType.isLateBind then
      new XSDatatypeExp(DatatypeFactory.deriveByList(nsUri, typeName, itemType.dt), reader.pool)
    else
      new XSDatatypeExp(nsUri, typeName, reader, new Renderer:
        def render(context: RenderingContext): XSDatatype =
          DatatypeFactory.deriveByList(nsUri, typeName, itemType.getType(context))
      )

  @throws(classOf[DatatypeException])
  def makeUnion(typeNameUri: String, typeName: String, members: JCollection[XSDatatypeExp], reader: GrammarReader): XSDatatypeExp =
    val m = new Array[XSDatatype](members.size())
    var hasLateBind = false
    val itr = members.iterator()
    while itr.hasNext do
      if itr.next().isLateBind then hasLateBind = true

    if hasLateBind then
      new XSDatatypeExp(typeNameUri, typeName, reader, new Renderer:
        def render(context: RenderingContext): XSDatatype =
          var i = 0
          val itr2 = members.iterator()
          while itr2.hasNext do
            m(i) = itr2.next().getType(context)
            i += 1
          DatatypeFactory.deriveByUnion(typeNameUri, typeName, m)
      )
    else
      var i = 0
      val itr2 = members.iterator()
      while itr2.hasNext do
        m(i) = itr2.next().dt
        i += 1
      new XSDatatypeExp(DatatypeFactory.deriveByUnion(typeNameUri, typeName, m), reader.pool)
