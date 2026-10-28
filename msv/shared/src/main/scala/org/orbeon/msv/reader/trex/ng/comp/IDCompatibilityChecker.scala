package org.orbeon.msv.reader.trex.ng.comp

import java.util.{ArrayList as JArrayList, HashMap, HashSet, Iterator as JIterator, List as JList, Map as JMap, Set as JSet}
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.util.{ExpressionWalker, RefExpRemover}
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.StringPair
import org.xml.sax.Locator

class IDCompatibilityChecker(reader: RELAXNGCompReader) extends CompatibilityChecker(reader):

  protected def setCompatibility(val_ : Boolean): Unit =
    grammar.isIDcompatible = val_

  private final class IDAttMap(val sampleDecl: ElementExp) {
    val idatts: JMap[StringPair, StringPair] = new HashMap[StringPair, StringPair]()
  }

  def test(): Unit =
    grammar.isIDcompatible = true

    val name2value: JMap[StringPair, IDAttMap] = new HashMap[StringPair, IDAttMap]()
    val elements: JSet[ElementExp] = new HashSet[ElementExp]()
    val remover: RefExpRemover = new RefExpRemover(reader.pool, false)

    /*
     * 1st pass
     */
    reader.getGrammar.visit(new ExpressionWalker {
      private var elementName: StringPair = null
      private var curElm: ElementExp = null
      private var curAtts: IDAttMap = null

      override def onElement(exp: ElementExp): Unit =
        if !elements.add(exp) then return

        val _en = elementName
        val _curAtts = curAtts
        val _curElm = curElm

        val nc = exp.getNameClass
        if nc.isInstanceOf[SimpleNameClass] then
          elementName = new StringPair(nc.asInstanceOf[SimpleNameClass])
          curAtts = name2value.get(elementName)
        else
          elementName = null
        curElm = exp

        exp.contentModel.visit(remover).visit(this)

        if elementName != null && curAtts != null then
          name2value.put(elementName, curAtts)

        elementName = _en
        curAtts = _curAtts
        curElm = _curElm

      override def onAttribute(exp: AttributeExp): Unit =
        if !exp.exp.isInstanceOf[DataOrValueExp] then
          exp.exp.visit(this)
          return

        val texp = exp.exp.asInstanceOf[DataOrValueExp]
        if texp.getType.getIdType() == Datatype.ID_TYPE_NULL then
          return

        if !exp.nameClass.isInstanceOf[SimpleNameClass] then
          reportCompError(
            Array[Locator](reader.getDeclaredLocationOf(exp)),
            IDCompatibilityChecker.CERR_ID_TYPE_WITH_NON_SIMPLE_ATTNAME,
            Array[AnyRef](texp.getName.localName, getSemanticsStr(texp.getType.getIdType()))
          )
          return

        val attName = new StringPair(exp.nameClass.asInstanceOf[SimpleNameClass])

        if elementName == null then
          reportCompError(
            Array[Locator](
              reader.getDeclaredLocationOf(exp),
              reader.getDeclaredLocationOf(curElm)
            ),
            IDCompatibilityChecker.CERR_ID_TYPE_WITH_NON_SIMPLE_ELEMENTNAME,
            Array[AnyRef](texp.getName.localName, getSemanticsStr(texp.getType.getIdType()))
          )
          return

        if curAtts == null then
          curAtts = new IDAttMap(curElm)
        curAtts.idatts.put(attName, texp.getName)

      override def onData(exp: DataExp): Unit = checkIdType(exp)
      override def onValue(exp: ValueExp): Unit = checkIdType(exp)

      private def checkIdType(exp: DataOrValueExp): Unit =
        if exp.getType.getIdType() != Datatype.ID_TYPE_NULL then
          reportCompError(
            Array[Locator](reader.getDeclaredLocationOf(exp)),
            IDCompatibilityChecker.CERR_MALPLACED_ID_TYPE,
            Array[AnyRef](exp.getName.localName, getSemanticsStr(exp.getType.getIdType()))
          )
    })

    if !grammar.isIDcompatible then return

    /*
     * 2nd pass
     */
    val itr = elements.iterator()
    val vec: JList[IDAttMap] = new JArrayList[IDAttMap]()
    while itr.hasNext do
      val eexp = itr.next()
      vec.clear()
      val jtr = name2value.entrySet().iterator()
      while jtr.hasNext do
        val e = jtr.next()
        if eexp.getNameClass.accepts(e.getKey) then
          vec.add(e.getValue)

      if !vec.isEmpty then
        eexp.contentModel.visit(remover).visit(new ExpressionWalker {
          override def onElement(exp: ElementExp): Unit = ()

          override def onAttribute(exp: AttributeExp): Unit =
            if exp.exp.isInstanceOf[DataOrValueExp] then
              val texp = exp.exp.asInstanceOf[DataOrValueExp]
              if texp.getType.getIdType() != Datatype.ID_TYPE_NULL then
                assert(vec.size() == 1)
                val attName = exp.nameClass.asInstanceOf[SimpleNameClass]
                val iam = vec.get(0)
                if texp.getName != iam.idatts.get(new StringPair(attName)) then
                  reportCompError(
                    Array[Locator](
                      reader.getDeclaredLocationOf(exp),
                      reader.getDeclaredLocationOf(iam.sampleDecl)
                    ),
                    IDCompatibilityChecker.CERR_COMPETING,
                    Array[AnyRef](texp.getName.localName, getSemanticsStr(texp.getType.getIdType()))
                  )
                return

            var i = vec.size() - 1
            while i >= 0 do
              val iam = vec.get(i)
              val jtr = iam.idatts.entrySet().iterator()
              while jtr.hasNext do
                val e = jtr.next()
                if exp.nameClass.accepts(e.getKey) then
                  reportCompError(
                    Array[Locator](
                      reader.getDeclaredLocationOf(exp),
                      reader.getDeclaredLocationOf(eexp),
                      reader.getDeclaredLocationOf(iam.sampleDecl)
                    ),
                    IDCompatibilityChecker.CERR_COMPETING2,
                    Array[AnyRef](e.getKey.localName, e.getValue.localName)
                  )
                  return
              i -= 1

          override def onList(exp: ListExp): Unit = ()
        })

  private def getSemanticsStr(type_ : Int): String =
    type_ match
      case Datatype.ID_TYPE_ID => "ID"
      case Datatype.ID_TYPE_IDREF => "IDREF"
      case Datatype.ID_TYPE_IDREFS => "IDREFS"
      case _ => throw new Error()

object IDCompatibilityChecker:
  val CERR_MALPLACED_ID_TYPE = "RELAXNGReader.Compatibility.ID.MalplacedIDType"
  val CERR_ID_TYPE_WITH_NON_SIMPLE_ATTNAME = "RELAXNGReader.Compatibility.ID.IDTypeWithNonSimpleAttName"
  val CERR_ID_TYPE_WITH_NON_SIMPLE_ELEMENTNAME = "RELAXNGReader.Compatibility.ID.IDTypeWithNonSimpleElementName"
  val CERR_COMPETING = "RELAXNGReader.Compatibility.ID.Competing"
  val CERR_COMPETING2 = "RELAXNGReader.Compatibility.ID.Competing2"
