package org.orbeon.msv.grammar.util

import org.orbeon.msv.grammar.{AttributeExp, BinaryExp, ChoiceExp, ConcurExp, DataExp, ElementExp, Expression, ExpressionVisitor, InterleaveExp, ListExp, MixedExp, OneOrMoreExp, OtherExp, ReferenceContainer, ReferenceExp, SequenceExp, ValueExp}

/**
 * creates a string representation of the expression.
 *
 * useful for debug and dump.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ExpressionPrinter(protected val mode: Int) extends ExpressionVisitor {
  import ExpressionPrinter._

  def printRefContainer(cont: ReferenceContainer): String = {
    var r = ""
    val itr = cont.iterator()
    while (itr.hasNext) {
      val exp = itr.next()
      r += exp.name + "  : " + exp.exp.visit(this) + "\n"
    }
    r
  }

  protected def printBinary(exp: BinaryExp, op: String): String = {
    var r = ""
    if (exp.exp1.getClass == exp.getClass || !isComplex(exp.exp1))
      r = exp.exp1.visit(this).asInstanceOf[String]
    else
      r = "(" + exp.exp1.visit(this) + ")"

    r += op

    if (!isComplex(exp.exp2))
      r += exp.exp2.visit(this)
    else
      r += "(" + exp.exp2.visit(this) + ")"

    r
  }

  def onAttribute(exp: AttributeExp): AnyRef =
    "@" + exp.nameClass.toString + "<" + exp.exp.visit(this) + ">"

  private def optional(exp: Expression): AnyRef = {
    if (exp.isInstanceOf[OneOrMoreExp]) {
      val ome = exp.asInstanceOf[OneOrMoreExp]
      if (isComplex(ome.exp)) "(" + ome.exp.visit(this).asInstanceOf[String] + ")*"
      else ome.exp.visit(this).asInstanceOf[String] + "*"
    } else {
      if (isComplex(exp)) "(" + exp.visit(this).asInstanceOf[String] + ")?"
      else exp.visit(this).asInstanceOf[String] + "?"
    }
  }

  def onChoice(exp: ChoiceExp): AnyRef = {
    if (exp.exp1 eq Expression.epsilon) return optional(exp.exp2)
    if (exp.exp2 eq Expression.epsilon) return optional(exp.exp1)
    printBinary(exp, "|")
  }

  def onConcur(exp: ConcurExp): AnyRef = printBinary(exp, "&")
  def onInterleave(exp: InterleaveExp): AnyRef = printBinary(exp, "^")

  def onElement(exp: ElementExp): AnyRef = {
    if ((mode & CONTENTMODEL) != 0)
      exp.getNameClass.toString
    else
      exp.getNameClass.toString + "<" + exp.contentModel.visit(this).asInstanceOf[String] + ">"
  }

  def onOneOrMore(exp: OneOrMoreExp): AnyRef = {
    if (isComplex(exp.exp)) "(" + exp.exp.visit(this).asInstanceOf[String] + ")+"
    else exp.exp.visit(this).asInstanceOf[String] + "+"
  }

  def onMixed(exp: MixedExp): AnyRef =
    "mixed[" + exp.exp.visit(this).asInstanceOf[String] + "]"

  def onList(exp: ListExp): AnyRef =
    "list[" + exp.exp.visit(this).asInstanceOf[String] + "]"

  def onEpsilon(): AnyRef = "#epsilon"
  def onNullSet(): AnyRef = "#nullSet"
  def onAnyString(): AnyRef = "<anyString>"
  def onSequence(exp: SequenceExp): AnyRef = printBinary(exp, ",")
  def onData(exp: DataExp): AnyRef = "$" + exp.name.localName
  def onValue(exp: ValueExp): AnyRef = "$$" + exp.value
  def onOther(exp: OtherExp): AnyRef = exp.printName() + "[" + exp.exp.visit(this).asInstanceOf[String] + "]"

  def onRef(exp: ReferenceExp): AnyRef = {
    if ((mode & FRAGMENT) != 0) "{%" + exp.name + "}"
    else "(" + exp.exp.visit(this).asInstanceOf[String] + ")"
  }
}

object ExpressionPrinter {
  val FRAGMENT: Int = 0x001
  val CONTENTMODEL: Int = 0x002

  val fragmentInstance: ExpressionPrinter = new ExpressionPrinter(FRAGMENT)
  val contentModelInstance: ExpressionPrinter = new ExpressionPrinter(CONTENTMODEL)
  val smallestInstance: ExpressionPrinter = new ExpressionPrinter(CONTENTMODEL | FRAGMENT)

  def printFragment(exp: Expression): String =
    exp.visit(fragmentInstance).asInstanceOf[String]

  def printContentModel(exp: Expression): String =
    exp.visit(contentModelInstance).asInstanceOf[String]

  def printSmallest(exp: Expression): String =
    exp.visit(smallestInstance).asInstanceOf[String]

  protected def isComplex(exp: Expression): Boolean =
    exp.isInstanceOf[BinaryExp]
}
