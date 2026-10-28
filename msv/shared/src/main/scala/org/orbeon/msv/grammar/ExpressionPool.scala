package org.orbeon.msv.grammar

import org.orbeon.msv.datatype.xsd.XSDatatype
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.StringPair

/**
 * Creates a new Expression by combining existing expressions.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ExpressionPool(parent: ExpressionPool) extends Serializable {
  def this() = this(null.asInstanceOf[ExpressionPool])

  private val expTable: ExpressionPool.ClosedHash =
    if (parent != null) new ExpressionPool.ClosedHash(parent.expTable)
    else new ExpressionPool.ClosedHash()

  final def createAttribute(nameClass: NameClass): Expression =
    unify(new AttributeExp(nameClass, Expression.anyString))

  final def createAttribute(nameClass: NameClass, content: Expression): Expression = {
    if (content eq Expression.nullSet) content
    else unify(new AttributeExp(nameClass, content))
  }

  final def createEpsilon(): Expression = Expression.epsilon
  final def createNullSet(): Expression = Expression.nullSet
  final def createAnyString(): Expression = Expression.anyString

  final def createChoice(left: Expression, right: Expression): Expression = {
    if (left eq Expression.nullSet) return right
    if (right eq Expression.nullSet) return left

    if ((left eq Expression.epsilon) && right.isEpsilonReducible) return right
    if ((right eq Expression.epsilon) && left.isEpsilonReducible) return left

    if (right.isInstanceOf[ChoiceExp]) {
      val c = right.asInstanceOf[ChoiceExp]
      return createChoice(createChoice(left, c.exp1), c.exp2)
    }

    var next = left
    var searching = true
    while (searching) {
      if (next eq right) return left
      if (!next.isInstanceOf[ChoiceExp]) {
        searching = false
      } else {
        val cp = next.asInstanceOf[ChoiceExp]
        if (cp.exp2 eq right) return left
        next = cp.exp1
      }
    }

    val o = expTable.getBinExp(left, right, classOf[ChoiceExp])
    if (o == null)
      unify(new ChoiceExp(left, right))
    else
      o
  }

  final def createOneOrMore(child: Expression): Expression = {
    if ((child eq Expression.epsilon) ||
        (child eq Expression.anyString) ||
        (child eq Expression.nullSet) ||
        child.isInstanceOf[OneOrMoreExp])
      return child

    unify(new OneOrMoreExp(child))
  }

  final def createZeroOrMore(child: Expression): Expression =
    createOptional(createOneOrMore(child))

  final def createOptional(child: Expression): Expression =
    createChoice(child, Expression.epsilon)

  final def createData(dt: XSDatatype): Expression = {
    var ns = dt.getNamespaceUri
    if (ns == null) ns = "\u0000"
    createData(dt, new StringPair(ns, dt.displayName()))
  }

  final def createData(dt: Datatype, typeName: StringPair): Expression =
    createData(dt, typeName, Expression.nullSet)

  final def createData(dt: Datatype, typeName: StringPair, except: Expression): Expression =
    unify(new DataExp(dt, typeName, except))

  final def createValue(dt: XSDatatype, value: AnyRef): Expression =
    createValue(dt, new StringPair("", dt.displayName()), value)

  final def createValue(dt: Datatype, typeName: StringPair, value: AnyRef): Expression =
    unify(new ValueExp(dt, typeName, value))

  final def createList(exp: Expression): Expression = {
    if (exp eq Expression.nullSet) exp
    else unify(new ListExp(exp))
  }

  final def createMixed(body: Expression): Expression = {
    if (body eq Expression.nullSet) Expression.nullSet
    else if (body eq Expression.epsilon) Expression.anyString
    else unify(new MixedExp(body))
  }

  final def createSequence(left: Expression, right: Expression): Expression = {
    if ((left eq Expression.nullSet) || (right eq Expression.nullSet)) return Expression.nullSet
    if (left eq Expression.epsilon) return right
    if (right eq Expression.epsilon) return left

    if (right.isInstanceOf[SequenceExp]) {
      val s = right.asInstanceOf[SequenceExp]
      return createSequence(createSequence(left, s.exp1), s.exp2)
    }

    val o = expTable.getBinExp(left, right, classOf[SequenceExp])
    if (o == null)
      unify(new SequenceExp(left, right))
    else
      o
  }

  final def createConcur(left: Expression, right: Expression): Expression = {
    if ((left eq Expression.nullSet) || (right eq Expression.nullSet)) return Expression.nullSet
    if (left eq Expression.epsilon) {
      if (right.isEpsilonReducible) return Expression.epsilon
      else return Expression.nullSet
    }
    if (right eq Expression.epsilon) {
      if (left.isEpsilonReducible) return Expression.epsilon
      else return Expression.nullSet
    }

    if (right.isInstanceOf[ConcurExp]) {
      val c = right.asInstanceOf[ConcurExp]
      return createConcur(createConcur(left, c.exp1), c.exp2)
    }

    unify(new ConcurExp(left, right))
  }

  final def createInterleave(left: Expression, right: Expression): Expression = {
    if (left eq Expression.epsilon) return right
    if (right eq Expression.epsilon) return left
    if ((left eq Expression.nullSet) || (right eq Expression.nullSet)) return Expression.nullSet

    if (right.isInstanceOf[InterleaveExp]) {
      val i = right.asInstanceOf[InterleaveExp]
      return createInterleave(createInterleave(left, i.exp1), i.exp2)
    }

    unify(new InterleaveExp(left, right))
  }

  protected final def unify(exp: Expression): Expression = {
    var o = expTable.get(exp)
    if (o == null) {
      expTable.synchronized {
        o = expTable.get(exp)
        if (o == null) {
          expTable.put(exp)
          return exp
        }
      }
    }
    o
  }
}

object ExpressionPool {
  final class ClosedHash(private var parent: ClosedHash) extends Serializable {
    def this() = this(null)

    private val loadFactor: Float = 0.3f
    private val initialCapacity: Int = 191

    private var table: Array[Expression] = new Array[Expression](initialCapacity)
    private var count: Int = 0
    private var threshold: Int = (initialCapacity * loadFactor).toInt

    def getBinExp(left: Expression, right: Expression, `type`: Class[?]): Expression = {
      val hash = (left.hashCode + right.hashCode) ^ `type`.hashCode
      getBinExp(hash, left, right, `type`)
    }

    private def getBinExp(hash: Int, left: Expression, right: Expression, `type`: Class[?]): Expression = {
      if (parent != null) {
        val e = parent.getBinExp(hash, left, right, `type`)
        if (e != null) return e
      }

      val tab = table
      var index = (hash & 0x7fffffff) % tab.length
      while (true) {
        val e = tab(index)
        if (e == null) return null
        if (e.hashCode == hash && (e.getClass eq `type`)) {
          val be = e.asInstanceOf[BinaryExp]
          if ((be.exp1 eq left) && (be.exp2 eq right)) return be
        }
        index = (index + 1) % tab.length
      }
      null
    }

    def get(hash: Int, child: Expression, `type`: Class[?]): Expression = {
      if (parent != null) {
        val e = parent.get(hash, child, `type`)
        if (e != null) return e
      }

      val tab = table
      var index = (hash & 0x7fffffff) % tab.length
      while (true) {
        val e = tab(index)
        if (e == null) return null
        if (e.hashCode == hash && (e.getClass eq `type`)) {
          val ue = e.asInstanceOf[UnaryExp]
          if (ue.exp eq child) return ue
        }
        index = (index + 1) % tab.length
      }
      null
    }

    def get(key: Expression): Expression = {
      if (parent != null) {
        val e = parent.get(key)
        if (e != null) return e
      }

      val tab = table
      var index = (key.hashCode & 0x7fffffff) % tab.length
      while (true) {
        val e = tab(index)
        if (e == null) return null
        if (e.equals(key)) return e
        index = (index + 1) % tab.length
      }
      null
    }

    private def rehash(): Unit = {
      val oldCapacity = table.length
      val oldMap = table
      val newCapacity = oldCapacity * 2 + 1
      val newMap = new Array[Expression](newCapacity)

      var i = oldCapacity
      while (i > 0) {
        i -= 1
        if (oldMap(i) != null) {
          var index = (oldMap(i).hashCode & 0x7fffffff) % newMap.length
          while (newMap(index) != null) {
            index = (index + 1) % newMap.length
          }
          newMap(index) = oldMap(i)
        }
      }

      threshold = (newCapacity * loadFactor).toInt
      table = newMap
    }

    def put(newExp: Expression): Unit = {
      if (count >= threshold)
        rehash()

      val tab = table
      var index = (newExp.hashCode & 0x7fffffff) % tab.length
      while (tab(index) != null) {
        index = (index + 1) % tab.length
      }
      tab(index) = newExp
      count += 1
    }
  }
}
