/**
 * *****************************************************************************
 * Copyright (c) 2016, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer.domain

import kr.ac.kaist.safe.analyzer.domain.Utils._
import scala.collection.immutable.HashSet

////////////////////////////////////////////////////////////////////////////////
// concrete primitive value type
////////////////////////////////////////////////////////////////////////////////
abstract class PValue extends Value

////////////////////////////////////////////////////////////////////////////////
// primitive value abstract domain
////////////////////////////////////////////////////////////////////////////////
trait AbsPValue extends AbsDomain[PValue, AbsPValue] {
  val undefval: AbsUndef
  val nullval: AbsNull
  val boolval: AbsBool
  val numval: AbsNumber
  val strval: AbsString

  def typeCount: Int
  def typeKinds: String
  def toStringSet: Set[AbsString]
  def copyWith(
    undefval: AbsUndef = this.undefval,
    nullval: AbsNull = this.nullval,
    boolval: AbsBool = this.boolval,
    numval: AbsNumber = this.numval,
    strval: AbsString = this.strval
  ): AbsPValue
}

trait AbsPValueUtil extends AbsDomainUtil[PValue, AbsPValue] {
  def apply(undefval: AbsUndef): AbsPValue
  def apply(nullval: AbsNull): AbsPValue
  def apply(boolval: AbsBool): AbsPValue
  def apply(numval: AbsNumber): AbsPValue
  def apply(strval: AbsString): AbsPValue
}

////////////////////////////////////////////////////////////////////////////////
// default primitive value abstract domain
////////////////////////////////////////////////////////////////////////////////
object DefaultPValue extends AbsPValueUtil {
  lazy val Bot: AbsDom =
    AbsDom(AbsUndef.Bot, AbsNull.Bot, AbsBool.Bot, AbsNumber.Bot, AbsString.Bot)
  lazy val Top: AbsDom =
    AbsDom(AbsUndef.Top, AbsNull.Top, AbsBool.Top, AbsNumber.Top, AbsString.Top)

  def alpha(pvalue: PValue): AbsPValue = pvalue match {
    case Undef => Bot.copy(undefval = AbsUndef.Top)
    case Null => Bot.copy(nullval = AbsNull.Top)
    case Bool(b) => Bot.copy(boolval = AbsBool(b))
    case Num(n) => Bot.copy(numval = AbsNumber(n))
    case Str(str) => Bot.copy(strval = AbsString(str))
  }

  def apply(undefval: AbsUndef): AbsPValue = Bot.copyWith(undefval = undefval)
  def apply(nullval: AbsNull): AbsPValue = Bot.copyWith(nullval = nullval)
  def apply(boolval: AbsBool): AbsPValue = Bot.copyWith(boolval = boolval)
  def apply(numval: AbsNumber): AbsPValue = Bot.copyWith(numval = numval)
  def apply(strval: AbsString): AbsPValue = Bot.copyWith(strval = strval)

  case class AbsDom(
      undefval: AbsUndef,
      nullval: AbsNull,
      boolval: AbsBool,
      numval: AbsNumber,
      strval: AbsString
  ) extends AbsPValue {
    def gamma: ConSet[PValue] = ConSetTop() // TODO more precisely

    def isBottom: Boolean = this == Bot

    /* partial order */
    def <=(that: AbsPValue): Boolean = {
      val (left, right) = (this, check(that))
      if (left eq right) true
      else {
        (left.undefval <= right.undefval) &&
          (left.nullval <= right.nullval) &&
          (left.boolval <= right.boolval) &&
          (left.numval <= right.numval) &&
          (left.strval <= right.strval)
      }
    }

    /* join */
    def +(that: AbsPValue): AbsPValue = {
      val (left, right) = (this, check(that))
      if (left eq right) left
      else {
        AbsDom(
          left.undefval + right.undefval,
          left.nullval + right.nullval,
          left.boolval + right.boolval,
          left.numval + right.numval,
          left.strval + right.strval
        )
      }
    }

    /* meet */
    def <>(that: AbsPValue): AbsPValue = {
      val (left, right) = (this, check(that))
      AbsDom(
        left.undefval <> right.undefval,
        left.nullval <> right.nullval,
        left.boolval <> right.boolval,
        left.numval <> right.numval,
        left.strval <> right.strval
      )
    }

    override def toString(): String = {
      var lst: List[String] = Nil

      this.undefval.fold(()) { lst ::= _.toString }
      this.nullval.fold(()) { lst ::= _.toString }
      this.boolval.fold(()) { lst ::= _.toString }
      this.numval.fold(()) { lst ::= _.toString }
      this.strval.fold(()) { lst ::= _.toString }

      lst match {
        case Nil => "⊥PValue"
        case _ => lst.mkString(", ")
      }
    }

    def typeCount: Int = {
      var count = 0;
      this.undefval.fold(()) { _ => count += 1 }
      this.nullval.fold(()) { _ => count += 1 }
      this.boolval.fold(()) { _ => count += 1 }
      this.numval.fold(()) { _ => count += 1 }
      this.strval.fold(()) { _ => count += 1 }
      count
    }

    def typeKinds: String = {
      var lst: List[String] = Nil
      this.undefval.fold(()) { _ => lst ::= "Undefined" }
      this.nullval.fold(()) { _ => lst ::= "Null" }
      this.boolval.fold(()) { _ => lst ::= "Boolean" }
      this.numval.fold(()) { _ => lst ::= "Number" }
      this.strval.fold(()) { _ => lst ::= "String" }
      lst.mkString(", ")
    }

    def toStringSet: Set[AbsString] = {
      var set = HashSet[AbsString]()

      this.undefval.foldUnit(set += AbsString("undefined"))
      this.nullval.foldUnit(set += AbsString("null"))

      if (AbsBool.True <= this.boolval) set += AbsString("true")
      if (AbsBool.False <= this.boolval) set += AbsString("false")

      set += this.numval.toAbsString

      this.strval.foldUnit(set += this.strval)

      // remove redundancies
      set.filter(s => !set.exists(o => s != o && s <= o))
    }

    def copyWith(
      undefval: AbsUndef = this.undefval,
      nullval: AbsNull = this.nullval,
      boolval: AbsBool = this.boolval,
      numval: AbsNumber = this.numval,
      strval: AbsString = this.strval
    ): AbsPValue = AbsDom(undefval, nullval, boolval, numval, strval)
  }
}
