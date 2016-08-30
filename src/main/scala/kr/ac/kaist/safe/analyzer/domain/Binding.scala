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

import kr.ac.kaist.safe.util.Loc

case class BindingUtil(utils: Utils) {
  val valueU = utils.value
  val boolU = utils.absBool

  val Bot: Binding = Binding(valueU.Bot, boolU.Bot)
  // TODO Top

  // constructor
  def apply(
    value: Value = valueU.Bot,
    mutable: AbsBool = boolU.True
  ): Binding = Binding(value, mutable)
}

case class Binding(
    value: Value,
    mutable: AbsBool
) {
  /* partial order */
  def <=(that: Binding): Boolean = {
    this.value <= that.value &&
      this.mutable <= that.mutable
  }

  /* not a partial order */
  def </(that: Binding): Boolean = !(this <= that)

  /* join */
  def +(that: Binding): Binding = Binding(
    this.value + that.value,
    this.mutable + that.mutable
  )

  /* meet */
  def <>(that: Binding): Binding = Binding(
    this.value + that.value,
    this.mutable + that.mutable
  )

  override def toString: String = s"[${mutable.toString.take(1)}] $value"
}
