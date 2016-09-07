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

// concrete undefined type
sealed abstract class Undef
case object Undef extends Undef

// undefined abstract domain
trait AbsUndef extends AbsDomain[Undef, AbsUndef] with Primitive {
  def gamma: ConSimple[Undef]
  def ===(that: AbsUndef): AbsBool
}

trait AbsUndefUtil extends AbsDomainUtil[Undef, AbsUndef] {
  def alpha(value: Undef): AbsUndef = Top
}
