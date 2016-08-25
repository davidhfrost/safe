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

package kr.ac.kaist.safe.analyzer.models.builtin

import kr.ac.kaist.safe.analyzer.models._

object BuiltinArray extends FuncModel(
  name = "Array",
  props = List(
    // TODO isArray
    ("isArray", FuncModel(
      name = "Array.isArray",
      code = EmptyCode(argLen = 1)
    ), T, F, T)
  ),
  // TODO @function
  code = EmptyCode(argLen = 1),
  // TODO @construct
  construct = Some(EmptyCode()),
  protoModel = Some((BuiltinArrayProto, F, F, F))
)

object BuiltinArrayProto extends ObjModel(
  name = "Array.prototype",
  props = List(
    ("@class", PrimModel("Array"), F, F, F),
    ("length", PrimModel(0.0), T, F, T),

    // TODO toString
    ("toString", FuncModel(
      name = "Array.prototype.toString",
      code = EmptyCode(argLen = 0)
    ), T, F, T),

    // TODO toLocaleString
    ("toLocaleString", FuncModel(
      name = "Array.prototype.toLocaleString",
      code = EmptyCode(argLen = 0)
    ), T, F, T),

    // TODO concat
    ("concat", FuncModel(
      name = "Array.prototype.concat",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO join
    ("join", FuncModel(
      name = "Array.prototype.join",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO pop
    ("pop", FuncModel(
      name = "Array.prototype.pop",
      code = EmptyCode(argLen = 0)
    ), T, F, T),

    // TODO push
    ("push", FuncModel(
      name = "Array.prototype.push",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO reverse
    ("reverse", FuncModel(
      name = "Array.prototype.reverse",
      code = EmptyCode(argLen = 0)
    ), T, F, T),

    // TODO shift
    ("shift", FuncModel(
      name = "Array.prototype.shift",
      code = EmptyCode(argLen = 0)
    ), T, F, T),

    // TODO slice
    ("slice", FuncModel(
      name = "Array.prototype.slice",
      code = EmptyCode(argLen = 2)
    ), T, F, T),

    // TODO sort
    ("sort", FuncModel(
      name = "Array.prototype.sort",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO splice
    ("splice", FuncModel(
      name = "Array.prototype.splice",
      code = EmptyCode(argLen = 2)
    ), T, F, T),

    // TODO unshift
    ("unshift", FuncModel(
      name = "Array.prototype.unshift",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO indexOf
    ("indexOf", FuncModel(
      name = "Array.prototype.indexOf",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO lastIndexOf
    ("lastIndexOf", FuncModel(
      name = "Array.prototype.lastIndexOf",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO every
    ("every", FuncModel(
      name = "Array.prototype.every",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO some
    ("some", FuncModel(
      name = "Array.prototype.some",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO forEach
    ("forEach", FuncModel(
      name = "Array.prototype.forEach",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO map
    ("map", FuncModel(
      name = "Array.prototype.map",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO filter
    ("filter", FuncModel(
      name = "Array.prototype.filter",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO reduce
    ("reduce", FuncModel(
      name = "Array.prototype.reduce",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO reduceRight
    ("reduceRight", FuncModel(
      name = "Array.prototype.reduceRight",
      code = EmptyCode(argLen = 1)
    ), T, F, T)
  )
)
