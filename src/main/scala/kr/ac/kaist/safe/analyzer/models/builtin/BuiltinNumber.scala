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

import kr.ac.kaist.safe.analyzer.models.{ PrimModel, FuncModel, EmptyCode }

// TODO Number
object BuiltinNumber extends FuncModel(
  name = "Number",
  props = List(
    ("length", PrimModel(1), F, F, F),
    ("MAX_VALUE", PrimModel(Double.MaxValue), F, F, F),
    ("MIN_VALUE", PrimModel(Double.MinValue), F, F, F),
    ("NaN", PrimModel(Double.NaN), F, F, F),
    ("NEGATIVE_INFINITY", PrimModel(Double.NegativeInfinity), F, F, F),
    ("POSITIVE_INFINITY", PrimModel(Double.PositiveInfinity), F, F, F)
  ),
  protoProps = List(
    ("@class", PrimModel("Number"), F, F, F)
  ),
  prototypeWritable = F,
  argLen = 1,
  code = EmptyCode
) with Builtin
