/**
 * *****************************************************************************
 * Copyright (c) 2016-2018, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.nodes.cfg._

// a "control point" is a step in program execution.
// a program state is maintained for each control point during analysis.

// note that many control points may correspond to the same block of code (`CFGBlock`).
// the same block of code may be traversed many times throughout a program's execution,
// and so it's useful to be able to distinguish different traversals of the same block of code.

// the most clear examples of code blocks that get traversed more than once
// are function bodies and loop bodies, and trace partition tokens exist to handle both of these cases.
// namely, `CallSiteContext` and `LoopContext`, respectively.

case class ControlPoint(
    block: CFGBlock,
    tracePartition: TracePartition
) {
  // computes a list of control points across the edge from this control point to the destination block `to`.
  // note that the edge type `edgeType` is specified.
  def next(
    to: CFGBlock,
    edgeType: CFGEdgeType,
    sem: Semantics,
    st: AbsState
  ): List[ControlPoint] = {
    tracePartition.next(block, to, edgeType, sem, st).map(ControlPoint(to, _))
  }
  override def toString: String = {
    val fid = block.func.id
    s"($fid:$block, $tracePartition)"
  }
}
