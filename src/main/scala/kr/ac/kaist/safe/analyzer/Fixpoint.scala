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
import kr.ac.kaist.safe.analyzer.console.Interactive
import kr.ac.kaist.safe.nodes.cfg._

class Fixpoint(
    semantics: Semantics,
    val consoleOpt: Option[Interactive]
) {
  def worklist: Worklist = semantics.worklist

  // performs the worklist algorithm on the given `Semantics` object.
  // that is, the initial program state is propagated across CFG edges
  // until it stabilizes to a fixpoint.
  def compute(initIters: Int = 0): (Int, Double) = {
    // set the start time.
    val startTime = System.nanoTime

    var iters = initIters

    // the main (and only) loop of the worklist algorithm.
    // repeatedly run `computeOneStep` until the worklist queue is empty.
    while (!worklist.isEmpty) {
      iters += 1
      computeOneStep
    }
    consoleOpt.foreach(_.runFinished)

    // calculate duration
    val duration = (System.nanoTime - startTime) / 1e9

    (iters, duration)
  }

  var cpSet: Set[CFGBlock] = Set()

  // performs one step of the worklist algorithm.
  def computeOneStep: Unit = {
    consoleOpt.foreach(_.runFixpoint)
    // dequeue the next control point from the worklist queue
    val cp = worklist.pop
    // read the current state at the entrance of that control point
    val st = semantics.getState(cp)
    // compute the state at the exit of that control point
    val (nextSt, nextExcSt) = semantics.C(cp, st)

    // propagate the exit state across normal CFG edges (`CFGEdgeNormal`)
    propagateNormal(cp, nextSt)
    // propagate the exit exceptional state across exceptional CFG edges (`CFGEdgeExc`)
    propagateException(cp, nextExcSt)
    // propagate the exit state across interprocedural edges (`CFGEdgeCall` and `CFGEdgeReturn`)
    propagateInterProc(cp, nextSt)
  }

  // propagate the exit state `nextSt` from the control point `cp` across normal edges.
  def propagateNormal(cp: ControlPoint, nextSt: AbsState): Unit = {
    // compute the list of blocks which are successors to `cp` across normal edges.
    cp.block.getSucc(CFGEdgeNormal) match {
      // if there aren't any such blocks, do nothing.
      case Nil => ()
      // otherwise, for each such successor `block` across a normal edge:
      case lst => lst.foreach(block => {
        // compute control point successors across the normal edge to `block`.
        // (note that each such control point will have its block be `block`,
        // but we may have multiple control points if there are multiple
        // successor trace partition tokens across the edge.)
        cp.next(block, CFGEdgeNormal, semantics, nextSt).foreach(succCP => {
          // for each successor control point, compare its current state (`oldSt`)
          // to the current exit state of its predecessor (`nextSt`).
          val oldSt = semantics.getState(succCP)
          // if the next state contains new information relative to the old state,
          if (!(nextSt ⊑ oldSt)) {
            // join the next state with the old state and set the result as the new state
            // at this control point.
            val newSt = oldSt ⊔ nextSt
            semantics.setState(succCP, newSt)
            worklist.add(succCP)
          }
        })
      })
    }
  }

  // very similar to `propagateNormal` above, except now we're propagating an exit
  // exception state across exception edges.
  def propagateException(cp: ControlPoint, nextExcSt: AbsState): Unit = {
    // Propagate exception output state (outES) along exception edges.
    // 1) If successor is catch, current exception value is assigned to catch variable and
    //    previous exception values are restored.
    // 2) If successor is finally, current exception value is propagated further along
    //    finally block's "normal" edges.
    cp.block.getSucc(CFGEdgeExc) match {
      case Nil => ()
      case lst => lst.foreach(block => {
        cp.next(block, CFGEdgeExc, semantics, nextExcSt).foreach(excSuccCP => {
          val oldExcSt = semantics.getState(excSuccCP)
          if (!(nextExcSt ⊑ oldExcSt)) {
            val newExcSet = oldExcSt ⊔ nextExcSt
            semantics.setState(excSuccCP, newExcSet)
            worklist.add(excSuccCP)
          }
        })
      })
    }
  }

  def propagateInterProc(cp: ControlPoint, nextSt: AbsState): Unit = {
    // Propagate along inter-procedural edges
    // This step must be performed after evaluating abstract transfer function
    // because 'call' instruction can add inter-procedural edges.

    // retrieve all interprocedural edges starting from `cp`.
    semantics.getInterProcSucc(cp) match {
      case None => ()
      case Some(succMap) => {
        succMap.foreach {
          // for each interprocedural edge from `cp` to `succCP`:
          case (succCP, data) => {
            // read the current state at `succCP`
            val oldSt = semantics.getState(succCP)
            // compute the newly propagated state across the current interprocedural edge
            val nextSt2 = semantics.E(cp, succCP, data, nextSt)
            succCP.block match {
              case Entry(f) =>
                // if `succCP` is an entry block `Entry(f)`, then:
                //   - `cp` is a `Call` block, and
                //   - the edge from `cp` to `succCP` represents calling the function `f`.

                // in this case, add the control points corresponding to the exit points of `f` to the worklist queue.
                val tp = succCP.tracePartition
                val exitCP = ControlPoint(f.exit, tp)
                val exitExcCP = ControlPoint(f.exitExc, tp)
                if (!semantics.getState(exitCP).isBottom) worklist.add(exitCP)
                if (!semantics.getState(exitExcCP).isBottom) worklist.add(exitExcCP)
              case _ => ()
            }

            // join the successor's current state with its newly computed state.
            if (!(nextSt2 ⊑ oldSt)) {
              val newSt = oldSt ⊔ nextSt2
              semantics.setState(succCP, newSt)
              worklist.add(succCP)
            }
          }
        }
      }
    }
  }
}
