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

import kr.ac.kaist.safe.{ SafeConfig, CmdCFGBuild }
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.model._
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.util.NodeUtil
import kr.ac.kaist.safe.phase._

object Initialize {
  def apply(cfg: CFG, initHeap: Option[AbsHeap]): AbsState = {
    val globalLocSet = LocSet(GLOBAL_LOC)
    val globalPureLocalEnv = AbsLexEnv.newPureLocal(globalLocSet)

    val initCtx = AbsContext(Map(
      GLOBAL_ENV -> AbsLexEnv(AbsGlobalEnvRec.Top),
      PURE_LOCAL -> globalPureLocalEnv,
      COLLAPSED -> AbsLexEnv(AbsDecEnvRec.Empty)
    ), LocSet.Bot, globalLocSet)

    val modeledHeap: AbsHeap = {
      val model = HeapBuild.jscache getOrElse {
        Model.parseDir(NodeUtil.jsModelsBase)
      }
      model.funcs.foreach {
        case (_, func) => {
          cfg.addJSModel(func)
        }
      }
      AbsHeap(model.heap)
    }

    val heap = initHeap match {
      // the global object is the only heap location which should be
      // potentially shared between the default initial heap any user-defined initial heap.
      // we *only* want to use the modeled heap's global object, and keep other locations the same.
      case Some(h) => h.update(GLOBAL_LOC, modeledHeap.get(GLOBAL_LOC))
      case None => modeledHeap
    }

    AbsState(initHeap.getOrElse(modeledHeap), initCtx, AllocLocSet.Empty)
  }

  def addSnapshot(st: AbsState, snapshot: String): AbsState = {
    val concreteHeap = Heap.parse(snapshot)
    val abstractHeap = AbsHeap.alpha(concreteHeap)
    st.copy(heap = st.heap ⊔ abstractHeap)
  }
}
