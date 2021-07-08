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

package kr.ac.kaist.safe.phase

import scala.util.{ Failure, Success, Try }
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.domain.{ Map, _ }
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.analyzer.TracePartition

// ReactBugDetect phase
case object ThisBugDetect extends PhaseObj[(CFG, Int, TracePartition, Semantics), ThisBugDetectConfig, CFG] {
  val name: String = "thisBugDetector"
  val help: String = "Detect possible misuse of the `this` keyword."

  // TODO
  // Generators of bug detector messages
  def always(expr: CFGExpr, cond: Boolean): String =
    expr.ir.span.toString + ":\n    [Warning] The conditional expression \"" + expr.ir.ast.toString(0) + "\" is always " + cond + "."
  def absentProp(expr: CFGExpr, name: AbsStr, obj: CFGExpr): String =
    expr.ir.span.toString + ":\n    [Warning] The property " + name + " of the object \"" + obj.ir.ast.toString(0) + "\" is absent."

  // TODO
  // Move to CFGBlock?  Copied from HTMLWriter.
  private def isReachableUserCode(sem: Semantics, block: CFGBlock): Boolean =
    !sem.getState(block).isEmpty && !NodeUtil.isModeled(block)

  // Collect CFG expressions from CFG instructions
  private def collectExprs(i: CFGNormalInst): List[CFGExpr] = i match {
    case CFGAlloc(_, _, _, Some(e), _) => List(e)
    case CFGEnterCode(_, _, _, e) => List(e)
    case CFGExprStmt(_, _, _, e) => List(e)
    case CFGDelete(_, _, _, e) => List(e)
    case CFGDeleteProp(_, _, _, e1, e2) => List(e1, e2)
    case CFGStore(_, _, e1, e2, e3) => List(e1, e2, e3)
    case CFGStoreStringIdx(_, _, e1, _, e2) => List(e1, e2)
    case CFGAssert(_, _, e, _) => List(e)
    case CFGReturn(_, _, Some(e)) => List(e)
    case CFGThrow(_, _, e) => List(e)
    case CFGInternalCall(_, _, _, _, es, _) => es
    case _ => Nil
  }

  private def checkReactExpr(expr: CFGExpr, state: AbsState, semantics: Semantics): List[String] = {
    checkExpr(expr, state, semantics)
  }

  // Check expression-level rules: AbsentPropertyRead
  private def checkExpr(expr: CFGExpr, state: AbsState,
    semantics: Semantics): List[String] = expr match {
    // Don't check if this instruction is "LHS = <>fun<>["prototype"]".
    case CFGLoad(_, CFGVarRef(_, CFGTempId(name, _)),
      CFGVal(EJSString("prototype"))) if name.startsWith("<>fun<>") =>
      List[String]()
    case CFGLoad(_, obj, index) =>
      val (objV, _) = semantics.V(obj, state)
      val (propV, _) = semantics.V(index, state)
      // Check for each object location
      objV.locset.foldLeft(List[String]())((bugs, objLoc) => {
        if (!propV.isBottom && !propV.pvalue.strval.isBottom) {
          val propStr = propV.pvalue.strval
          val heap = state.heap
          val propExist = heap.get(objLoc).HasProperty(propStr, heap)
          if (!propExist.isBottom && propExist ⊑ AbsBool.False)
            absentProp(expr, propStr, obj) :: bugs
          else bugs
        } else bugs
      })
    case _ => List[String]()
  }

  private def blockUsesThis(block: CFGBlock): Bool = {
    // for each instruction,
    block.getInsts.exists {
      case i: CFGNormalInst => {
        // for each expression in the instruction,
        collectExprs(i).exists {
          // check if that instruction contains the `this` expression
          case _: CFGThis => true
          case _ => false
        }
      }
      case _ => false
    }
  }

  var funcsReferencingThis = Set[FunctionId]()
  private def cacheFunctionsUsingThis(cfg: CFG): Unit = {
    funcsReferencingThis = cfg.getUserFuncs.foldLeft(Set[FunctionId]())((result, func) => {
      val funcReferencesThis = func.getAllBlocks.exists(blockUsesThis)
      if (funcReferencesThis) result + func.id else result
    })
  }

  private def calleeFidsOfFunction(semantics: Semantics, st: AbsState, func: CFGFunction): List[FunctionId] = {
    // for each block in the function body
    func.getAllBlocks.foldLeft(List[FunctionId]())((res, block) => {
      // for each instruction in the block
      block.getInsts.foldLeft(res)((res, inst) => {
        inst match {
          // if the instruction is a call, record the fidset of the callee
          case i: CFGCallInst => {
            val (funV, excSet) = semantics.V(i.fun, st)
            println("funV: " + funV)
            println("fidset: " + funV.fidset)
            res
          }
          case _ => res
        }
      })
    })
  }

  private def functionName(cfg: CFG, st: AbsState, v: AbsValue): String = {
    val names = v.locset.foldLeft(List[String]())((res, loc) => {
      res ++ st.heap.get(loc)(ICall).fidset.foldLeft(List[String]())((res, fid) => {
        cfg.getFunc(fid) match {
          case Some(func) => func.simpleName :: res
          case None => res
        }
      })
    })

    names.mkString(",")
  }

  private def isAnonymousFunction(mFunc: Option[CFGFunction]): Bool = {
    mFunc match {
      case Some(func) => func.simpleName.startsWith("funexpr@")
      case None => false
    }
  }

  // computes whether `obj` is an unbound, named function that uses `this`
  private def isUnboundFunctionUsingThis(cfg: CFG, st: AbsState, obj: AbsObj): Bool = {
    val isNamed = obj(ICall).fidset.map(cfg.getFunc).foldLeft(false)(
      (res, func) => res || !isAnonymousFunction(func)
    )

    val isFunction = obj(IClass).toString == "\"Function\""
    val isUnbound = obj(IBoundThis).isBottom
    val refsThis = obj(ICall).fidset.exists(funcsReferencingThis.contains)

    isNamed && isFunction && isUnbound && refsThis
  }

  private def funcValFids(st: AbsState, funcVal: AbsValue): Set[FunctionId] = {
    funcVal.locset.map(st.heap.get)
      .foldLeft(Set[FunctionId]())((fids, calleeObj) => {
        calleeObj(ICall).fidset.foldLeft(fids)((fids, fid) => fids + fid)
      })
  }

  private def fidsCalledInFunction(semantics: Semantics, func: CFGFunction): Set[FunctionId] = {
    func.getAllBlocks.foldLeft(Set[FunctionId]())((fids, block) => {
      fids ++ semantics.getState(block).foldLeft(Set[FunctionId]())((blockFids, pair) => {
        val (tp, st) = pair
        block match {
          case callBlock: Call => {
            val (calleeVal, _) = semantics.V(callBlock.callInst.fun, st)
            blockFids ++ funcValFids(st, calleeVal)
          }
          case _ => blockFids
        }
      })
    })
  }

  // computes the set of `FunctionId` values called by the `AbsValue` at `calleeV`, which is treated as a function.
  private def fidsCalledByFuncVal(cfg: CFG, semantics: Semantics, st: AbsState, calleeV: AbsValue): Set[FunctionId] = {
    funcValFids(st, calleeV).foldLeft(Set[FunctionId]())((calledFids, calleeFid) => {
      cfg.getFunc(calleeFid) match {
        case None => calledFids
        case Some(func) => {
          calledFids ++ fidsCalledInFunction(semantics, func)
        }
      }
    })
  }

  // performs a left fold over the integer-valued keys of an `AbsValue`.
  // this is meant to iterate over the usual array values of an array object.
  private def foldOverIntKeys[T](st: AbsState, arrV: AbsValue, init: T, cb: (T, Int, (AbsDesc, AbsUndef)) => T): T = {
    arrV.locset.map(st.heap.get)
      .foldLeft(init)((result, arrObj) => {
        var locResult = result
        arrObj.nmap.map.foldLeft(result)((locResult, pair) => {
          val (key, optV) = pair
          Try(key.toInt).toOption match {
            case Some(i) => cb(locResult, i, arrObj.GetOwnProperty(AbsStr(i.toString)))
            case None => locResult
          }
        })
      })
  }

  private def locName(loc: Loc): String = {
    loc match {
      case TraceSensLoc(innerLoc, tp) => locName(innerLoc)
      case UserAllocSite(id) => id.toString
      case PredAllocSite(name) => name
      case _ => "unknown"
    }
  }

  private val arrayIteratorNames: List[String] = List[String](
    "Array.prototype.map",
    "Array.prototype.filter",
    "Array.prototype.forEach"
  )

  private def calleeIsArrayIterator(st: AbsState, calleeV: AbsValue): Boolean = {
    calleeV.locset.exists(loc => {
      arrayIteratorNames.contains(locName(loc))
    })
  }

  private val eventListenerNames: List[String] = List[String](
    "EventTarget.prototype.addEventListener"
  )

  private def calleeIsEventListener(calleeV: AbsValue): Boolean = {
    calleeV.locset.exists(loc => {
      eventListenerNames.contains(locName(loc))
    })
  }

  private val createElementName: String = "React.createElement"

  private def calleeIsCreateElement(calleeV: AbsValue): Boolean = {
    calleeV.locset.exists(loc => locName(loc) == createElementName)
  }

  // checks the call for an instance of the "unbound context" bug.
  // in this bug:
  //   - an argument to the callee is a function `f`
  //   - `f` has an unbound `this` value
  //   - `f` uses `this` in its body
  //   - the callee calls `f`.
  // it's likely that `f` is being called with an incorrect context when the above holds.
  private def checkCallBlockForUnboundArg(cfg: CFG, semantics: Semantics, callBlock: Call): List[String] = {
    semantics.getState(callBlock).foldLeft(List[String]())((result, pair) => {
      val (tp, st) = pair
      val (calleeV, excSetO) = semantics.V(callBlock.callInst.fun, st)
      val args = callBlock.callInst.arguments
      val (argsV, excSet1) = semantics.V(args, st)

      val fidsCalledByCallee = fidsCalledByFuncVal(cfg, semantics, st, calleeV)
      val isArrayIterator = calleeIsArrayIterator(st, calleeV)
      val isEventListener = calleeIsEventListener(calleeV)
      val isCreateElement = calleeIsCreateElement(calleeV)

      val lineNum = callBlock.callInst.ir.line
      //println(f"Call block: callee=$calleeV, isArrayIterator=$isArrayIterator, isCreateElt=$isCreateElement")

      result ++ foldOverIntKeys(st, argsV, List[String](), (result: List[String], i: Int, pair) => {
        //println(f"folding over key i=$i, argsV=$argsV")
        val (argDesc, argUndef) = pair
        val (argV, _) = argDesc.value
        //println(s"argV.locset=${argV.locset}")
        argV.locset.foldLeft(result)((result, argLoc) => {
          val argObj = st.heap.get(argLoc)
          // check if the argument is an unbound function that should generate a warning
          val argCalledByCallee = (isArrayIterator && i == 0) || funcValFids(st, argV).intersect(fidsCalledByCallee).nonEmpty
          val isUnbound = isUnboundFunctionUsingThis(cfg, st, argObj)

          //println("func val fids: " + funcValFids(st, argV))
          //println("fids called by callee: " + fidsCalledByCallee)
          //println(f"isUnbound=$isUnbound, argCalledByCallee=$argCalledByCallee")
          if (isEventListener && i == 1) {
            val fnName = functionName(cfg, st, calleeV)
            val msg = s"Warning (line $lineNum): Unbound function added as event listener."
            msg :: result
          } else if (isCreateElement && i == 1) {
            argObj.nmap.map.foldLeft(result)((result, keyValue) => {
              val (k, v) = keyValue
              val vLocs = v.value.value.locset
              val unboundLocs = vLocs.filter(loc => isUnboundFunctionUsingThis(cfg, st, st.heap.get(loc)))
              val errors = unboundLocs.map(loc => {
                s"Warning (line $lineNum): Unbound function passed as prop '$k'."
              }).toList
              // generate an error message
              //              if (isUnboundFunctionUsingThis(cfg, st, v))
              errors ++ result
            })
          } else if (isUnbound && argCalledByCallee) {
            // generate an error message
            val fnName = functionName(cfg, st, calleeV)
            val msg = s"Warning (line $lineNum): Unbound function passed as argument $i to $fnName."
            msg :: result
          } else {
            result
          }
        })
      })
    })
  }

  private def checkCallBlock(cfg: CFG, semantics: Semantics, callBlock: Call): List[String] = {
    checkCallBlockForUnboundArg(cfg, semantics, callBlock)
  }

  // Check block/instruction-level rules: ConditionalBranch
  private def checkBlock(cfg: CFG, semantics: Semantics, block: CFGBlock): List[String] = {
    block match {
      case callBlock: Call => checkCallBlock(cfg, semantics, callBlock)
      case _ => List[String]()
    }
  }

  def apply(
    in: (CFG, Int, TracePartition, Semantics),
    safeConfig: SafeConfig,
    config: ThisBugDetectConfig
  ): Try[CFG] = {
    val (cfg, _, _, semantics) = in

    // compute the functions using the `this` keyword
    cacheFunctionsUsingThis(cfg)

    val result = cfg.getUserBlocks.foldRight(List[String]())((b, r) => checkBlock(cfg, semantics, b) ++ r)
    if (result.length > 0) {
      result.distinct.reverse.foreach(println)
    } else {
      println("No warnings.")
    }

    Success(cfg)
  }

  def defaultConfig: ThisBugDetectConfig = ThisBugDetectConfig()
  val options: List[PhaseOption[ThisBugDetectConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during bug detection are muted.")
  )
}

case class ThisBugDetectConfig(
  var silent: Boolean = false
) extends Config
