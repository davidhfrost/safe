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

import scala.util.{Failure, Success, Try}
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.domain.{Map, _}
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.analyzer.TracePartition

// ReactBugDetect phase
case object ReactBugDetect extends PhaseObj[(CFG, Int, TracePartition, Semantics), ReactBugDetectConfig, CFG] {
  val name: String = "reactBugDetector"
  val help: String = "Detect possible bugs in React programs."

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
            println("fidsCalledInFunction calleeVal: " + calleeVal)
            blockFids ++ funcValFids(st, calleeVal)
          }
          case _ => blockFids
        }
      })
    })
  }

  private def checkCallBlock(cfg: CFG, semantics: Semantics, callBlock: Call): List[String] = {
    val func = callBlock.func
    val callee = callBlock.callInst.fun
    var result = List[String]()

    semantics.getState(callBlock).foreach(pair => {
      val (tp, st) = pair
      val (calleeV, excSetO) = semantics.V(callee, st)
      val args = callBlock.callInst.arguments
      val (argsV, excSet1) = semantics.V(args, st)

      val fidsCalledByCallee = funcValFids(st, calleeV).foldLeft(Set[FunctionId]())((calledFids, calleeFid) => {
        cfg.getFunc(calleeFid) match {
          case None => calledFids
          case Some(func) => {
            calledFids ++ fidsCalledInFunction(semantics, func)
          }
        }
      })
      // iterate over arguments object locations
      argsV.locset.map(st.heap.get)
        .foreach(argsObj => {
          // iterate over key-value pairs in the arguments object
          argsObj.nmap.map.foreach(pair => {
            val (key, optV) = pair
            // check if the key is an integer
            Try(key.toInt).toOption match {
              case Some(i) => {
                val (argDesc, _) = argsObj.GetOwnProperty(AbsStr(i.toString))
                val (argV, _) = argDesc.value
                argV.locset.foreach(argLoc => {
                  val argObj = st.heap.get(argLoc)
                  // check if the argument is an unbound function that should generate a warning
                  val argCalledByCallee = funcValFids(st, argV).intersect(fidsCalledByCallee).nonEmpty
                  println("func val fids: " + funcValFids(st, argV))
                  println("fids called by callee: " + fidsCalledByCallee)
                  if (isUnboundFunctionUsingThis(cfg, st, argObj) && argCalledByCallee) {
                    // generate an error message
                    val lineNum = callBlock.callInst.ir.line
                    val fnName = functionName(cfg, st, calleeV)
                    result = s"Warning (line $lineNum): Unbound function passed as argument $i to $fnName." :: result
                  }
                })
              }
              case None => {}
            }
          })
        })
    })

    result
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
     config: ReactBugDetectConfig
   ): Try[CFG] = {
    val (cfg, _, _, semantics) = in

    // compute the functions using the `this` keyword
    cacheFunctionsUsingThis(cfg)


    val result = cfg.getUserBlocks.foldRight(List[String]())((b, r) => checkBlock(cfg, semantics, b) ++ r)
    result.reverse.foreach(println)

    Success(cfg)
  }

  def defaultConfig: ReactBugDetectConfig = ReactBugDetectConfig()
  val options: List[PhaseOption[ReactBugDetectConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during bug detection are muted.")
  )
}

case class ReactBugDetectConfig(
  var silent: Boolean = false
) extends Config
