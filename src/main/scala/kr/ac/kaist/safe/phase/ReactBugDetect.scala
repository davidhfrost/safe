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
import kr.ac.kaist.safe.analyzer.model.GLOBAL_LOC
import kr.ac.kaist.safe.nodes.ast.{ ASTWalker, ClassDeclaration, ClassMethod, FromImportDeclaration, Id, ImportClause, ModuleImportDeclaration, Stmt, VarRef }
import kr.ac.kaist.safe.nodes.ir.IRRoot

object SetStateBugDetect {
  // walks an AST and maintains a list of `ClassDeclaration` nodes which extend `Component`.
  private object ReactComponentWalker extends ASTWalker {
    var reactComponents: List[ClassDeclaration] = List()
    override def walk(node: Stmt): Stmt = node match {
      // check if the superclass of a `ClassDeclaration` node is `Component`.
      case classDecl @ ClassDeclaration(_, _, Some(VarRef(_, id)), _) if id.text == "Component" =>
        reactComponents = classDecl :: reactComponents
        super.walk(node)

      case _ => super.walk(node)
    }
  }

  private def checkMethod(cfg: CFG, semantics: Semantics, className: String, method: ClassMethod): List[String] = {
    List(s"className: ${className}, method: ${method.ftn.name.text}")
  }

  private def checkComponent(cfg: CFG, semantics: Semantics, classDecl: ClassDeclaration): List[String] = {
    // combine the warnings found for each method declared by the class
    classDecl.methods.foldLeft(List[String]())((warnings, method) => {
      warnings ++ checkMethod(cfg, semantics, classDecl.name.text, method)
    })
  }

  def runDetector(cfg: CFG, semantics: Semantics): List[String] = {
    val irRoot = cfg.ir.asInstanceOf[IRRoot]
    // compute the list of react components declared in the input program
    ReactComponentWalker.walk(irRoot.inputAST.get)
    val components = ReactComponentWalker.reactComponents

    components.foldLeft(List[String]())((warnings, classDecl) => {
      warnings ++ checkComponent(cfg, semantics, classDecl)
    })
  }
}

// ReactBugDetect phase
case object ReactBugDetect extends PhaseObj[(CFG, Int, TracePartition, Semantics), ReactBugDetectConfig, CFG] {
  val name: String = "reactBugDetector"
  val help: String = "Detect bugs in React applications."

  private def computeThisDotStateLocs(state: AbsState): Set[Loc] = {
    // compute all possible heap locations of `this.state`.
    var thisDotStateLocs = Set[Loc]()

    // read the value of `this` from the state.
    val thisValue = state.context.thisBinding

    // abstract value:
    // - a list of possible literal values (3, 7, 'asdf', true/false)
    // - a list of possible heap locations

    // for each possible heap location of `thisValue`, get the heap object at that location.
    thisValue.locset.foreach(thisLoc => {
      // get the object at location `thisLoc` in the heap.
      val thisObj = state.heap.get(thisLoc)

      // read the `state` property at that object.
      thisObj.nmap.map.get("state") match {
        // if `this.state` is defined:
        case Some(thisDotState) =>
          // read out the underlying abstract value from the data recorded in `thisObj`.
          val thisDotStateValue = thisDotState.value.value

          // add all of its possible locations to `thisDotStateLocs`.
          thisDotStateValue.locset.foreach(loc => thisDotStateLocs += loc)

        // if `this.state` is not defined, do nothing.
        case None => ()
      }
    })

    thisDotStateLocs
  }

  // returns a list of `String` warnings to print which declare the bugs found in `block`.
  private def checkBlock(cfg: CFG, semantics: Semantics, block: CFGBlock): List[String] = {
    var warnings = List[String]()

    // iterate over all possible program states when the block starts.
    // each block can have many starting states, indexed by a `TracePartition` value.
    // so for a given block, all states can be described as a list of pairs: `(TracePartition, AbsState)`.
    semantics.getState(block).foreach {
      case (tp, initialBlockState) => {
        val cp = ControlPoint(block, tp)
        var state = initialBlockState

        val thisDotStateLocs = computeThisDotStateLocs(state)
        if (thisDotStateLocs.nonEmpty) {
          //          println("thisDotStateLocs: " + thisDotStateLocs)
          //          println("block: " + block.toString(0))
        }

        // block instructions are stored in reverse order (for some reason),
        // so we need to re-reverse the instruction list to get the original order.
        block.getInsts.reverse.foreach {
          case i: CFGNormalInst =>
            // apply this instruction to the program state, producing a new state `nextState`.
            val (nextState, _) = semantics.I(cp, i, state, AbsState.Bot)
            state = nextState

            i match {
              // obj[index] = rhs
              case CFGStore(ir, block, obj, index, rhs) =>
                // figure out if `obj` coincides with `this.state`?
                // in order to check if they coincide, we need an `AbsValue` for obj
                val (objValue, exceptionSet) = semantics.V(obj, state)
                val objLocs = objValue.locset
                //                println("objLocs: " + objLocs)

                objLocs.foreach(objLoc => {
                  if (thisDotStateLocs.contains(objLoc)) {
                    warnings ++= List("found bug at: " + obj.ir.ast.info)
                  }
                })

              case _ => ()
            }

          case _ => ()
        }
      }
    }

    warnings
  }

  def apply(
    in: (CFG, Int, TracePartition, Semantics),
    safeConfig: SafeConfig,
    config: ReactBugDetectConfig
  ): Try[CFG] = {
    val (cfg, _, _, semantics) = in

    val result = cfg.getUserBlocks.foldRight(List[String]())((b, r) => checkBlock(cfg, semantics, b) ++ r)
    if (result.length > 0) {
      result.distinct.reverse.foreach(println)
    } else {
      println("No warnings.")
    }

    println("=== set state bug detector output ===")
    SetStateBugDetect.runDetector(cfg, semantics).foreach(println)

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
