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

package kr.ac.kaist.safe.cfg_builder

import kr.ac.kaist.safe.nodes.{ CFGCallInst, CFGId, IRNode }
import scala.collection.mutable.{ Map => MMap, HashMap => MHashMap }

case class CFGFunction(cfg: CFG, argumentsName: String, argVars: List[CFGId], localVars: List[CFGId], name: String, ir: IRNode, body: String, isUser: Boolean) {
  val id: FunctionId = CFGFunction.getId

  val entry = Entry(this)
  val exit = Exit(this)
  val exitExc = ExitExc(this)

  // create call
  def createCall(callInstCons: Call => CFGCallInst, retVar: CFGId): Call = {
    val call = Call(this, callInstCons, retVar)
    blocks = call :: call.afterCall :: call.afterCatch :: blocks
    call
  }

  // all blocks in this function
  private var blocks: List[CFGBlock] = List(entry, exit, exitExc)
  def getBlocks: List[CFGBlock] = blocks
  def createBlock: CFGNormalBlock = {
    val block = CFGNormalBlock(this)
    blocks ::= block
    cfg.addNode(block) // TODO delete this after refactoring dump
    block
  }

  // TODO: PureLocal - may not need to distinguish Captured
  // captured variable list for each function
  private var captured: List[CFGId] = Nil
  def addCaptured(captId: CFGId): Unit = captured ::= captId
  def getCaptured: List[CFGId] = captured

  // equals
  override def equals(other: Any): Boolean = other match {
    case func: CFGFunction => (func.id == id)
    case _ => false
  }
}

object CFGFunction {
  private var counter = 0
  private def getId: Int = { counter += 1; counter - 1 }
  def resetId: Unit = counter = 0
}
