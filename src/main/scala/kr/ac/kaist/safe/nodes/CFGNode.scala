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

package kr.ac.kaist.safe.nodes

import scala.collection.mutable.{ HashMap => MHashMap, Map => MMap }
import kr.ac.kaist.safe.analyzer.domain.Address
import kr.ac.kaist.safe.config.Config
import kr.ac.kaist.safe.util.{ NodeUtil, EJSOp, Span, SourceLoc }

sealed abstract class CFGNode(val ir: IRNode)
    extends Node {
  override def toString(indent: Int): String = " " * indent + this
  def span: Span = ir.span
  def comment: Option[Comment] = ir.comment
  def fileName: String = ir.fileName
  def begin: SourceLoc = ir.begin
  def end: SourceLoc = ir.end
  def line: Int = ir.line
  def offset: Int = ir.offset
}

////////////////////////////////////////////////////////////////////////////////
// CFG
////////////////////////////////////////////////////////////////////////////////

class CFG(
    override val ir: IRNode,
    globalVars: List[CFGId]
) extends CFGNode(ir) {
  // all functions in this cfg
  private var userFuncs: List[CFGFunction] = Nil
  private var modelFuncs: List[CFGFunction] = Nil
  def getUserFuncs: List[CFGFunction] = userFuncs
  def getModelFuncs: List[CFGFunction] = modelFuncs

  // TODO: delete this after refactoring dump
  // all blocks in this cfg
  private var blocks: List[CFGNormalBlock] = Nil
  def addNode(block: CFGNormalBlock): Unit = blocks ::= block

  // create function
  def createFunction(argumentsName: String, argVars: List[CFGId], localVars: List[CFGId],
    name: String, ir: IRNode, body: String, isUser: Boolean): CFGFunction = {
    val func: CFGFunction =
      CFGFunction(ir, this, argumentsName, argVars, localVars, name, body, isUser)
    funMap(func.id) = func
    isUser match {
      case true => userFuncs ::= func
      case false => modelFuncs ::= func
    }
    return func
  }

  // add edge
  def addEdge(fromList: List[CFGBlock], toList: List[CFGBlock], etype: CFGEdgeType = CFGEdgeNormal): Unit = {
    fromList.foreach((from) => toList.foreach((to) => {
      from.addSucc(etype, to)
      to.addPred(etype, from)
    }))
  }

  // dump cfg
  def dump: String = {
    var str: String = ""
    for (block <- blocks) str += block.dump
    str
  }

  // init id counter
  CFGFunction.resetId
  CFGNormalBlock.resetId
  CFGInst.resetId

  // function / block map from id
  val funMap: MMap[FunctionId, CFGFunction] = MHashMap()

  // global function
  val globalFunc: CFGFunction = createFunction("", Nil, globalVars, "top-level", ir, "", true)
}

////////////////////////////////////////////////////////////////////////////////
// CFG Function
////////////////////////////////////////////////////////////////////////////////

case class CFGFunction(
    override val ir: IRNode,
    cfg: CFG,
    argumentsName: String,
    argVars: List[CFGId],
    localVars: List[CFGId],
    name: String,
    body: String,
    isUser: Boolean
) extends CFGNode(ir) {
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

////////////////////////////////////////////////////////////////////////////////
// CFG Block
////////////////////////////////////////////////////////////////////////////////

sealed abstract class CFGBlock {
  val func: CFGFunction

  // edges incident with this cfg node
  protected val succs: MMap[CFGEdgeType, List[CFGBlock]] = MHashMap()
  protected val preds: MMap[CFGEdgeType, List[CFGBlock]] = MHashMap()
  def getSucc(edgeType: CFGEdgeType): List[CFGBlock] = succs.getOrElse(edgeType, Nil)
  def getPred(edgeType: CFGEdgeType): List[CFGBlock] = preds.getOrElse(edgeType, Nil)

  // add edge
  def addSucc(edgeType: CFGEdgeType, node: CFGBlock): Unit = succs(edgeType) = node :: succs.getOrElse(edgeType, Nil)
  def addPred(edgeType: CFGEdgeType, node: CFGBlock): Unit = preds(edgeType) = node :: preds.getOrElse(edgeType, Nil)
}
object CFGBlock {
  implicit def node2nodelist(node: CFGBlock): List[CFGBlock] = List(node)
}

// entry, exit, exception exit
case class Entry(func: CFGFunction) extends CFGBlock
case class Exit(func: CFGFunction) extends CFGBlock
case class ExitExc(func: CFGFunction) extends CFGBlock

// call, after-call, after-catch
case class Call(func: CFGFunction) extends CFGBlock {
  private var iAfterCall: AfterCall = _
  private var iAfterCatch: AfterCatch = _
  private var iCallInst: CFGCallInst = _
  def afterCall: AfterCall = iAfterCall
  def afterCatch: AfterCatch = iAfterCatch
  def callInst: CFGCallInst = iCallInst
}
object Call {
  def apply(func: CFGFunction, callInstCons: Call => CFGCallInst, retVar: CFGId): Call = {
    val call = Call(func)
    call.iAfterCall = AfterCall(func, retVar, call)
    call.iAfterCatch = AfterCatch(func, call)
    call.iCallInst = callInstCons(call)
    call
  }
}
case class AfterCall(func: CFGFunction, retVar: CFGId, call: Call) extends CFGBlock
case class AfterCatch(func: CFGFunction, call: Call) extends CFGBlock

// normal block
case class CFGNormalBlock(func: CFGFunction) extends CFGBlock {
  val id: BlockId = CFGNormalBlock.getId

  // inst list
  private var insts: List[CFGNormalInst] = Nil
  def getInsts: List[CFGNormalInst] = insts

  // create inst
  def createInst(instCons: CFGNormalBlock => CFGNormalInst): CFGNormalInst = {
    val inst: CFGNormalInst = instCons(this)
    insts ::= inst
    inst
  }

  // equals
  override def equals(other: Any): Boolean = other match {
    case (block: CFGNormalBlock) => (block.id == id)
    case _ => false
  }

  // toString
  override def toString: String = s"(${func.id},LBlock($id))"

  // dump node
  def dump: String = {
    var str: String = s"$this" + Config.LINE_SEP
    str += (preds.get(CFGEdgeNormal) match {
      case Some(List(AfterCall(_, retVar, _))) => s"    [EDGE] after-call($retVar)" + Config.LINE_SEP
      case _ => ""
    })
    insts.length > Config.MAX_INST_PRINT_SIZE match {
      case true => str + "    A LOT!!! " + ((succs.get(CFGEdgeNormal) match {
        case Some(List(call: Call)) => 1
        case _ => 0
      }) + insts.length) + " instructions are not printed here." + Config.LINE_SEP + Config.LINE_SEP
      case false =>
        insts.reverseIterator.foldLeft(str) {
          case (s, inst) => s + s"    [${inst.id}] $inst" + Config.LINE_SEP
        } + (succs.get(CFGEdgeNormal) match {
          case Some(List(call: Call)) =>
            val inst = call.callInst
            s"    [${inst.id}] $inst" + Config.LINE_SEP
          case _ => ""
        }) + Config.LINE_SEP + Config.LINE_SEP
    }
  }
}
object CFGNormalBlock {
  private var counter: Int = 0
  private def getId: Int = { counter += 1; counter - 1 }
  def resetId: Unit = counter = 0
}

////////////////////////////////////////////////////////////////////////////////
// CFG Block
////////////////////////////////////////////////////////////////////////////////

// edge type for cfg nodes
sealed abstract class CFGEdgeType
case object CFGEdgeNormal extends CFGEdgeType // normal edges
case object CFGEdgeExc extends CFGEdgeType // exception edges
case object CFGEdgeLoopCond extends CFGEdgeType // loop condition edges
case object CFGEdgeLoop extends CFGEdgeType // loop edges
case object CFGEdgeLoopIter extends CFGEdgeType // loop iteration edges
case object CFGEdgeLoopOut extends CFGEdgeType // loop out edges
case object CFGEdgeLoopBreak extends CFGEdgeType // loop break edges
case object CFGEdgeLoopReturn extends CFGEdgeType // looop return edges

////////////////////////////////////////////////////////////////////////////////
// CFG Instruction
////////////////////////////////////////////////////////////////////////////////

sealed abstract class CFGInst(
    override val ir: IRNode,
    block: CFGBlock
) extends CFGNode(ir) {
  val id: InstId = CFGInst.getId
}
object CFGInst {
  private var counter: Int = 0
  private def getId: Int = { counter += 1; counter - 1 }
  def resetId: Unit = counter = 0
}

/**
 * CFG Normal Instruction
 */
sealed abstract class CFGNormalInst(
  override val ir: IRNode,
  val block: CFGNormalBlock
) extends CFGInst(ir, block)

// x := alloc(e^?)
case class CFGAlloc(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    lhs: CFGId,
    protoOpt: Option[CFGExpr],
    addr: Address
) extends CFGNormalInst(ir, block) {
  override def toString: String = {
    val proto = protoOpt.getOrElse("")
    s"$lhs := alloc($proto) @ #$addr"
  }
}

// x := allocArray(n)
case class CFGAllocArray(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    lhs: CFGId,
    length: Int,
    addr: Address
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"$lhs := allocArray($length) @ #$addr"
}

// x := allocArg(n)
case class CFGAllocArg(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    lhs: CFGId,
    length: Int,
    addr: Address
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"$lhs := allocArg($length) @ #$addr"
}

// x := e
case class CFGExprStmt(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    lhs: CFGId, right: CFGExpr
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"$lhs := $right"
}

// x := delete(e)
case class CFGDelete(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    lhs: CFGId,
    expr: CFGExpr
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"$lhs := delete($expr)"
}

// x := delete(e1, e2)
case class CFGDeleteProp(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    lhs: CFGId,
    obj: CFGExpr,
    index: CFGExpr
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"$lhs := delete($obj, $index)"
}

// e1[e2] := e3
case class CFGStore(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    obj: CFGExpr,
    index: CFGExpr,
    rhs: CFGExpr
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"$obj[$index] := $rhs"
}

// x1 := function x_2^?(f)
case class CFGFunExpr(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    lhs: CFGId,
    nameOpt: Option[CFGId],
    func: CFGFunction,
    addr1: Address,
    addr2: Address,
    addr3Opt: Option[Address]
) extends CFGNormalInst(ir, block) {
  override def toString: String = {
    val name = nameOpt.getOrElse("")
    s"$lhs := function $name(${func.id}) @ #$addr1, #$addr2" + (addr3Opt match {
      case Some(addr) => s", #$addr"
      case None => ""
    })
  }
}

// assert(e1 x e2)
case class CFGAssert(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    expr: CFGExpr,
    flag: Boolean
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"assert($expr)"
}

// cond(x)
case class CFGCond(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    expr: CFGExpr,
    isEvent: Boolean = false
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"cond($expr)"
}

// catch(x)
case class CFGCatch(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    name: CFGId
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"catch($name)"
}

// return(e^?)
case class CFGReturn(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    exprOpt: Option[CFGExpr]
) extends CFGNormalInst(ir, block) {
  override def toString: String = {
    val expr = exprOpt.getOrElse("")
    s"return($expr)"
  }
}

// throw(e)
case class CFGThrow(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    expr: CFGExpr
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"throw($expr)"
}

// noop
case class CFGNoOp(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    desc: String
) extends CFGNormalInst(ir, block) {
  override def toString: String = s"noop($desc)"
}

// x := <>x(x^*)
case class CFGInternalCall(
    override val ir: IRNode,
    override val block: CFGNormalBlock,
    lhs: CFGId,
    fun: CFGId,
    arguments: List[CFGExpr],
    addrOpt: Option[Address]
) extends CFGNormalInst(ir, block) {
  override def toString: String = {
    val arg = arguments.mkString(", ")
    s"$lhs := $fun($arg)"
  } + (addrOpt match {
    case Some(addr) => s" @ #$addr"
    case None => ""
  })
}

// TODO revert after modeling
// case class CFGAPICall(
//   override val ir: IRNode,
//   override val block: CFGNormalBlock,
//   model: String,
//   fun: String,
//   arguments: CFGExpr
// ) extends CFGNormalInst(ir, block){
//   override def toString: String = s"[]$model.$fun($arguments)"
// }
// 
// case class CFGAsyncCall(
//   override val ir: IRNode,
//   override val block: CFGNormalBlock,
//   modelType: String,
//   callType: String,
//   addr1: Address,
//   addr2: Address,
//   addr3: Address
// ) extends CFGNormalInst(ir, block){
//   override def toString: String = s"async($modelType, $callType) @ #$addr1, #$addr2, #$addr3"
// }

/**
 * CFG Call Instruction
 */
sealed abstract class CFGCallInst(
  override val ir: IRNode,
  val block: Call
) extends CFGInst(ir, block)

// call(e1, e2, e3)
case class CFGCall(
    override val ir: IRNode,
    override val block: Call,
    fun: CFGExpr,
    thisArg: CFGExpr,
    arguments: CFGExpr,
    addr1: Address,
    addr2: Address
) extends CFGCallInst(ir, block) {
  override def toString: String = s"call($fun, $thisArg, $arguments) @ #$addr1"
}

// construct(e1, e2, e3)
case class CFGConstruct(
    override val ir: IRNode,
    override val block: Call,
    cons: CFGExpr,
    thisArg: CFGExpr,
    arguments: CFGExpr,
    addr1: Address,
    addr2: Address
) extends CFGCallInst(ir, block) {
  override def toString: String = s"construct($cons, $thisArg, $arguments) @ #$addr1, #$addr2"
}

////////////////////////////////////////////////////////////////////////////////
// CFG Expression
////////////////////////////////////////////////////////////////////////////////

trait CFGExpr

/**
 * Complex expression
 */
sealed abstract class CFGComplexExpr(
  override val ir: IRNode
) extends CFGNode(ir) with CFGExpr

// variable reference
case class CFGVarRef(
    override val ir: IRNode,
    id: CFGId
) extends CFGComplexExpr(ir) {
  override def toString: String = s"$id"
}

// load
case class CFGLoad(
    override val ir: IRNode,
    obj: CFGExpr,
    index: CFGExpr
) extends CFGComplexExpr(ir) {
  override def toString: String = s"$obj[$index]"
}

// this
case class CFGThis(
    override val ir: IRNode
) extends CFGComplexExpr(ir) {
  override def toString: String = "this"
}

/**
 * Simple expression
 */
sealed abstract class CFGSimpleExpr extends CFGExpr

// binary operation
case class CFGBin(
    first: CFGExpr,
    op: EJSOp,
    second: CFGExpr
) extends CFGSimpleExpr {
  override def toString: String = s"$first $op $second"
}

// unary operation
case class CFGUn(
    op: EJSOp,
    expr: CFGExpr
) extends CFGSimpleExpr {
  override def toString: String = s"$op $expr"
}

// number
case class CFGNumber(
    text: String,
    num: Double
) extends CFGSimpleExpr {
  override def toString: String = text
}

// string
case class CFGString(
    str: String
) extends CFGSimpleExpr {
  override def toString: String = "\"" + NodeUtil.pp(str) + "\""
}

// boolean
case class CFGBool(
    bool: Boolean
) extends CFGSimpleExpr {
  override def toString: String = if (bool) "true" else "false"
}

// null
case object CFGNull extends CFGSimpleExpr {
  override def toString: String = "null"
}

////////////////////////////////////////////////////////////////////////////////
// CFG Expression
////////////////////////////////////////////////////////////////////////////////

sealed abstract class CFGId(
  val text: String,
  val kind: VarKind
)

case class CFGUserId(
    override val text: String,
    override val kind: VarKind,
    originalName: String,
    fromWith: Boolean
) extends CFGId(text, kind) {
  override def toString: String = NodeUtil.pp(text)
}

case class CFGTempId(
    override val text: String,
    override val kind: VarKind
) extends CFGId(text, kind) {
  override def toString: String = NodeUtil.pp(text)
}

sealed abstract class VarKind
case object GlobalVar extends VarKind
case object PureLocalVar extends VarKind
case object CapturedVar extends VarKind
case object CapturedCatchVar extends VarKind
