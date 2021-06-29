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

import javax.script.ScriptEngineManager
import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.model._
import kr.ac.kaist.safe.nodes.ir._
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.util.{ AllocSite, EJSBool, EJSNull, EJSNumber, EJSString, EJSUndef, NodeUtil, Old, PredAllocSite, Recency, Recent, TraceSensLoc, Useful, UserAllocSite }
import kr.ac.kaist.safe.{ CmdAnalyze, CmdCFGBuild, CmdTranslate, LINE_SEP, SafeConfig }
import kr.ac.kaist.safe.analyzer.domain.react.{ CompDesc, ReactHelper, ReactState }
import kr.ac.kaist.safe.phase.{ Analyze, AnalyzeConfig, CFGBuild, HeapBuild, HeapBuildConfig }

import java.io.File
import java.nio.file.{ Path, Paths }
import scala.util.{ Success, Try }
import scala.collection.mutable.{ Map => MMap }
import scala.util.{ Failure, Success }

case class Semantics(
    cfg: CFG,
    worklist: Worklist,
    safeConfig: SafeConfig,
    heapBuildConfig: HeapBuildConfig
) {
  lazy val engine = new ScriptEngineManager().getEngineByMimeType("text/javascript")
  def init: Unit = {
    val entry = cfg.globalFunc.entry
    val entryCP = ControlPoint(entry, getState(entry).head match { case (tp, _) => tp })
    val initSt = getState(entryCP)
    cpToState.clear
    setState(entryCP, initSt)
    worklist.init(entryCP)
  }

  // exception log
  lazy val excLog: ExcLog = new ExcLog

  // abstract boolean
  private val AB = AbsBool.Bot

  // analyses of imported files
  type AnalysisData = (CFG, Int, TracePartition, Semantics)
  // maps each source file name to its analysis data
  val importedFiles: MMap[String, AnalysisData] = MMap()
  var numImportedFiles: Int = heapBuildConfig.initNumImportedFiles
  val fidsPerFile: Int = 1000
  val userAsitesPerFile: Int = 100000

  // the directory containing modeled NPM modules
  private val npmModuleDirs = List("src", "main", "resources", "modules")
  // a mapping of (module
  private val modeledNpmModules: Map[String, String] = Map(
    ("react", "react.js")
  )

  private def npmModulePath(moduleName: String): Option[String] =
    modeledNpmModules.get(moduleName).map(n => Useful.path(npmModuleDirs ++ List(n): _*))

  private val jsExtensions = List[String](".js", ".jsx")

  // maps a *relative* module specifier path to its *absolute* canonical path.
  // returns `None` if the module specifier is referencing a non-JS file
  private def resolveModuleSpecifierPath(path: String): Option[String] = {
    // if the path starts with a period, it's (probably) a relative file path

    if (path.startsWith(".")) {
      // concatenate the source file's directory with the module specifier string
      val baseDir = Paths.get(new File(safeConfig.fileNames.head).getParentFile.getCanonicalPath)
      val fileName = baseDir.resolve(path).toString

      val baseName = fileName.substring(fileName.lastIndexOf("/"))
      if (baseName.contains(".")) {
        val extn = baseName.toString.split("\\.").last

        // check if the file has an explicit extension *other than* a JS file.
        // react programs may import non-js files, like CSS stylesheets or images.
        if (jsExtensions.exists(fileName.endsWith(_))) Some(fileName)
        else None
      } else {
        // add an implicit `.js` suffix on module specifiers with no explicit file extension
        Some(fileName + ".js")
      }
    } else {
      // if `path` doesn't start with a period, it's an npm package, which we can model from SAFE
      npmModulePath(path) match {
        case Some(mp) => Some(mp)
        case None =>
          println(s"Unmodelled NPM module was imported: '${path}'")
          None
      }
    }
  }

  // returns the new state after analyzing the imported file.
  private def importFile(fileName: String, entryState: AbsState): AbsState = importedFiles.get(fileName) match {
    // if `fileName` has already been imported, return the cached analysis data.
    case Some(value) => entryState

    // if `fileName` isn't present in `importedFiles`, we import it for the first time here.
    case None => {
      // run the `translate` command on the imported file
      val ir: Try[IRRoot] = CmdTranslate(List("-silent", fileName), false)

      numImportedFiles += 1

      val newSafeConfig = safeConfig.copy(fileNames = List(fileName))

      // apply the `CFGBuild` phase with offset function and heap allocation indices
      // (we offset them so that they don't collide with allocations from other files)
      val cfgBuildConfig = CFGBuild.defaultConfig.copy(
        initFIdCount = numImportedFiles * fidsPerFile,
        initUserAsiteSize = numImportedFiles * userAsitesPerFile
      )
      val importCFG: Try[CFG] = ir.flatMap(CFGBuild(_, newSafeConfig, cfgBuildConfig))

      // apply the `HeapBuild` phase to the CFG, which creates a pre-initialized `Semantics` object.
      val heapBuildConfig = HeapBuild.defaultConfig.copy(
        initHeap = Some(entryState.heap),
        initNumImportedFiles = numImportedFiles
      )
      val heapBuild = importCFG.flatMap(HeapBuild(_, newSafeConfig, heapBuildConfig))

      // apply the `Analyze` phase to the pre-initialized `Semantics` object.
      val analysisRes = heapBuild.flatMap(Analyze(_, newSafeConfig, Analyze.defaultConfig))

      analysisRes match {
        case Failure(exception) =>
          throw new Error(s"failure analyzing imported file ${fileName}")
        case Success(data) =>
          importedFiles(fileName) = data
          val (importCFG, _, initTP, importSem) = data

          // sync this file's `numImportedFiles` count to match the "child" imported file,
          // which may itself have imported more files to increase the count further.
          numImportedFiles = importSem.numImportedFiles

          importCFG.getUserFuncs.foreach(cfg.addJSModel)

          // extract the heap at the end of the imported file.
          val heapAfterImport = importSem.getState(ControlPoint(importCFG.globalFunc.exit, initTP)).heap

          // maintain the same global scope of the entry state, rather than that of the imported file.
          val nextHeap = heapAfterImport.update(GLOBAL_LOC, entryState.heap.get(GLOBAL_LOC))

          // write the new heap to the entry state, and return it.
          entryState.copy(heap = nextHeap)
      }
    }
  }

  private def getImportValue(fileName: String, importName: String): (AbsValue, Set[Exception]) = {
    val (cfg, _, globalTP, sem) = importedFiles(fileName)
    val exitCP = ControlPoint(cfg.globalFunc.exit, globalTP)
    sem.getExport(exitCP, importName)
  }

  private def getNameSpaceImportObj(fileName: String): (AbsObj, Set[Exception]) = {
    val (cfg, _, globalTP, sem) = importedFiles(fileName)
    val exitCP = ControlPoint(cfg.globalFunc.exit, globalTP)
    sem.getNameSpaceExportObj(exitCP)
  }

  private def getDefaultImportValue(fileName: String): (AbsValue, Set[Exception]) = {
    val (cfg, _, globalTP, sem) = importedFiles(fileName)
    val exitCP = ControlPoint(cfg.globalFunc.exit, globalTP)
    sem.getDefaultExport(exitCP)
  }

  private def getImportFileState(fileName: String): AbsState = {
    val (cfg, _, globalTP, sem) = importedFiles(fileName)
    val exitCP = ControlPoint(cfg.globalFunc.exit, globalTP)
    sem.getState(exitCP)
  }

  // exported values
  val exports: MMap[String, AbsExportValue] = MMap()
  var defaultExport: Option[AbsExportValue] = None

  def getExport(cp: ControlPoint, exportName: String): (AbsValue, Set[Exception]) =
    exports.get(exportName) match {
      case Some(v) => v.getValue(this, cp)
      case None => {
        println(s"Attempted to read undefined export '${exportName}' from '${safeConfig.fileNames.head}'")
        (AbsValue.Bot, Set(ReferenceError))
      }
    }

  def getNameSpaceExportObj(cp: ControlPoint): (AbsObj, Set[Exception]) = exports.foldLeft((AbsObj.Empty, Set[Exception]()))((resultPair, exportPair) => {
    val (namespace, exc) = resultPair
    val (exportName, exportV) = exportPair

    // read the `AbsValue` exported under the name `exportName`
    val (value, valueExc) = exportV.getValue(this, cp)

    val absDataProp = AbsDataProp(value, AbsBool(true), AbsBool(true), AbsBool(false))
    (namespace.update(exportName, absDataProp), exc ++ valueExc)
  })

  def getDefaultExport(cp: ControlPoint): (AbsValue, Set[Exception]) = defaultExport match {
    case Some(expVal) => expVal.getValue(this, cp)
    case None => (AbsValue.Bot, Set())
  }

  // call control point to CallInfo
  // Call -> TracePartition -> CallInfo
  private val ccpToCallInfo: MMap[Call, MMap[TracePartition, CallInfo]] = MMap()

  def setCallInfo(call: Call, tp: TracePartition, info: CallInfo): Unit = {
    val map = ccpToCallInfo.getOrElse(call, {
      val newMap = MMap[TracePartition, CallInfo]()
      ccpToCallInfo(call) = newMap
      newMap
    })
    map(tp) = info
  }

  def getCallInfo(call: Call, tp: TracePartition): CallInfo = {
    ccpToCallInfo
      .getOrElse(call, MMap())
      .getOrElse(tp, CallInfo(AbsState.Bot, AbsValue.Bot, AbsValue.Bot))
  }

  // constructs a string representation of the current `ControlPoint -> CallInfo` map.
  def getCallInfoString: String = ccpToCallInfo.foldLeft("")((result, pair) => {
    val (callBlock, v) = pair
    v.foldLeft(result)((res, innerPair) => {
      val (tp, callInfo) = innerPair
      res + s"($callBlock (${callBlock.func.name}), $tp) -> $callInfo\n\n"
    })
  })

  // each "block" of the CFG corresponds a section of code that will execute linearly.
  // for each such block, the analysis maintains a different program state for each trace partition token
  // it encounters at that block.
  // the `cpToState` map thus records all such program states, parameterized by each state's `CFGBlock` and
  // `TracePartition` token.
  private val cpToState: MMap[CFGBlock, MMap[TracePartition, AbsState]] = MMap()

  def getState(block: CFGBlock): Map[TracePartition, AbsState] =
    cpToState.getOrElse(block, {
      val newMap = MMap[TracePartition, AbsState]()
      cpToState(block) = newMap
      newMap
    }).foldLeft(Map[TracePartition, AbsState]())(_ + _)

  def getState(cp: ControlPoint): AbsState = {
    val block = cp.block
    val tp = cp.tracePartition
    getState(block).getOrElse(tp, AbsState.Bot)
  }
  def setState(cp: ControlPoint, state: AbsState): Unit = {
    val block = cp.block
    val tp = cp.tracePartition
    val map = cpToState.getOrElse(block, {
      val newMap = MMap[TracePartition, AbsState]()
      cpToState(block) = newMap
      newMap
    })
    if (state.isBottom) map -= tp
    else map(tp) = state
  }

  type OutCtxtMap = Map[CFGBlock, Set[LoopContext]]
  private var outCtxtMap: OutCtxtMap = Map()
  def addOutCtxt(block: CFGBlock, ctxt: LoopContext): Unit =
    outCtxtMap += block -> (getOutCtxtSet(block) + ctxt)
  def getOutCtxtSet(block: CFGBlock): Set[LoopContext] =
    outCtxtMap.getOrElse(block, Set())

  // for each interprocedural edge between two control points,
  // we record `EdgeData` that represents a "context switch" from the
  // caller function's context to the callee function's context.
  type IPSucc = Map[ControlPoint, EdgeData]
  type IPSuccMap = Map[ControlPoint, IPSucc]
  private var ipSuccMap: IPSuccMap = Map()
  def getAllIPSucc: IPSuccMap = ipSuccMap
  def setAllIPSucc(newMap: IPSuccMap): Unit = { ipSuccMap = newMap }

  // gets all outward interprocedural edges from the input control point `cp`
  def getInterProcSucc(cp: ControlPoint): Option[IPSucc] = ipSuccMap.get(cp)

  // Adds inter-procedural call edge from call-block cp1 to entry-block cp2.
  // Edge label ctx records callee context, which is joined if the edge existed already.
  def addIPEdge(cp1: ControlPoint, cp2: ControlPoint, data: EdgeData): Unit = {
    val updatedSuccMap = ipSuccMap.get(cp1) match {
      case None => Map(cp2 -> data)
      case Some(map2) => map2.get(cp2) match {
        case None =>
          map2 + (cp2 -> data)
        case Some(oldData) =>
          map2 + (cp2 -> (data ⊔ oldData))
      }
    }
    ipSuccMap += (cp1 -> updatedSuccMap)
  }

  // handle an interprocedural edge, which occurs when control flow transitions from one function to another.
  // the different ways in which this can happen are expressed as cases of this function's `match` statement.
  // specifically, the function applies the transfer function of an interprocedural edge to the input state `st`,
  // returning the outgoing state on the other side of the interprocedural edge.
  def E(cp1: ControlPoint, cp2: ControlPoint, data: EdgeData, st: AbsState): AbsState = {
    (cp1.block, cp2.block) match {
      // case 1:
      // calling the function `f`, inducing an edge from the `Call` block that calls `f`
      // to the `Entry` block of `f` itself.
      case (_, Entry(f)) => st.context match {
        // case 1.1:
        // if the state's context is bottom, propagate that across the interprocedural edge.
        case _ if st.context.isBottom => AbsState.Bot

        // case 1.2:
        // entering a function, and the state has a nondegenerate context.
        case ctx1: AbsContext => {
          val objEnv = data.env.record.decEnvRec.GetBindingValue("@scope") match {
            case (value, _) => AbsLexEnv.NewDeclarativeEnvironment(value.locset)
          }
          val (envRec, _) = data.env.record.decEnvRec.DeleteBinding("@scope")
          val ctx2 = ctx1.subsPureLocal(data.env.copy(record = envRec))
          val ctx3 = data.env.outer.foldLeft[AbsContext](AbsContext.Bot)((hi, locEnv) => {
            hi ⊔ ctx2.update(locEnv, objEnv)
          })
          st.copy(context = ctx3.setThisBinding(data.thisBinding))
            .setAllocLocSet(data.allocs)
        }
      }

      // the remaining cases are induced by exiting (i.e. returning from) a function.

      // case 2:
      // if the state's context is bottom as we exit a function, propagate that across the interprocedural edge.
      case (Exit(_), _) if st.context.isBottom => AbsState.Bot

      // case 3:
      // a normal exit (i.e. where no exception is thrown) will return from the callee's `Exit` block to
      // the caller's `AfterCall` block corresponding to the original `Call` block.
      case (Exit(f1), acall @ AfterCall(f2, retVar, call)) =>
        val call = acall.call
        val params = f1.argVars
        val info = getCallInfo(call, cp2.tracePartition)
        val state =
          if (RecencyMode) {
            st.afterCall(info)
          } else st
        val (ctx1, allocs1) = (state.context, state.allocs)
        val EdgeData(allocs2, env1, thisBinding) = data.fix(allocs1)

        if (allocs2.isBottom) AbsState.Bot
        else {
          val localEnv = ctx1.pureLocal
          val (returnV, _) = localEnv.record.decEnvRec.GetBindingValue("@return")
          val ctx2 = ctx1.subsPureLocal(env1)
          val newSt = state.copy(context = ctx2.setThisBinding(thisBinding))
            .setAllocLocSet(allocs2)
          newSt.varStore(retVar, returnV)
        }

      // cases 4, 5:
      // if we exit the function via a thrown exception and either the state's context or allocation set is bottom,
      // propagate that across the interprocedural edge.
      case (ExitExc(_), _) if st.context.isBottom => AbsState.Bot
      case (ExitExc(_), _) if st.allocs.isBottom => AbsState.Bot

      // case 6:
      // we exit the callee `f1` via a thrown exception, and the state is nondegenerate.
      // in this case, we return to the caller's `AfterCatch` block corresponding to the original `Call` block
      // which called `f1`.
      case (ExitExc(f1), acatch @ AfterCatch(_, _)) =>
        val call = acatch.call
        val params = f1.argVars
        val info = getCallInfo(call, cp2.tracePartition)
        val state =
          if (RecencyMode) {
            st.afterCall(info)
          } else st
        val (ctx1, c1) = (state.context, state.allocs)
        val EdgeData(c2, envL, thisBinding) = data.fix(c1)
        val env1 = envL.record.decEnvRec
        if (c2.isBottom) AbsState.Bot
        else {
          val localEnv = ctx1.pureLocal
          val (excValue, _) = localEnv.record.decEnvRec.GetBindingValue("@exception")
          val (oldExcAllValue, _) = env1.GetBindingValue("@exception_all")
          val (env2, _) = env1.SetMutableBinding("@exception", excValue)
          val (env3, _) = env2.SetMutableBinding("@exception_all", excValue ⊔ oldExcAllValue)
          val ctx2 = ctx1.subsPureLocal(envL.copy(record = env3))
          state.copy(context = ctx2.setThisBinding(thisBinding))
            .setAllocLocSet(c2)
        }
      case _ => st
    }
  }

  // C = "control point".
  // apply the transfer function of the control point `cp` to the incoming abstract state `st`.
  // returns a pair of outgoing normal and exception states.
  def C(cp: ControlPoint, st: AbsState): (AbsState, AbsState) = {
    // if the incoming state is bottom, then so are the outgoing states.
    if (st.isBottom) (AbsState.Bot, AbsState.Bot)
    else {
      val ctx = st.context
      val allocs = st.allocs
      cp.block match {
        // case 1: an `Entry` block denoting the endpoint of an interprocedural edge.
        case Entry(_) => {
          // the `CFGFunction` containing this `Entry` control point
          val fun = cp.block.func
          // list of argument names
          val xArgVars = fun.argVars
          // list of locally declared variable names
          val xLocalVars = fun.localVars
          // the local lexical environment of this function
          val localEnv = ctx.pureLocal
          // the abstract value of the `arguments` array
          val (argV, _) = localEnv.record.decEnvRec.GetBindingValue(fun.argumentsName)

          // bind argument values to their names in the function scope, one at a time.
          // each time we bind a new argument, we get an intermediate state `iSt`.
          // after binding every argument, we end up with the final state `nSt`.
          val (nSt, _) = xArgVars.foldLeft((st, 0))((result, iArgId) => {
            // within this iteration:
            //   - `i` is the index of the next argument we're going to bind.
            //   - `iSt` is the intermediate state which incorporates all argument bindings we've made so far.
            val (iSt, i) = result

            // compute the abstract value of the ith argument by joining its abstract value
            // in all possible `arguments` arrays.
            // start by folding over the possible locations of `arguments`:
            val iArgValue = argV.locset.foldLeft(AbsValue.Bot)((vk, argumentsLoc) => {
              // first, retrieve the object at that location from the heap: `iSt.heap.get(argumentsLoc)`.
              // then from that object retrieve the value at key `i`.
              vk ⊔ iSt.heap.get(argumentsLoc).Get(i.toString, iSt.heap)
            })

            // update the intermediate state `iSt` by binding the i-th argument identifier
            // to its previously computed abstract value.
            // this is also where we increment the argument index `i`.
            (iSt.createMutableBinding(iArgId, iArgValue), i + 1)
          })

          // bind declared local variable names to `undefined`
          val newSt = xLocalVars.foldLeft(nSt)((jSt, x) => {
            val undefV = AbsValue(Undef)
            jSt.createMutableBinding(x, undefV)
          })

          // return the final state with all bindings applied.
          (newSt, AbsState.Bot)
        }

        // case 2: a `Call` block, denoting the starting point of an interprocedural edge.
        case (call: Call) =>
          // the transfer function for a `Call` block is implemented in `internalCI`.
          // from that method, we return the outgoing normal and exceptional states,
          // as well as the value of `this` (`thisVal`) and `arguments` (`argVal`).
          val (thisVal, argVal, resSt, resExcSt) = internalCI(cp, call.callInst, st, AbsState.Bot)

          // cache this data in the `CallInfo` map
          setCallInfo(call, cp.tracePartition, CallInfo(resSt, thisVal, argVal))

          // return the states computed by `internalCI`.
          (resSt, resExcSt)

        // case 3: a `NormalBlock` block, which covers all other blocks that affect the program state.
        // (in particular, these are intraprocedural, which means they don't cross between two functions.)

        // in this case, the transfer function of the entire block is the composition of the
        // transfer functions of each of the block's individual instructions.
        case block: NormalBlock =>
          // fold over each instruction in the block.
          // NOTE that we're doing a `foldRight` here, which means iterating over the instruction list in reverse order.
          // apparently, the instruction list is just stored like that (with the first element being the final instruction
          // and the last element being the first instruction).
          block.getInsts.foldRight((st, AbsState.Bot))((inst, states) => {
            // retrieve the states computed by the previous iteration
            val (oldSt, oldExcSt) = states

            // run the next instruction's transfer function on those states.
            // the method `I` returns the (normal state, exceptional state) pair, which agrees with
            // the result of this fold, so we can directly return the call to `I`.
            I(cp, inst, oldSt, oldExcSt)
          })

        // case 4: for all other block types, pass through the original state `st` unchanged.
        // this indicates that executing one of these blocks doesn't affect the program state.
        case _ => (st, AbsState.Bot)
      }
    }
  }

  // `I` is short for "instruction".
  // apply the transfer function of the normal instruction `i` in the control point `cp`
  // to the normal and exceptional states `(st, excSt)`.
  // returns the resulting outgoing normal and exceptional states as a pair.
  def I(cp: ControlPoint, i: CFGNormalInst, st: AbsState, excSt: AbsState): (AbsState, AbsState) = {
    val tp = cp.tracePartition
    i match {
      case _ if st.isBottom => (AbsState.Bot, excSt)
      case CFGAlloc(_, _, x, e, newASite) => {
        val objProtoSingleton = LocSet(OBJ_PROTO_LOC)
        val loc = Loc(newASite, tp)
        val st1 = st.alloc(loc)
        val (vLocSet, excSet) = e match {
          case None => (objProtoSingleton, ExcSetEmpty)
          case Some(proto) => {
            val (v, es) = V(proto, st1)
            if (!v.pvalue.isBottom)
              (v.locset + OBJ_PROTO_LOC, es)
            else
              (v.locset, es)
          }
        }
        val h2 = st1.heap.update(loc, AbsObj.newObject(vLocSet))
        val newSt = st1.copy(heap = h2).varStore(x, AbsValue(loc))
        val newExcSt = st.raiseException(excSet)
        val s1 = excSt ⊔ newExcSt
        (newSt, s1)
      }
      case CFGAllocArray(_, _, x, n, newASite) => {
        val loc = Loc(newASite, tp)
        val st1 = st.alloc(loc)
        val np = AbsNum(n.toInt)
        val h2 = st1.heap.update(loc, AbsObj.newArrayObject(np))
        val newSt = st1.copy(heap = h2).varStore(x, AbsValue(loc))
        (newSt, excSt)
      }
      case CFGAllocArg(_, _, x, n, newASite) => {
        val loc = Loc(newASite, tp)
        val st1 = st.alloc(loc)
        val absN = AbsNum(n.toInt)
        val h2 = st1.heap.update(loc, AbsObj.newArgObject(absN))
        val newSt = st1.copy(heap = h2).varStore(x, AbsValue(loc))
        (newSt, excSt)
      }

      // `x`: the identifier receiving the value of `this`
      // `e`: the expression which holds the default value of `this` (global object or method receiver)
      case CFGEnterCode(_, _, thisId, thisExpr) => {
        // compute the abstract value of `this` from the program state.
        // note that we capture exceptions raised by this computation in `excSet`.
        val (v, excSet) = V(thisExpr, st)
        val thisVal = AbsValue(v.getThis(st.heap))

        // bind the computed value to the `this` identifier,
        // resulting in a new state `st1`.
        val st1 = if (!v.isBottom) st.varStore(thisId, thisVal)
        else AbsState.Bot

        // raise the exceptions captured above in `st`.
        // note that we don't raise these in `st1`, since the exceptions occurred
        // before we could compute a value for `this` and save it to `st1`.
        val newExcSt = st.raiseException(excSet)

        (st1, excSt ⊔ newExcSt)
      }

      case CFGExprStmt(_, _, lhs, expr) => {
        // compute the abstract value of `expr`
        val (v, excSet) = V(expr, st)

        // write the computed value to the identifier `lhs`
        val st1 =
          if (!v.isBottom) st.varStore(lhs, v)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }

      case CFGDelete(_, _, x1, CFGVarRef(_, x2)) => {
        val baseV = st.lookupBase(x2)
        val undefB = baseV.pvalue.undefval.fold(AB)(_ => AT)
        val (st1, locB) =
          baseV.locset.foldLeft[(AbsState, AbsBool)](AbsState.Bot, AB)((res, baseLoc) => {
            val (tmpState, tmpB) = res
            val (delState, delB) = st.delete(baseLoc, x2.text)
            (tmpState ⊔ delState, tmpB ⊔ delB)
          })
        val st2 = st1.varStore(x1, locB ⊔ undefB)
        (st2, excSt)
      }
      case CFGDelete(_, _, x1, expr) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) {
            val trueVal = AbsValue(AT)
            st.varStore(x1, trueVal)
          } else AbsState.Bot
        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case CFGDeleteProp(_, _, lhs, obj, index) => {
        // locSet must not be empty because obj is coming through <>toObject.
        val (value, _) = V(obj, st)
        val locSet = value.locset
        val (v, excSet) = V(index, st)
        val absStrSet =
          if (v.isBottom) Set[AbsStr]()
          else TypeConversionHelper.ToPrimitive(v, st.heap).toStringSet
        val (h1, b) = locSet.foldLeft[(AbsHeap, AbsBool)](AbsHeap.Bot, AB)((res1, l) => {
          val (tmpHeap1, tmpB1) = res1
          absStrSet.foldLeft((tmpHeap1, tmpB1))((res2, s) => {
            val (tmpHeap2, tmpB2) = res2
            val (delHeap, delB) = st.heap.delete(l, s)
            (tmpHeap2 ⊔ delHeap, tmpB2 ⊔ delB)
          })
        })
        val st1 = st.copy(heap = h1)
        val st2 =
          if (st1.isBottom) AbsState.Bot
          else {
            st1.varStore(lhs, AbsValue(b))
          }
        val newExcSt = st.raiseException(excSet)
        (st2, excSt ⊔ newExcSt)
      }
      case CFGStore(_, block, obj, index, rhs) => {
        // locSet must not be empty because obj is coming through <>toObject.
        val (value, _) = V(obj, st)
        val locSet = value.locset

        val (idxV, excSetIdx) = V(index, st)
        val (vRhs, esRhs) = V(rhs, st)

        val (heap1, excSet1) =
          (idxV, vRhs) match {
            case (v, _) if v.isBottom => (AbsHeap.Bot, excSetIdx)
            case (_, v) if v.isBottom => (AbsHeap.Bot, excSetIdx ++ esRhs)
            case _ =>
              // iterate over set of strings for index
              val absStrSet = TypeConversionHelper.ToPrimitive(idxV, st.heap).toStringSet
              absStrSet.foldLeft[(AbsHeap, Set[Exception])]((AbsHeap.Bot, excSetIdx ++ esRhs))((res1, absStr) => {
                val (tmpHeap1, tmpExcSet1) = res1
                val (tmpHeap2, tmpExcSet2) = Helper.storeHelp(locSet, absStr, vRhs, st.heap)
                (tmpHeap1 ⊔ tmpHeap2, tmpExcSet1 ++ tmpExcSet2)
              })
          }

        val newExcSt = st.raiseException(excSet1)
        (st.copy(heap = heap1), excSt ⊔ newExcSt)
      }
      case CFGStoreStringIdx(_, block, obj, strIdx, rhs) => {
        // locSet must not be empty because obj is coming through <>toObject.
        val (value, _) = V(obj, st)
        val locSet = value.locset
        val (vRhs, esRhs) = V(rhs, st)

        val (heap1, excSet1) =
          (strIdx, vRhs) match {
            case (_, v) if v.isBottom => (AbsHeap.Bot, esRhs)
            case (EJSString(str), v) =>
              val absStr = AbsStr(str)
              val (tmpHeap2, tmpExcSet2) = Helper.storeHelp(locSet, absStr, vRhs, st.heap)
              (tmpHeap2, tmpExcSet2 ++ esRhs)
          }

        val newExcSt = st.raiseException(excSet1)
        (st.copy(heap = heap1), excSt ⊔ newExcSt)
      }

      case CFGFunExpr(_, block, lhs, None, f, aNew1, aNew2, None) => {
        //Recency Abstraction
        val loc1 = Loc(aNew1, tp)
        val loc2 = Loc(aNew2, tp)
        val st1 = st.alloc(loc1)
        val st2 = st1.alloc(loc2)
        val oNew = AbsObj.newObject(OBJ_PROTO_LOC)

        val n = AbsNum(f.argVars.length)
        val localEnv = st2.context.pureLocal
        val h3 = st2.heap.update(loc1, AbsObj.newFunctionObject(f.id, localEnv.outer, loc2, n))

        val fVal = AbsValue(loc1)
        val h4 = h3.update(loc2, oNew.update("constructor", AbsDataProp(fVal, AT, AF, AT)))

        val newSt = st2.copy(heap = h4).varStore(lhs, fVal)
        (newSt, excSt)
      }

      case CFGFunExpr(_, block, lhs, Some(name), f, aNew1, aNew2, Some(aNew3)) => {
        // Recency Abstraction
        val loc1 = Loc(aNew1, tp)
        val loc2 = Loc(aNew2, tp)
        val loc3 = Loc(aNew3, tp)
        val st1 = st.alloc(loc1)
        val st2 = st1.alloc(loc2)
        val st3 = st2.alloc(loc3)

        val oNew = AbsObj.newObject(OBJ_PROTO_LOC)
        val n = AbsNum(f.argVars.length)
        val fObjValue = AbsValue(loc3)
        val h4 = st3.heap.update(loc1, AbsObj.newFunctionObject(f.id, fObjValue, loc2, n))

        val fVal = AbsValue(loc1)
        val h5 = h4.update(loc2, oNew.update("constructor", AbsDataProp(fVal, AT, AF, AT)))

        val localEnv = st3.context.pureLocal
        val oEnv = AbsLexEnv.NewDeclarativeEnvironment(localEnv.outer)
        val oEnvRec2 = oEnv.record.decEnvRec
          .CreateImmutableBinding(name.text)
          .InitializeImmutableBinding(name.text, fVal)
        val newCtx = st3.context.update(loc3, oEnv.copy(record = oEnvRec2))
        val newSt = AbsState(h5, newCtx, st3.allocs).varStore(lhs, fVal)
        (newSt, excSt)
      }

      case CFGAssert(_, _, expr, _) => B(expr, st, excSt)

      case CFGCatch(_, _, x) => {
        val localEnv = st.context.pureLocal
        val (excSetV, _) = localEnv.record.decEnvRec.GetBindingValue("@exception_all")
        val (excV, _) = localEnv.record.decEnvRec.GetBindingValue("@exception")
        val st1 = st.createMutableBinding(x, excV)
        val env = st1.context.pureLocal
        val (newEnv, _) = env.record.decEnvRec.SetMutableBinding("@exception", excSetV)
        val newCtx = st1.context.subsPureLocal(env.copy(record = newEnv))
        val newSt = st1.copy(context = newCtx)
        (newSt, AbsState.Bot)
      }
      case CFGReturn(_, _, Some(expr)) => {
        val (v, excSet) = V(expr, st)
        val ctx1 =
          if (!v.isBottom) {
            val localEnv = st.context.pureLocal
            val (localEnv2, _) = localEnv.record.decEnvRec.SetMutableBinding("@return", v)
            st.context.subsPureLocal(localEnv.copy(record = localEnv2))
          } else AbsContext.Bot
        val newExcSt = st.raiseException(excSet)
        (st.copy(context = ctx1), excSt ⊔ newExcSt)
      }
      case CFGReturn(_, _, None) => {
        val localEnv = st.context.pureLocal
        val (localEnv2, _) = localEnv.record.decEnvRec.SetMutableBinding("@return", AbsUndef.Top)
        val ctx1 = st.context.subsPureLocal(localEnv.copy(record = localEnv2))
        val newSt = st.copy(context = ctx1)
        (newSt, excSt)
      }
      case CFGThrow(_, _, expr) => {
        val (v, excSet) = V(expr, st)
        val localEnv = st.context.pureLocal
        val (excSetV, _) = localEnv.record.decEnvRec.GetBindingValue("@exception_all")
        val (newEnv, _) = localEnv.record.decEnvRec.SetMutableBinding("@exception", v)
        val (newEnv2, _) = newEnv.SetMutableBinding("@exception_all", v ⊔ excSetV)
        val (newEnv3, _) = newEnv2
          .CreateMutableBinding("@return").fold(newEnv2)((e: AbsDecEnvRec) => e)
          .SetMutableBinding("@return", AbsUndef.Top)
        val ctx1 = st.context.subsPureLocal(localEnv.copy(record = newEnv3))
        val newExcSt = st.raiseException(excSet)

        (AbsState.Bot, excSt ⊔ st.copy(context = ctx1) ⊔ newExcSt)
      }
      case CFGInternalCall(ir, _, lhs, name, arguments, loc) =>
        IC(cp, ir, lhs, name, arguments, loc, st, excSt)
      case CFGNoOp(_, _, _) => (st, excSt)

      case CFGNameSpaceImport(_, _, importedFile, binding) =>
        resolveModuleSpecifierPath(importedFile) match {
          case Some(fileName) =>
            val st1 = importFile(fileName, st)

            val (exportObj, _) = getNameSpaceImportObj(fileName)

            // write the export obj to the location(s) referenced by `binding` in the heap
            val (target, exc) = st1.lookup(binding)
            val heap1 = target.locset.foldLeft(st1.heap)((nextHeap, loc) => nextHeap.update(loc, exportObj))

            // write the new heap to the state
            val st2 = st1.copy(heap = heap1)

            (st2, excSt)
          case None =>
            (st, excSt)
        }

      case CFGDefaultImport(_, _, importedFile, binding) =>
        // imported files write to separate sections of the heap.
        // after importing the file, we first copy that updated heap into the program state.
        resolveModuleSpecifierPath(importedFile) match {
          case Some(fileName) =>
            val st1 = importFile(fileName, st)

            // read the value of `importName` from the exports of `importedFile`.
            val (v, _) = getDefaultImportValue(fileName)

            // write that value to the identifier `binding` in the program state.
            val st2 =
              if (!v.isBottom) st1.varStore(binding, v)
              else AbsState.Bot

            (st2, excSt)
          case None =>
            (st, excSt)
        }

      case CFGImport(_, _, importedFile, binding, importName) =>
        // imported files write to separate sections of the heap.
        // after importing the file, we first copy that updated heap into the program state.
        resolveModuleSpecifierPath(importedFile) match {
          case Some(fileName) =>
            val st1 = importFile(fileName, st)

            // read the value of `importName` from the exports of `importedFile`.
            val (v, _) = getImportValue(fileName, importName)

            // write that value to the identifier `binding` in the program state.
            val st2 =
              if (!v.isBottom) st1.varStore(binding, v)
              else AbsState.Bot

            (st2, excSt)
          case None =>
            (st, excSt)
        }

      case CFGDefaultExport(_, _, binding) =>
        defaultExport = Some(ExportedId(binding))
        (st, excSt)

      case CFGExport(_, _, binding, exportName) =>
        // `export { _ as default }` also implies a default export
        if (exportName.text == "default") {
          defaultExport = Some(ExportedId(binding))
        } else {
          exports(exportName.text) = ExportedId(binding)
        }

        (st, excSt)

      case _ =>
        println("unrecognized instruction " + i.toString())
        (st, excSt)
    }
  }

  // internal API value
  def getInternalValue(name: String): Option[AbsValue] = name match {
    case (NodeUtil.INTERNAL_TOP) => Some(AbsValue.Top)
    case (NodeUtil.INTERNAL_UINT) => Some(AbsNum.UInt)
    case (NodeUtil.INTERNAL_NUINT) => Some(AbsNum.NUInt)
    case (NodeUtil.INTERNAL_GLOBAL) => Some(AbsValue(GLOBAL_LOC))
    case (NodeUtil.INTERNAL_BOOL_TOP) => Some(AbsBool.Top)
    case (NodeUtil.INTERNAL_NUM_TOP) => Some(AbsNum.Top)
    case (NodeUtil.INTERNAL_STR_TOP) => Some(AbsStr.Top)

    case (NodeUtil.INTERNAL_EVAL_ERR) => Some(EVAL_ERROR_LOC)
    case (NodeUtil.INTERNAL_RANGE_ERR) => Some(RANGE_ERROR_LOC)
    case (NodeUtil.INTERNAL_REF_ERR) => Some(REF_ERROR_LOC)
    case (NodeUtil.INTERNAL_SYNTAX_ERR) => Some(SYNTAX_ERROR_LOC)
    case (NodeUtil.INTERNAL_TYPE_ERR) => Some(TYPE_ERROR_LOC)
    case (NodeUtil.INTERNAL_URI_ERR) => Some(URI_ERROR_LOC)
    case (NodeUtil.INTERNAL_EVAL_ERR_PROTO) => Some(EVAL_ERROR_PROTO_LOC)
    case (NodeUtil.INTERNAL_RANGE_ERR_PROTO) => Some(RANGE_ERROR_PROTO_LOC)
    case (NodeUtil.INTERNAL_REF_ERR_PROTO) => Some(REF_ERROR_PROTO_LOC)
    case (NodeUtil.INTERNAL_SYNTAX_ERR_PROTO) => Some(SYNTAX_ERROR_PROTO_LOC)
    case (NodeUtil.INTERNAL_TYPE_ERR_PROTO) => Some(TYPE_ERROR_PROTO_LOC)
    case (NodeUtil.INTERNAL_URI_ERR_PROTO) => Some(URI_ERROR_PROTO_LOC)
    case (NodeUtil.INTERNAL_ERR_PROTO) => Some(ERROR_PROTO_LOC)
    case (NodeUtil.INTERNAL_OBJ_CONST) => Some(OBJ_LOC)
    case (NodeUtil.INTERNAL_ARRAY_CONST) => Some(ARR_LOC)
    case _ => None
  }

  // internal SAFE API call
  // CFGInternalCall(ir, _, lhs, name, arguments, loc)
  def IC(
    cp: ControlPoint, ir: IRNode, lhs: CFGId, name: String, args: List[CFGExpr],
    loc: Option[AllocSite], st: AbsState, excSt: AbsState
  ): (AbsState, AbsState) = {
    val tp = cp.tracePartition
    (name, args, loc) match {
      case (NodeUtil.INTERNAL_REACT_RENDER, List(element, container), None) => {
        val (elementValue, excSetO) = V(element, st)
        val desc: CompDesc = ReactHelper.extractCompDesc(elementValue, st, cfg)
        ReactState.mount(desc)
        Console.println(ReactState.toString)
        (st, excSt)
      }

      case (NodeUtil.INTERNAL_PRINT, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        println(s"[DEBUG] $cp")
        println(s"        expression: $expr")
        println(s"        exceptions: $excSet")
        println(s"        pvalue    : ${v.pvalue}")
        println(s"        objects:")
        v.locset.foreach(loc => println(st.heap.toStringLoc(loc).get))
        (st, excSt)
      }
      case (NodeUtil.INTERNAL_NOT_YET_IMPLEMENTED, List(expr), None) => {
        val (v, excSet) = V(expr, st);
        excLog.signal(SemanticsNotYetImplementedError(v, cp))
        (st, excSt)
      }
      case (NodeUtil.INTERNAL_CHAR_CODE, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val numval = v.pvalue.strval.gamma match {
          case ConFin(strset) => AbsNum(strset.map {
            case Str(str) => Num(str(0).toInt)
          })
          case ConInf => AbsNum.UInt
        }
        val newSt = st.varStore(lhs, AbsValue(numval))
        val newExcSt = st.raiseException(excSet)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_CLASS, List(exprO, exprP), None) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)
        val newH = v.locset.foldLeft(st.heap) {
          case (h, loc) => {
            val obj = st.heap.get(loc)
            val newObj = obj.update(IClass, AbsIValue(p))
            h.update(loc, newObj)
          }
        }
        val newSt = st.copy(heap = newH).varStore(lhs, p)
        val newExcSt = st.raiseException(excSetO ++ excSetP)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_CLASS, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val obj = st.heap.get(v.locset)
        val className = obj(IClass).value
        val st1 =
          if (!v.isBottom) st.varStore(lhs, className)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_PRIM_VAL, List(exprO, exprP), None) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)
        val newH = v.locset.foldLeft(st.heap) {
          case (h, loc) => {
            val obj = st.heap.get(loc)
            val newObj = obj.update(IPrimitiveValue, AbsIValue(p))
            h.update(loc, newObj)
          }
        }
        val newSt = st.copy(heap = newH).varStore(lhs, p)
        val newExcSt = st.raiseException(excSetO ++ excSetP)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_PRIM_VAL, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val obj = st.heap.get(v.locset)
        val value = obj(IPrimitiveValue).value
        val st1 =
          if (!v.isBottom) st.varStore(lhs, value)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_PROTO, List(exprO, exprP), None) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)
        val newH = v.locset.foldLeft(st.heap) {
          case (h, loc) => {
            val obj = st.heap.get(loc)
            val newObj = obj.update(IPrototype, AbsIValue(p))
            h.update(loc, newObj)
          }
        }
        val newSt = st.copy(heap = newH).varStore(lhs, p)
        val newExcSt = st.raiseException(excSetO ++ excSetP)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_PROTO, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val obj = st.heap.get(v.locset)
        val value = obj(IPrototype).value
        val st1 =
          if (!v.isBottom) st.varStore(lhs, value)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_EXTENSIBLE, List(exprO, exprP), None) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)
        val newH = v.locset.foldLeft(st.heap) {
          case (h, loc) => {
            val obj = st.heap.get(loc)
            val newObj = obj.update(IExtensible, AbsIValue(p))
            h.update(loc, newObj)
          }
        }
        val newSt = st.copy(heap = newH).varStore(lhs, p)
        val newExcSt = st.raiseException(excSetO ++ excSetP)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_EXTENSIBLE, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val obj = st.heap.get(v.locset)
        val value = obj(IExtensible).value
        val st1 =
          if (!v.isBottom) st.varStore(lhs, value)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_GET_BASE, List(CFGVarRef(_, x2)), None) => {
        val baseV = st.lookupBase(x2)
        val st1 = st.varStore(lhs, baseV)
        (st1, excSt)
      }
      case (NodeUtil.INTERNAL_GET_OWN_PROP, List(exprO, exprP), Some(aNew)) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)
        val obj = st.heap.get(v.locset)
        val name = TypeConversionHelper.ToString(p)
        val (desc, undef) = obj.GetOwnProperty(name)
        val (retSt, retV, excSet) = if (!desc.isBottom) {
          val (descObj, excSet) = AbsObj.FromPropertyDescriptor(st.heap, desc)
          val descLoc = Loc(aNew, tp)
          val state = st.alloc(descLoc)
          val retH = state.heap.update(descLoc, descObj.alloc(aNew))
          val retV = AbsValue(undef, LocSet(descLoc))
          (state.copy(heap = retH), retV, excSet)
        } else (st, AbsValue(undef), ExcSetEmpty)
        val newSt = retSt.varStore(lhs, retV)
        val newExcSt = st.raiseException(excSetO ++ excSetP ++ excSet)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_DEF_OWN_PROP, List(exprO, exprP, exprA), None) => {
        val h = st.heap
        val (objV, excSetO) = V(exprO, st)
        val (propV, excSetP) = V(exprP, st)
        val (attrV, excSetA) = V(exprA, st)

        val name = propV.pvalue.strval
        // ToPropertyDescriptor ( Obj )
        // 1. If Type(Obj) is not Object throw a TypeError exception.
        val excSet =
          if (attrV.pvalue.isBottom) ExcSetEmpty
          else Set(TypeError)
        val attr = h.get(attrV.locset)
        val desc = AbsDesc.ToPropertyDescriptor(attr, h)
        val (retH, retExcSet) = objV.locset.foldLeft((h, excSet ++ excSetO ++ excSetP ++ excSetA)) {
          case ((heap, e), loc) => {
            val obj = heap.get(loc)
            val (retObj, _, newExcSet) = obj.DefineOwnProperty(name, desc, true, h)
            val retH = heap.update(loc, retObj)
            (retH, e ++ newExcSet)
          }
        }
        val retSt = st.copy(heap = retH).varStore(lhs, AbsValue(objV.locset))
        val excSt = st.raiseException(retExcSet)
        (retSt, excSt)
      }
      case (NodeUtil.INTERNAL_TO_PRIM, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(TypeConversionHelper.ToPrimitive(v)))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_BOOL, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(TypeConversionHelper.ToBoolean(v)))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_NUM, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(TypeConversionHelper.ToNumber(v, st.heap)))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_INT, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(TypeConversionHelper.ToInteger(v, st.heap)))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_UINT_32, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(TypeConversionHelper.ToUint32(v, st.heap)))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_UINT_16, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(TypeConversionHelper.ToUint16(v, st.heap)))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_STR, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(TypeConversionHelper.ToString(v, st.heap)))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_OBJ, List(expr), Some(aNew)) => {
        val (v, excSet1) = V(expr, st)
        val (newSt, newExcSet) =
          if (v.isBottom) {
            (AbsState.Bot, excSet1)
          } else {
            val (v1, st1, excSet2) = TypeConversionHelper.ToObject(tp, v, st, aNew)
            val st2 =
              if (!v1.isBottom) st1.varStore(lhs, v1)
              else AbsState.Bot
            (st2, excSet1 ++ excSet2)
          }
        val newExcSt = st.raiseException(newExcSet)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_IS_CALLABLE, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(TypeConversionHelper.IsCallable(v, st.heap)))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_SAME_VALUE, List(left, right), None) => {
        val (l, excSet1) = V(left, st)
        val (r, excSet2) = V(right, st)
        val st1 =
          if (!l.isBottom && !r.isBottom) {
            st.varStore(lhs, AbsValue(TypeConversionHelper.SameValue(st.heap, l, r)))
          } else AbsState.Bot

        val newExcSt = st.raiseException(excSet1 ++ excSet2)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_GET_OWN_PROP_NAMES, List(expr), Some(aNew)) => {
        val h = st.heap
        val arrASite = aNew
        val (objV, excSet1) = V(expr, st)
        // 1. If Type(O) is not Object throw a TypeError exception.
        val excSet2: Set[Exception] =
          if (objV.pvalue.isBottom) ExcSetEmpty
          else Set(TypeError)

        // 2. Let array be the result of creating a new Array object.
        // (XXX: we assign the length of the Array object as the number of properties)
        val uint = "(0|[1-9][0-9]*)".r
        def toUInt(str: String): Option[Int] = str match {
          case uint(n) => Some(n.toInt)
          case _ => None
        }
        val uintFirst: (String, String) => Boolean = (l, r) => (toUInt(l), toUInt(r)) match {
          case (Some(l), Some(r)) => l < r
          case (Some(_), _) => true
          case (_, Some(_)) => false
          case _ => l < r
        }
        val (obj, resExcSt) = objV.locset.foldLeft(AbsObj.Bot, excSet1 ++ excSet2) {
          case ((o, es), loc) => h.get(loc).collectKeySet match {
            case ConInf => (AbsObj.Top, es)
            case ConFin(set) => {
              val array = AbsObj.newArrayObject(AbsNum(set.size))
              val AT = (AbsBool.True, AbsAbsent.Bot)
              // 3. For each named own property P of O (with index n started from 0)
              //   a. Let name be the String value that is the name of P.
              val (obj, resExcSt) = set.toSeq.sortWith(uintFirst).zipWithIndex.foldLeft((array, es)) {
                case ((arr, es), (key, n)) => {
                  val desc = AbsDesc((AbsValue(AbsStr(key)), AbsAbsent.Bot), AT, AT, AT)
                  val prop = AbsStr(n.toString)
                  // b. Call the [[DefineOwnProperty]] internal method of array with arguments
                  //    ToString(n), the PropertyDescriptor {[[Value]]: name, [[Writable]]:
                  //    true, [[Enumerable]]: true, [[Configurable]]:true}, and false.
                  val (newArr, _, excSet) = arr.DefineOwnProperty(prop, desc, false, h)
                  (newArr, es ++ excSet)
                }
              }
              (o ⊔ obj, resExcSt)
            }
          }
        }

        val excSt = st.raiseException(resExcSt)

        // 5. Return array.
        obj.isBottom match {
          case true => (AbsState.Bot, excSt)
          case false => {
            val arrLoc = Loc(arrASite, tp)
            val state = st.alloc(arrLoc)
            val retHeap = state.heap.update(arrLoc, obj.alloc(arrLoc))
            val excSt = state.raiseException(resExcSt)
            val st2 = state.copy(heap = retHeap)
            val retSt = st2.varStore(lhs, AbsValue(arrLoc))

            (retSt, excSt)
          }
        }
      }
      case (NodeUtil.INTERNAL_STR_OBJ, List(expr), Some(aNew)) => {
        val (v, excSet) = V(expr, st)
        val str = TypeConversionHelper.ToString(v)
        val loc = Loc(aNew, tp)
        val st1 = st.alloc(loc)
        val heap = st1.heap.update(loc, AbsObj.newStringObj(str))
        val st2 = st1.copy(heap = heap)
        val st3 =
          if (!v.isBottom) st2.varStore(lhs, AbsValue(loc))
          else AbsState.Bot
        val newExcSt = st.raiseException(excSet)
        (st3, newExcSt)
      }
      case (NodeUtil.INTERNAL_INDEX_OF, List(expr, str, pos), None) => {
        val (thisval, excSet1) = V(expr, st)
        val (strval, excSet2) = V(str, st)
        val (posval, excSet3) = V(pos, st)
        val kval = (
          thisval.pvalue.strval.gamma,
          strval.pvalue.strval.gamma,
          posval.pvalue.numval.gamma
        ) match {
            case (ConFin(thisset), ConFin(strset), ConFin(posset)) =>
              AbsNum(for (t <- thisset; s <- strset; p <- posset)
                yield Num(t.str.indexOf(s.str, p.num.toInt)))
            case _ => AbsNum.Top
          }
        val st1 = st.varStore(lhs, AbsValue(kval))
        val newExcSt = st.raiseException(excSet1 ++ excSet2 ++ excSet3)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_LAST_INDEX_OF, List(expr, str, pos), None) => {
        val (thisval, excSet1) = V(expr, st)
        val (strval, excSet2) = V(str, st)
        val (posval, excSet3) = V(pos, st)
        val kval = (
          thisval.pvalue.strval.gamma,
          strval.pvalue.strval.gamma,
          posval.pvalue.numval.gamma
        ) match {
            case (ConFin(thisset), ConFin(strset), ConFin(posset)) =>
              AbsNum(for (t <- thisset; s <- strset; p <- posset)
                yield Num(t.str.lastIndexOf(s.str, p.num.toInt)))
            case _ => AbsNum.Top
          }
        val st1 = st.varStore(lhs, AbsValue(kval))
        val newExcSt = st.raiseException(excSet1 ++ excSet2 ++ excSet3)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_SPLIT, List(str, sep, lim), Some(aNew)) => {
        val h = st.heap
        val arrASite = aNew
        val (strval, excSet1) = V(str, st)
        val (sepval, excSet2) = V(sep, st)
        val (limval, excSet3) = V(lim, st)
        val arr = (
          strval.pvalue.strval.gamma,
          sepval.pvalue.strval.gamma,
          limval.pvalue.numval.gamma
        ) match {
            case (ConFin(strset), ConFin(sepset), ConFin(limset)) => {
              val arrs = {
                for (s <- strset; p <- sepset; l <- limset)
                  yield s.str.split(p.str).take(l.num.toInt)
              }
              (AbsObj.Bot /: arrs) {
                case (obj, arr) => obj ⊔ ((AbsObj.newArrayObject(AbsNum(arr.length)) /: arr.zipWithIndex) {
                  case (arr, (str, idx)) => arr.update(
                    AbsStr(idx.toString),
                    AbsDataProp(DataProp(str, T, T, T))
                  )
                })
              }
            }
            case _ => AbsObj.newArrayObject(AbsNum.Top).update(AbsStr.Number, AbsDataProp.Top)
          }
        val arrLoc = Loc(arrASite, tp)
        val state = st.alloc(arrLoc)
        val retHeap = state.heap.update(arrLoc, arr.alloc(arrLoc))
        val excSt = state.raiseException(excSet1 ++ excSet2 ++ excSet3)
        val st2 = state.copy(heap = retHeap)
        val retSt = st2.varStore(lhs, AbsValue(arrLoc))

        (retSt, excSt)
      }
      case (NodeUtil.INTERNAL_SUBSTRING, List(str, from, to), None) => {
        val (strval, excSet1) = V(str, st)
        val (fromval, excSet2) = V(from, st)
        val (toval, excSet3) = V(to, st)
        val res = (
          strval.pvalue.strval.gamma,
          fromval.pvalue.numval.gamma,
          toval.pvalue.numval.gamma
        ) match {
            case (ConFin(strset), ConFin(fromset), ConFin(toset)) =>
              AbsStr(for (s <- strset; f <- fromset; t <- toset)
                yield Str(s.str.substring(f.num.toInt, t.num.toInt)))
            case _ => AbsStr.Top
          }
        val st1 = st.varStore(lhs, AbsValue(res))
        val newExcSt = st.raiseException(excSet1 ++ excSet2 ++ excSet3)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_LOWER_CASE, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val str = v.pvalue.strval
        val lower = AbsStr.alpha(s => Str(s.str.toLowerCase))(AbsStr)(str)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(lower))
          else AbsState.Bot
        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TO_UPPER_CASE, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val str = v.pvalue.strval
        val upper = AbsStr.alpha(s => Str(s.str.toUpperCase))(AbsStr)(str)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(upper))
          else AbsState.Bot
        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TRIM, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val str = v.pvalue.strval
        val trimmed = AbsStr.alpha(s => Str(s.str.trim))(AbsStr)(str)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(trimmed))
          else AbsState.Bot
        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_BOOL_OBJ, List(expr), Some(aNew)) => {
        val (v, excSet) = V(expr, st)
        val bool = TypeConversionHelper.ToBoolean(v)
        val loc = Loc(aNew, tp)
        val st1 = st.alloc(loc)
        val heap = st1.heap.update(loc, AbsObj.newBooleanObj(bool))
        val st2 = st1.copy(heap = heap)
        val st3 =
          if (!v.isBottom) st2.varStore(lhs, AbsValue(loc))
          else AbsState.Bot
        val newExcSt = st.raiseException(excSet)
        (st3, newExcSt)
      }
      case (NodeUtil.INTERNAL_NUM_OBJ, List(expr), Some(aNew)) => {
        val (v, excSet) = V(expr, st)
        val num = TypeConversionHelper.ToNumber(v)
        val loc = Loc(aNew, tp)
        val st1 = st.alloc(loc)
        val heap = st1.heap.update(loc, AbsObj.newNumberObj(num))
        val st2 = st1.copy(heap = heap)
        val st3 =
          if (!v.isBottom) st2.varStore(lhs, AbsValue(loc))
          else AbsState.Bot
        val newExcSt = st.raiseException(excSet)
        (st3, newExcSt)
      }
      case (NodeUtil.INTERNAL_ABS, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).abs)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ACOS, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).acos)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ASIN, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).asin)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ATAN, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).atan)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ATAN_TWO, List(exprY, exprX), None) => {
        val (y, excSetY) = V(exprY, st)
        val (x, excSetX) = V(exprX, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(y, st.heap)
          .atan2(TypeConversionHelper.ToNumber(x, st.heap)))
        val st1 =
          if (!y.isBottom && !x.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSetX ++ excSetY)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_CEIL, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).ceil)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_COS, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).cos)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_EXP, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).exp)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_FLOOR, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).floor)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_LOG, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).log)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_POW, List(exprX, exprY), None) => {
        val (x, excSetX) = V(exprX, st)
        val (y, excSetY) = V(exprY, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(x, st.heap)
          .pow(TypeConversionHelper.ToNumber(y, st.heap)))
        val st1 =
          if (!x.isBottom && !y.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSetX ++ excSetY)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ROUND, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).round)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_SIN, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).sin)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_SQRT, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).sqrt)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_TAN, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val resV = AbsValue(TypeConversionHelper.ToNumber(v, st.heap).tan)
        val st1 =
          if (!v.isBottom) st.varStore(lhs, resV)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_REGEX_TEST, List(thisE, strE), None) => {
        val (thisV, excSet1) = V(thisE, st)
        val (strV, excSet2) = V(strE, st)
        val resV = (thisV.getSingle, strV.getSingle) match {
          case (ConOne(loc: Loc), ConOne(Str(arg))) =>
            val obj = st.heap.get(loc)
            (obj("source").value.getSingle, obj("flags").value.getSingle) match {
              case (ConOne(Str(source)), ConOne(Str(flags))) =>
                AbsBool(true == engine.eval(s"/$source/$flags.test('$arg');"))
              case _ => AbsBool.Top
            }
          case _ => AbsBool.Top
        }
        val st1 = st.varStore(lhs, resV)
        val newExcSt = st.raiseException(excSet1 ++ excSet2)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_IS_OBJ, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val st1 =
          if (!v.isBottom) {
            val b1 =
              if (!v.locset.isBottom) AT
              else AB
            val b2 =
              if (!v.pvalue.isBottom) AF
              else AB
            val boolVal = AbsValue(b1 ⊔ b2)
            st.varStore(lhs, boolVal)
          } else {
            AbsState.Bot
          }
        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ITER_INIT, List(expr), Some(aNew)) => {
        val (v, excSet1) = V(expr, st)
        val vObj = AbsValue(v.pvalue.copy(
          undefval = AbsUndef.Bot,
          nullval = AbsNull.Bot
        ), v.locset)
        val (locset, st1, excSet2) = TypeConversionHelper.ToObject(tp, vObj, st, aNew)
        val (locset2, st2) =
          if (v.pvalue.undefval.isTop || v.pvalue.nullval.isTop) {
            val heap = st.heap
            val newObj = heap.get(locset) ⊔ AbsObj.Empty
            val loc = Loc(aNew, tp)
            (locset + loc, st.copy(heap = st1.heap ⊔ heap.update(loc, newObj)))
          } else (locset, st1)
        val st3 = st2.varStore(lhs, AbsValue(AbsNum(0), locset2))
        val newExcSt = st.raiseException(excSet1 ++ excSet2)
        (st3, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ITER_HAS_NEXT, List(_, expr), None) => {
        val heap = st.heap
        val (v, excSet) = V(expr, st)
        val locset = v.locset
        val cur = v.pvalue.numval
        val boolV = cur.gamma match {
          case ConInf => AbsBool.Top
          case ConFin(idxSet) => idxSet.foldLeft(AbsBool.Bot) {
            case (b, idx) => locset.foldLeft(b) {
              case (b, loc) => {
                val (strList, astr) = heap.get(loc).keySetPair(heap)
                if (idx < strList.length) b ⊔ AbsBool.True
                else b ⊔ astr.fold(AbsBool.False) { _ => AbsBool.Top }
              }
            }
          }
        }
        val st1 = st.varStore(lhs, boolV)
        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ITER_NEXT, List(_, expr @ CFGVarRef(_, id)), None) => {
        val heap = st.heap
        val (v, excSet) = V(expr, st)
        val locset = v.locset
        val cur = v.pvalue.numval
        val strV = locset.foldLeft(AbsStr.Bot) {
          case (str, loc) => {
            val obj = heap.get(loc)
            val (strList, astr) = heap.get(loc).keySetPair(heap)
            cur.gamma match {
              case ConInf => str ⊔ AbsStr(strList.toSet) ⊔ astr
              case ConFin(idxSet) => idxSet.foldLeft(str) {
                case (str, Num(idx)) => {
                  if (idx < strList.length) str ⊔ AbsStr(strList(idx.toInt))
                  else str ⊔ astr
                }
              }
            }
          }
        }
        val st1 = st.varStore(lhs, strV)
        val next = AbsValue(cur + AbsNum(1), locset)
        val st2 = st1.varStore(id, next)
        val newExcSt = st.raiseException(excSet)
        (st2, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_ADD_EVENT_FUNC, List(exprV), None) => {
        val (v, excSetV) = V(exprV, st)
        val id = NodeUtil.getInternalVarId(NodeUtil.INTERNAL_EVENT_FUNC)
        val (curV, _) = st.lookup(id)
        val newSt = st.varStore(id, curV.locset ⊔ v.locset)
        val newExcSt = st.raiseException(excSetV)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_GET_LOC, List(exprV), None) => {
        val (v, excSetV) = V(exprV, st)
        val locset = v.pvalue.strval.gamma match {
          case ConInf => LocSet.Top
          case ConFin(strset) => LocSet(strset.map(str => Loc(str)))
        }
        val newSt = st.varStore(lhs, locset)
        val newExcSt = st.raiseException(excSetV)
        (newSt, excSt ⊔ newExcSt)
      }

      // @TargetFunction(func, target)
      // writes the [[TargetFunction]] property of `func` to be `target`.
      // the target function is defined in the ES spec, and references the "bindee" of a bound function.
      // (i.e., the target function of the bound function `f.bind(obj)` is `f`)
      case (NodeUtil.INTERNAL_TARGET_FUN, List(exprO, exprP), None) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)
        val newH = v.locset.foldLeft(st.heap) {
          case (h, loc) => {
            val obj = st.heap.get(loc)
            val newObj = obj.update(ITargetFunction, AbsIValue(p))
            h.update(loc, newObj)
          }
        }
        val newSt = st.copy(heap = newH).varStore(lhs, p)
        val newExcSt = st.raiseException(excSetO ++ excSetP)
        (newSt, excSt ⊔ newExcSt)
      }

      // @TargetFunction(func)
      // reads the [[TargetFunction]] property of `func`.
      case (NodeUtil.INTERNAL_TARGET_FUN, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val obj = st.heap.get(v.locset)
        val value = obj(ITargetFunction).value
        val st1 =
          if (!v.isBottom) st.varStore(lhs, value)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }

      // @BoundThis(func, thisArg)
      // writes the [[BoundThis]] property of `func` to be `thisArg`.
      case (NodeUtil.INTERNAL_BOUND_THIS, List(funcExpr, thisExpr), None) => {
        // the two arguments to @BoundThis can only be assumed to be expressions, rather than single values.
        // so first, we simplify these expressions down to single values using the `V` method.
        // note that we potentially run into exceptions while doing this, which are also returned by `V`.
        val (funcVal, funcExcSet) = V(funcExpr, st)
        val (thisVal, thisExcSet) = V(thisExpr, st)

        // for each possible heap location of the `funcVal`:
        val newH = funcVal.locset.foldLeft(st.heap) {
          case (h, loc) => {
            // read the object at that heap location
            val obj = st.heap.get(loc)
            // update its [[BoundThis]] internal property to `thisVal`
            val newObj = obj.update(IBoundThis, AbsIValue(thisVal))
            // write the updated object back to the heap in the same location
            h.update(loc, newObj)
          }
        }
        val newSt = st
          // write the updated heap into the program state
          .copy(heap = newH)
          // write the `thisVal` to the "left-hand side" of the function call
          // (this is equivalent to its return value)
          // even if the return value isn't used by the program under analysis,
          // the way that SAFE's CFG works means it makes sense to save it anyway.
          .varStore(lhs, thisVal)

        // raise the possible exceptions created by this function call
        val newExcSt = st.raiseException(funcExcSet ++ thisExcSet)
        (newSt, excSt ⊔ newExcSt)
      }

      // @BoundThis(func)
      // retrieve the `[[BoundThis]]` value of the argument `func`.
      case (NodeUtil.INTERNAL_BOUND_THIS, List(funcExpr), None) => {
        val (v, excSet) = V(funcExpr, st)
        val obj = st.heap.get(v.locset)
        val value = obj(IBoundThis).value
        val st1 =
          if (!v.isBottom) st.varStore(lhs, value)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_BOUND_ARGS, List(exprO, exprP), None) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)
        val newH = v.locset.foldLeft(st.heap) {
          case (h, loc) => {
            val obj = st.heap.get(loc)
            val newObj = obj.update(IBoundArgs, AbsIValue(p))
            h.update(loc, newObj)
          }
        }
        val newSt = st.copy(heap = newH).varStore(lhs, p)
        val newExcSt = st.raiseException(excSetO ++ excSetP)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_BOUND_ARGS, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val obj = st.heap.get(v.locset)
        val value = obj(IBoundArgs).value
        val st1 =
          if (!v.isBottom) st.varStore(lhs, value)
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }

      // seems to be unused; normal usage of @Call involves three arguments, the third of which is an argument list
      case (NodeUtil.INTERNAL_CALL, List(exprO, exprP), None) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)

        val obj = st.heap.get(p.locset)
        val fidset = obj(ICall).fidset

        val newH = v.locset.foldLeft(st.heap) {
          case (h, loc) => {
            val obj = st.heap.get(loc)
            val newObj = obj.update(ICall, fidset)
            h.update(loc, newObj)
          }
        }

        val newSt = st.copy(heap = newH).varStore(lhs, p)
        val newExcSt = st.raiseException(excSetO ++ excSetP)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_CONSTRUCT, List(exprO, exprP), None) => {
        val (v, excSetO) = V(exprO, st)
        val (p, excSetP) = V(exprP, st)

        val obj = st.heap.get(p.locset)
        val cidset = obj(IConstruct).fidset

        val newH = v.locset.foldLeft(st.heap) {
          case (h, loc) => {
            val obj = st.heap.get(loc)
            val newObj = obj.update(IConstruct, cidset)
            h.update(loc, newObj)
          }
        }

        val newSt = st.copy(heap = newH).varStore(lhs, p)
        val newExcSt = st.raiseException(excSetO ++ excSetP)
        (newSt, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_HAS_CONST, List(expr), None) => {
        val (v, excSet) = V(expr, st)
        val obj = st.heap.get(v.locset)
        val isDomIn = obj.fold(AbsBool.False) { obj => (obj contains IConstruct) }
        val b1 =
          if (AbsBool.True ⊑ isDomIn) AbsBool.True
          else AbsBool.Bot
        val b2 =
          if (AbsBool.False ⊑ isDomIn) AbsBool.False
          else AbsBool.Bot

        val st1 =
          if (!v.isBottom) st.varStore(lhs, AbsValue(b1 ⊔ b2))
          else AbsState.Bot

        val newExcSt = st.raiseException(excSet)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_MAX2, List(e1, e2), None) => {
        val (v1, es1) = V(e1, st)
        val (v2, es2) = V(e2, st)
        val n1 = v1.pvalue.numval
        val n2 = v2.pvalue.numval
        val b = n1 > n2
        val res1 = if (AT ⊑ b) n1 else AbsNum.Bot
        val res2 = if (AF ⊑ b) n2 else AbsNum.Bot
        val st1 = st.varStore(lhs, AbsValue(res1 ⊔ res2))
        val newExcSt = st.raiseException(es1 ++ es2)
        (st1, excSt ⊔ newExcSt)
      }
      case (NodeUtil.INTERNAL_MIN2, List(e1, e2), None) => {
        val (v1, es1) = V(e1, st)
        val (v2, es2) = V(e2, st)
        val n1 = v1.pvalue.numval
        val n2 = v2.pvalue.numval
        val b = n1 < n2
        val res1 = if (AT ⊑ b) n1 else AbsNum.Bot
        val res2 = if (AF ⊑ b) n2 else AbsNum.Bot
        val st1 = st.varStore(lhs, AbsValue(res1 ⊔ res2))
        val newExcSt = st.raiseException(es1 ++ es2)
        (st1, excSt ⊔ newExcSt)
      }
      case _ =>
        excLog.signal(IRSemanticsNotYetImplementedError(ir))
        (AbsState.Bot, AbsState.Bot)
    }
  }

  // external call instruction
  // to be used by consumers of these semantics; not used during normal analysis pipeline
  def CI(cp: ControlPoint, i: CFGCallInst, st: AbsState, excSt: AbsState): (AbsState, AbsState) = {
    val (_, _, s, e) = internalCI(cp, i, st, excSt)
    (s, e)
  }

  // bind a nonnull `name` to the input environment record.
  // if the input `name` is null, return the original environment record.
  def bindNonnullName(envRec: AbsDecEnvRec, name: String, value: AbsValue): AbsDecEnvRec = {
    if (name == null) {
      envRec
    } else {
      val (result, _) = envRec.CreateMutableBinding(name)
        .SetMutableBinding(name, value)
      result
    }
  }

  // implements the transfer function of a call instruction.
  // returns (value of this, value of args, exit state, exit exception state)
  // `cp`: the control point containing the instruction's `Call` block
  // `i`: the call instruction being executed
  // `st`: the incoming normal program state
  // `excSt`: the incoming exceptional program state
  def internalCI(cp: ControlPoint, i: CFGCallInst, st: AbsState, excSt: AbsState): (AbsValue, AbsValue, AbsState, AbsState) = {
    // cons, thisArg and arguments must not be bottom
    val tp = cp.tracePartition
    val loc = Loc(i.asite, tp)
    val st1 = st.alloc(loc)
    val (funVal, funExcSet) = V(i.fun, st1)
    //println("funVal: " + funVal)
    //funVal.locset.foreach(loc => println("loc: " + loc))

    // compute the possible locations of the function being called based on the type of call instruction.
    val funLocSet = i match {
      // if it's a constructor (i.e. called with `new`), only use locations which may contain a constructor.
      case (_: CFGConstruct) => funVal.locset.filter(l => AT ⊑ st1.heap.hasConstruct(l))
      // if it's a normal function call, only use locations which may be callable.
      case (_: CFGCall) => funVal.locset.filter(l => AT ⊑ TypeConversionHelper.IsCallable(l, st1.heap))
    }

    // compute the abstract values of `this` and `arguments`
    val (thisArgVal, _) = V(i.thisArg, st1)
    val (argVal, _) = V(i.arguments, st1)

    // XXX: stop if thisArg or arguments is LocSetBot(ValueBot)
    if (thisArgVal.isBottom || argVal.isBottom) {
      (AbsValue.Bot, AbsValue.Bot, st, excSt)
    } else {
      val oldLocalEnv = st1.context.pureLocal
      val tp = cp.tracePartition
      val nCall = i.block
      val cpAfterCall = ControlPoint(nCall.afterCall, tp)
      val cpAfterCatch = ControlPoint(nCall.afterCatch, tp)

      // Draw call/return edges
      // for each possible callee location:
      funLocSet.foreach((fLoc) => {
        // compute the location's fidset based on the type of call instruction.
        // (if a constructor call, use the fidset of constructors; if a normal call, use the fidset of callables)
        val funObj = st1.heap.get(fLoc)
        val fidSet = i match {
          case _: CFGConstruct =>
            funObj(IConstruct).fidset
          case _: CFGCall =>
            funObj(ICall).fidset
        }

        // for each possible callee fid:
        fidSet.foreach((fid) => {
          // find the function associated to the callee fid in the CFG.
          cfg.getFunc(fid) match {
            case Some(funCFG) => {
              val scopeValue = funObj(IScope).value

              // create a new local lexical environment for the callee
              val newEnv = AbsLexEnv.newPureLocal(LocSet(loc))

              // bind the `arguments` and `@scope` names to the callee's new lexical environment
              val newRec = bindNonnullName(newEnv.record.decEnvRec, funCFG.argumentsName, argVal)
              val newRec2 = bindNonnullName(newRec, "@scope", scopeValue)

              // use `cp.next` to compute all possible control points at the callee's `Entry` block
              // across the call edge we're analyzing.
              // for each such entry control point of a callee:
              cp.next(funCFG.entry, CFGEdgeCall, this, st1).foreach(entryCP => {
                val newTP = entryCP.tracePartition

                // compute control points at the callee's `Exit` and `ExitExc` blocks which
                // correspond to the newly-found control point at the `Entry` block.
                val exitCP = ControlPoint(funCFG.exit, newTP)
                val exitExcCP = ControlPoint(funCFG.exitExc, newTP)

                // inherit the `thisBinding` in an arrow function from the caller's scope.
                // this isn't accurate yet, but it might be close enough.
                val thisVal = if (funCFG.isArrow) {
                  getState(cp).context.thisBinding
                } else {
                  thisArgVal
                }

                val data = EdgeData(
                  AllocLocSet.Empty,
                  newEnv.copy(record = newRec2),
                  thisVal
                )

                // each function call induces the following three interprocedural edges:

                // 1. from the caller's `Call` block to the callee's `Entry` block.
                addIPEdge(cp, entryCP, data)

                // 2. from the callee's `Exit` block to the caller's `AfterCall` block.
                addIPEdge(exitCP, cpAfterCall, EdgeData(
                  st1.allocs,
                  oldLocalEnv,
                  st1.context.thisBinding
                ))

                // 3. from the callee's `ExitExc` block to the caller's `AfterCatch` block.
                addIPEdge(exitExcCP, cpAfterCatch, EdgeData(
                  st1.allocs,
                  oldLocalEnv,
                  st1.context.thisBinding
                ))
              })
            }

            // this case would be hit when there's no `CFGFunction` corresponding to
            // a callee's `fid` in the `fidSet`.
            // (this shouldn't ever happen, and would indicate a bug in the analysis algorithm.)
            case None => excLog.signal(UndefinedFunctionCallError(i.ir))
          }
        })
      })

      val h2 = argVal.locset.foldLeft[AbsHeap](AbsHeap.Bot)((tmpHeap, l) => {
        val argObj = st1.heap.get(l)
        tmpHeap ⊔ st1.heap.update(l, argObj.update("callee", AbsDataProp(funLocSet, AT, AF, AT)))
      })

      // exception handling
      val typeExcSet1 = i match {
        case _: CFGConstruct if funVal.locset.exists(l => AF ⊑ st1.heap.hasConstruct(l)) => Set(TypeError)
        case _: CFGCall if funVal.locset.exists(l => AF ⊑ TypeConversionHelper.IsCallable(l, st1.heap)) => Set(TypeError)
        case _ => ExcSetEmpty
      }
      val typeExcSet2 =
        if (!funVal.pvalue.isBottom) Set(TypeError)
        else ExcSetEmpty

      val totalExcSet = funExcSet ++ typeExcSet1 ++ typeExcSet2
      val newExcSt = st1.raiseException(totalExcSet)

      val h3 =
        if (!funLocSet.isBottom) h2
        else AbsHeap.Bot

      val newSt = st1.copy(heap = h3)
      (thisArgVal, argVal, newSt, excSt ⊔ newExcSt)
    }
  }

  // compute the abstract value of `expr` in the abstract state `st`.
  // returns a value and a set of possible exceptions occurring during computation.
  def V(expr: CFGExpr, st: AbsState): (AbsValue, Set[Exception]) = expr match {
    case CFGVarRef(ir, id) => st.lookup(id)
    case CFGLoad(ir, obj, index) => {
      val (objV, _) = V(obj, st)
      val (idxV, idxExcSet) = V(index, st)
      val absStrSet =
        if (!idxV.isBottom) TypeConversionHelper.ToPrimitive(idxV, st.heap).toStringSet
        else Set[AbsStr]()
      val v1 = Helper.propLoad(objV, absStrSet, st.heap)
      (v1, idxExcSet)
    }
    case CFGThis(ir) =>
      (st.context.thisBinding, ExcSetEmpty)
    case CFGBin(ir, expr1, op, expr2) => {
      val (v1, excSet1) = V(expr1, st)
      val (v2, excSet2) = V(expr2, st)
      (v1, v2) match {
        case _ if v1.isBottom => (AbsValue.Bot, excSet1)
        case _ if v2.isBottom => (AbsValue.Bot, excSet1 ++ excSet2)
        case _ =>
          val h = st.heap
          op.name match {
            case "|" => (Helper.bopBitOr(v1, v2), excSet1 ++ excSet2)
            case "&" => (Helper.bopBitAnd(v1, v2), excSet1 ++ excSet2)
            case "^" => (Helper.bopBitXor(v1, v2), excSet1 ++ excSet2)
            case "<<" => (Helper.bopLShift(v1, v2), excSet1 ++ excSet2)
            case ">>" => (Helper.bopRShift(v1, v2), excSet1 ++ excSet2)
            case ">>>" => (Helper.bopURShift(v1, v2), excSet1 ++ excSet2)
            case "+" => (Helper.bopPlus(v1, v2), excSet1 ++ excSet2)
            case "-" => (Helper.bopMinus(v1, v2), excSet1 ++ excSet2)
            case "*" => (Helper.bopMul(v1, v2), excSet1 ++ excSet2)
            case "/" => (Helper.bopDiv(v1, v2), excSet1 ++ excSet2)
            case "%" => (Helper.bopMod(v1, v2), excSet1 ++ excSet2)
            case "==" => (Helper.bopEqBetter(h, v1, v2), excSet1 ++ excSet2)
            case "!=" => (Helper.bopNeq(h, v1, v2), excSet1 ++ excSet2)
            case "===" => (Helper.bopSEq(h, v1, v2), excSet1 ++ excSet2)
            case "!==" => (Helper.bopSNeq(h, v1, v2), excSet1 ++ excSet2)
            case "<" => (Helper.bopLess(v1, v2), excSet1 ++ excSet2)
            case ">" => (Helper.bopGreater(v1, v2), excSet1 ++ excSet2)
            case "<=" => (Helper.bopLessEq(v1, v2), excSet1 ++ excSet2)
            case ">=" => (Helper.bopGreaterEq(v1, v2), excSet1 ++ excSet2)
            case "instanceof" =>
              val locSet1 = v1.locset
              val locSet2 = v2.locset
              val locSet3 = locSet2.filter((l) => AT ⊑ st.heap.hasInstance(l))
              val protoVal = locSet3.foldLeft(AbsValue.Bot)((v, l) => {
                v ⊔ st.heap.get(l).Get("prototype", st.heap)
              })
              val locSet4 = protoVal.locset
              val locSet5 = locSet2.filter((l) => AF ⊑ st.heap.hasInstance(l))
              val b1 = locSet1.foldLeft[AbsValue](AbsValue.Bot)((tmpVal1, loc1) => {
                locSet4.foldLeft[AbsValue](tmpVal1)((tmpVal2, loc2) =>
                  tmpVal2 ⊔ Helper.inherit(st.heap, loc1, loc2))
              })
              val b2 =
                if (!v1.pvalue.isBottom && !locSet4.isBottom) AbsValue(AF)
                else AbsValue.Bot
              val excSet3 =
                if (!v2.pvalue.isBottom || !locSet5.isBottom || !protoVal.pvalue.isBottom) Set(TypeError)
                else ExcSetEmpty
              val b = b1 ⊔ b2
              val excSet = excSet1 ++ excSet2 ++ excSet3
              (b, excSet)
            case "in" => {
              val str = TypeConversionHelper.ToString(v1, st.heap)
              val absB = v2.locset.foldLeft(AB)((tmpAbsB, loc) => {
                tmpAbsB ⊔ st.heap.get(loc).HasProperty(str, st.heap)
              })
              val b = AbsValue(absB)
              val excSet3 =
                if (!v2.pvalue.isBottom) Set(TypeError)
                else ExcSetEmpty
              val excSet = excSet1 ++ excSet2 ++ excSet3
              (b, excSet)
            }
          }
      }
    }
    case CFGUn(ir, op, expr) => {
      val (v, excSet) = V(expr, st)
      op.name match {
        case "void" => (Helper.uVoid(v), excSet)
        case "+" => (Helper.uopPlus(v), excSet)
        case "-" => (Helper.uopMinusBetter(st.heap, v), excSet)
        case "~" => (Helper.uopBitNeg(v), excSet)
        case "!" => (Helper.uopNeg(v), excSet)
        case "typeof" =>
          expr match {
            case CFGVarRef(_, x) =>
              val absStr1 = TypeConversionHelper.typeTag(v, st.heap)
              val absStr2 =
                if (excSet.contains(ReferenceError)) AbsStr("undefined")
                else AbsStr.Bot
              val absStr = absStr1 ⊔ absStr2
              (AbsValue(absStr), ExcSetEmpty)
            case _ =>
              val absStr = TypeConversionHelper.typeTag(v, st.heap)
              (AbsValue(absStr), excSet)
          }
      }
    }
    case CFGInternalValue(ir, name) => getInternalValue(name) match {
      case Some(value) => (value, ExcSetEmpty)
      case None =>
        excLog.signal(IRSemanticsNotYetImplementedError(ir))
        (AbsValue.Bot, ExcSetEmpty)
    }
    case CFGVal(ejsVal) =>
      val pvalue: AbsPValue = ejsVal match {
        case EJSNumber(_, num) => AbsPValue(num)
        case EJSString(str) => AbsPValue(str)
        case EJSBool(bool) => AbsPValue(bool)
        case EJSNull => AbsPValue(Null)
        case EJSUndef => AbsPValue(Undef)
      }
      (AbsValue(pvalue), ExcSetEmpty)
  }

  // throws an exception unless `expr` is truthy in `st`, as in an `assert` statement.
  def B(expr: CFGExpr, st: AbsState, excSt: AbsState): (AbsState, AbsState) = {
    val st1 = st //TODO should be the pruned state

    val (v, excSet) = V(expr, st)
    val newExcSt = st.raiseException(excSet)
    val st2 =
      if (AbsBool(true) ⊑ TypeConversionHelper.ToBoolean(v)) st1
      else AbsState.Bot

    (st2, excSt ⊔ newExcSt)
  }
}

// Interprocedural edges
case class EdgeData(allocs: AllocLocSet, env: AbsLexEnv, thisBinding: AbsValue) {
  def ⊔(other: EdgeData): EdgeData = EdgeData(
    this.allocs ⊔ other.allocs,
    this.env ⊔ other.env,
    this.thisBinding ⊔ other.thisBinding
  )
  def ⊑(other: EdgeData): Boolean = {
    (this.allocs ⊑ other.allocs) &&
      (this.env ⊑ other.env) &&
      (this.thisBinding ⊑ other.thisBinding)
  }

  def subsLoc(from: Loc, to: Loc): EdgeData = EdgeData(
    allocs.subsLoc(from, to),
    env.subsLoc(from, to),
    thisBinding.subsLoc(from, to)
  )

  def weakSubsLoc(from: Loc, to: Loc): EdgeData = EdgeData(
    allocs.weakSubsLoc(from, to),
    env.weakSubsLoc(from, to),
    thisBinding.weakSubsLoc(from, to)
  )

  def fix(given: AllocLocSet): EdgeData = given.mayAlloc.foldLeft(this) {
    case (data, loc) => {
      val EdgeData(allocs, env, thisBinding) = loc match {
        case locR @ Recency(l, Recent) => {
          val locO = Recency(l, Old)
          if (given.mustAlloc contains locR) data.subsLoc(locR, locO)
          else data.weakSubsLoc(locR, locO)
        }
        case _ => data
      }
      val newAllocs =
        if (given.mustAlloc contains loc) allocs.alloc(loc)
        else allocs.weakAlloc(loc)
      EdgeData(newAllocs, env, thisBinding)
    }
  }
}
object EdgeData {
  val Bot: EdgeData = EdgeData(AllocLocSet.Bot, AbsLexEnv.Bot, AbsValue.Bot)
}

// call infomation
case class CallInfo(state: AbsState, thisVal: AbsValue, argVal: AbsValue) {
  override def toString: String = {
    s"this: ${thisVal}, args: ${argVal}"
  }
}

// an exported value can either be an expression or a variable (identifier)
sealed trait AbsExportValue {
  def getValue(sem: Semantics, cp: ControlPoint): (AbsValue, Set[Exception])
}

case class ExportedExpr(expr: CFGExpr) extends AbsExportValue {
  override def getValue(sem: Semantics, cp: ControlPoint): (AbsValue, Set[Exception]) =
    sem.V(expr, sem.getState(cp))

  override def toString = expr.toString(0)
}

case class ExportedId(id: CFGId) extends AbsExportValue {
  override def getValue(sem: Semantics, cp: ControlPoint): (AbsValue, Set[Exception]) =
    sem.getState(cp).lookup(id)

  override def toString = id.toString
}
