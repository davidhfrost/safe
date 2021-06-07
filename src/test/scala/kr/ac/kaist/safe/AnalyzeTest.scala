/**
 * *****************************************************************************
 * Copyright (c) 2016-2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe

import java.io._
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.model._
import kr.ac.kaist.safe.errors.error.ParserError
import kr.ac.kaist.safe.nodes.cfg.CFG
import kr.ac.kaist.safe.phase._
import kr.ac.kaist.safe.util._
import org.scalatest._
import scala.util.Random.shuffle
import scala.util.{ Failure, Success, Try }

abstract class AnalyzeTest extends SafeTest {
  // analysis configuration
  val heapBuildConfig = HeapBuild.defaultConfig
  val analyzeConfig = Analyze.defaultConfig
  val configFile = CONFIG_FILE

  // a predicate "generator" to check if an abstract string value starts with `prefix`.
  // (e.g. `prefixCheck("__result")` is a predicate to check if an abstract string value starts with `__result`).
  // this is used with `getAbsKeySet` to find the assertion variables in a test file's program state.
  def prefixCheck(prefix: String): (AbsStr, AbsDataProp) => Boolean = {
    // we can think of `(str, dp)` as a key-value pair representing a variable: its name (str) and its value (dp).
    // since `str` is an *abstract* string, we need to be careful about how we check that it starts with `prefix`.

    // for our purposes, since we're the ones writing the test files, we can assume the best case scenario;
    // namely, that `str` is a constant string, or a "single" concrete string value.
    // `str.getSingle` extracts this single string value from the abstract `str`.
    case (str, dp) => str.getSingle match {
      // once we have our single concrete string value, we can check that it starts with `prefix`,
      // since now we're finally dealing with an ordinary string.
      case ConOne(Str(concreteStr)) => concreteStr.startsWith(prefix)
      case _ => false
    }
  }

  // returns a set of keys of the abstract JS object `obj` that
  // satisfy the predicate `ppred`.
  // in particular, we will apply this with a `prefixCheck` predicate
  // that checks if the key name starts with "__result" or "__expect".
  def getAbsKeySet(
    obj: AbsObj,
    ppred: (AbsStr, AbsDataProp) => Boolean
  ): Set[String] = {
    obj.abstractKeySet(ppred) match {
      case ConInf => fail()
      case ConFin(set) => set.map(_.getSingle match {
        case ConOne(Str(str)) => str
        case _ => assert(false); ""
      })
    }
  }

  // initialization for tests
  HeapBuild.jscache = {
    val parser = new ArgParser(CmdBase, safeConfig)
    parser.addRule(heapBuildConfig, HeapBuild.name, HeapBuild.options)
    parser.addRule(analyzeConfig, Analyze.name, Analyze.options)
    parser(List(s"-config=$configFile"))

    register(
      heapBuildConfig.AbsUndef,
      heapBuildConfig.AbsNull,
      heapBuildConfig.AbsBool,
      heapBuildConfig.AbsNum,
      heapBuildConfig.AbsStr,
      heapBuildConfig.recencyMode,
      heapBuildConfig.heapClone,
      heapBuildConfig.callsiteSensitivity *
        heapBuildConfig.loopSensitivity
    )
    Some(Model.parseDir(NodeUtil.jsModelsBase))
  }

  // runs the default `cfgBuild` command on the source code file located at `filename`.
  // the `cfgBuild` command consists of the following four phases: `Parse, ASTRewrite, Translate, CFGBuild`.
  // in particular, note the distinction between the `cfgBuild` command and the `CFGBuild` phase.
  // the former command needs to run all four of the above phases, since the `CFGBuild` phase
  // relies on the output of the previous three phases.
  def getCFG(filename: String): Try[CFG] = CmdCFGBuild(List("-silent", filename), testMode = true)

  // these constant strings define the variable prefixes for the two halves of assertions
  val resultPrefix = "__result"
  val expectPrefix = "__expect"
  def analyzeTest(analysis: Try[(CFG, Int, TracePartition, Semantics)]): Unit = analysis match {
    case Failure(e) => throw e
    // the tuple `(cfg, iter, globalTP, sem)` is the output of a successful `Analyze` phase.
    // most important are:
    //   - `cfg`, the program's CFG computed in the `CFGBuild` phase, and
    //   - `sem`, the program semantics, which contains the program state we can use to check
    //     that all test assertions were satisfied.
    case Success((cfg, iter, globalTP, sem)) =>
      // SAFE represents the file-scope level of a source code file as a "global function",
      // identified by `cfg.globalFunc`.
      // every function, including the global function, has an "exit" block, representing the end of that function.
      // thus, `cfg.globalFunc.exit` represents the exit block, or end, of the file.
      val finalProgramPoint = ControlPoint(cfg.globalFunc.exit, globalTP)

      // read the program state at the final program point from `sem`, which contains the analysis results.
      val normalSt = sem.getState(finalProgramPoint)
      val excSt = sem.getState(ControlPoint(cfg.globalFunc.exitExc, globalTP))

      // `assert` is a Scala test method which verifies that its argument is true (as a boolean).
      // thus, the following line is verifying that the program state's heap isn't the "bottom" value.
      // in the general context of abstract interpretation, the "bottom" value refers to an
      // "impossible" value or an "error state".
      // so this line's effectively just checking that our analysis didn't wind up crashing.
      // it's something we'd want to check first, before we start looking at individual variable values
      // within our computed program state (which can't be checked if the analysis crashed somewhere).
      assert(!normalSt.heap.isBottom)

      // `GLOBAL_LOC` is the location of the global file-scope level object in a JS program.
      // this object will contain, in particular, all the variables defined in the file scope,
      // including the `__result` and `__expect` variables that comprise our assertions.
      // we're simply reading this object out of the program state's heap.
      normalSt.heap.get(GLOBAL_LOC) match {
        case globalObj => {
          // again, if our `globalObj` is "bottom", that means something crashed, and we better fail the test.
          // `fail` being called here is another built-in Scala test method which cause the current test to fail.
          if (globalObj.isBottom) fail()

          // read out all global variables in the test file whose names start with `__result`.
          // (see the definitions of `getAbsKeySet` and `prefixCheck` for annotations of those functions)
          val resultKeySet =
            getAbsKeySet(globalObj, prefixCheck(resultPrefix))

          // read out all global variables in the test file whose names start with `__expect`
          val expectKeySet =
            getAbsKeySet(globalObj, prefixCheck(expectPrefix))

          // for each variable name starting with `__result`:
          resultKeySet.foreach(resultKey => {
            // read out the "suffix" of the variable name, which encodes the assertion number
            // (e.g. for the variable `__result5`, the suffix `num` would be 5).
            val num = resultKey.substring(resultPrefix.length)

            // use `num` to compute the `__expect` variable corresponding to `resultKey`
            val expectKey = expectPrefix + num

            // first make sure that such an `__expect` variable was defined at all.
            assert(expectKeySet contains expectKey)

            // finally, read out the values of the two assertion variables from `globalObj`:
            // 1. `expectKey` is the name of the `__expect` variable half of an assertion.
            // 2. the call `globalObj(expectKey)` is reading that variable's value from the global object.
            //    (this specific notation is enabled by overloading the `apply` method of `globalObj`,
            //     a common trick in SAFE, which lets us define what happens when we "call" something like a function).
            // 3. similarly, `globalObj(resultKey)` is the `__result` variable's value in the global object.
            // 4. since these are *abstract* values, we again have to be careful how we compare them.
            //    the `⊑` operator used here is analagous to (and is intentionally visually similar to) the "subset" operator in math.
            //    when we're comparing two abstract values, "a ⊑ b" essentially means "a is more precise than b".
            //    what this means specifically will depend on the abstract domain that "a" and "b" belong to.
            //    if they're constant values (which I'd assume they almost always are in our tests), this is equivalent
            //    to checking that they're the same value.
            assert(globalObj(expectKey) ⊑ globalObj(resultKey))
          })
        }
      }
  }

  // this is the main "driver" function that runs the test files.
  // each of the subclasses of `AnalyzeTest` (e.g. `Test262AnalyzeTest`) calls this
  def analyzeHelper(prefix: String, dirs: List[String]): Unit = dirs.foreach(dir => {
    // `walkTree` searches the input directory `dir` for all test files nested within that directory,
    // returning a collection of all such files.
    val testFiles: Iterable[File] = walkTree(new File(dir))

    // note the use of `shuffle` here to randomly permute the list `testFiles`.
    // it's not clear to me what the significance of this is, but by randomly permuting the
    // order that test files are run, we would at least detect if our test results did depend
    // on that test order.
    for (file <- shuffle(testFiles)) {
      // for each test file object `file`:
      val filename = file.getName
      lazy val name = file.toString

      // a few things to note here:
      // 1. `safeConfig` is the skeleton configuration for all tests.
      //    its main feature is that the `testMode` field is set to true.
      // 2. the `copy` method used here is implicit to all `case class` objects in Scala.
      //    (see https://docs.scala-lang.org/tour/case-classes.html)
      // 3. from 1 and 2, we see that `config` is a SAFE configuration where `testMode` is on
      //    and the only file to be analyzed is that corresponding to this iteration's test file.
      lazy val config = safeConfig.copy(fileNames = List(name))

      // check if `file` is a JS or HTML file based on its `.js` or `.html` extension.
      if (jsFilter(filename) || htmlFilter(filename)) {
        // this block of code performs an equivalent of the terminal command `safe analyze [filename]`.
        // this command has six phases: `Parse >> ASTRewrite >> Translate >> CFGBuild >> HeapBuild >> Analyze`.
        // they're split up below into 3 separate computations as labelled below.

        // (this seems to be split up in order to inject the test-specific configuration that's defined
        //  above in this file, but all of this configuration appears to me to be the default anyway, so I'm
        //  not entirely sure why splitting the analysis up into three steps was necessary.)

        // 1. run the phases `Parse >> ASTRewrite >> Translate >> CFGBuild` on this iteration's test file.
        // this produces a `CFG` representing that test file.
        lazy val cfg = getCFG(name)

        // 2. run the `HeapBuild` phase on the previous computation's CFG output.
        //    the value `heapBuild` is essentially an initial program state, which notably
        //    includes built-in JS functions (string methods, math methods, etc.) that are
        //    necessary to accurately model the JS environment in a browser.

        //    the name "heap build" is referring to building the program's initial heap
        //    out of this built-in environment.
        lazy val heapBuild = cfg.flatMap(HeapBuild(_, config, heapBuildConfig))

        // 3. run the `Analyze` phase on the previous computation's output.
        //    starting from the initial program state described above, the `Analyze` phase performs
        //    abstract interpretation to approximate the program's state at each point of its execution.
        //    in particular, the analysis results will contain the values of each variable, from which we can
        //    decide if the test file's assertions were satisfied or not.
        lazy val analysis = heapBuild.flatMap(Analyze(_, config, analyzeConfig))

        // finally, use the `analyzeTest` method above to verify that the analysis results
        // satisfy the assertions defined in the test files.
        test(s"[$prefix] $filename") { analyzeTest(analysis) }
      } else if (errFilter(filename)) {
        test(s"[$prefix] $filename") {
          CmdParse(List("-silent", name), testMode = true) match {
            case Failure(ParserError(_, _)) =>
            case _ => fail()
          }
        }
      }
    }
  })
}
