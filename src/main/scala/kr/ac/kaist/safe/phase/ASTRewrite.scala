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

import scala.util.{ Success, Try }
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.ast_rewriter.{ ClassRewriter, Disambiguator, Hoister, ModuleRewriter, WithRewriter }
import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.nodes.ast.Program
import kr.ac.kaist.safe.util._

// ASTRewrite phase
case object ASTRewrite extends PhaseObj[Program, ASTRewriteConfig, (Program, Program)] {
  val name: String = "astRewriter"
  val help: String =
    "Rewrites AST in JavaScript source files (hoister, disambiguator, withRewriter)"

  def apply(
    inputProgram: Program,
    safeConfig: SafeConfig,
    config: ASTRewriteConfig
  ): Try[(Program, Program)] = {
    val (rewrittenProgram, excLog) = rewrite(safeConfig, inputProgram)

    // Report errors.
    if (excLog.hasError && !safeConfig.testMode && !safeConfig.silent) {
      println(rewrittenProgram.relFileName + ":")
      println(excLog)
    }

    // Pretty print to file.
    config.outFile match {
      case Some(out) => {
        val ((fw, writer)) = Useful.fileNameToWriters(out)
        writer.write(rewrittenProgram.toString(0))
        writer.close; fw.close
        println("Dumped rewritten AST to " + out)
      }
      case None => return Try((inputProgram, rewrittenProgram))
    }

    Success((inputProgram, rewrittenProgram))
  }

  def rewrite(safeConfig: SafeConfig, pgm: Program): (Program, ExcLog) = {
    var program = if (safeConfig.fileNames.length > 0) {
      val moduleRewriter = new ModuleRewriter(safeConfig.fileNames.head, pgm)
      moduleRewriter.result
    } else {
      pgm
    }

    val classRewriter = new ClassRewriter(program)
    program = classRewriter.result

    // hoist
    val hoister = new Hoister(program)
    program = hoister.result
    var excLog = hoister.excLog

    // disambiguate
    val disambiguator = new Disambiguator(program)
    program = disambiguator.result
    excLog += disambiguator.excLog

    // "with" rewrite
    val withRewriter = new WithRewriter(program, false)
    program = withRewriter.result
    excLog += withRewriter.excLog

    (program, excLog)
  }

  def defaultConfig: ASTRewriteConfig = ASTRewriteConfig()
  val options: List[PhaseOption[ASTRewriteConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during rewriting AST are muted."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the rewritten AST will be written to the outfile.")
  )
}

// ASTRewrite phase config
case class ASTRewriteConfig(
  var silent: Boolean = false,
  var outFile: Option[String] = None
) extends Config
