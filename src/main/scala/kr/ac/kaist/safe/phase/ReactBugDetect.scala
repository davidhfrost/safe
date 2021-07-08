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
case object ReactBugDetect extends PhaseObj[(CFG, Int, TracePartition, Semantics), ReactBugDetectConfig, CFG] {
  val name: String = "reactBugDetector"
  val help: String = "Detect bugs in React applications."

  def checkBlock(cfg: CFG, semantics: Semantics, block: CFGBlock): List[String] = {
    List()
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
