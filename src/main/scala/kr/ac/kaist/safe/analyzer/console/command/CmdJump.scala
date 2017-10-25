/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer.console.command

import kr.ac.kaist.safe.analyzer.console._

// jump
case object CmdJump extends Command("jump", "Continue to analyze until the given iteration.") {
  override val help: String = "usage: " + name + " {#iteration}"
  def run(c: Interactive, args: List[String]): Option[Target] = args match {
    case iter :: Nil if iter.forall(_.isDigit) => Some(TargetIter(iter.toInt))
    case _ => printResult(help); None
  }
}
