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

package kr.ac.kaist.safe.shell

import kr.ac.kaist.safe.Config
import kr.ac.kaist.safe.Safe
import kr.ac.kaist.safe.compiler.Parser
import kr.ac.kaist.safe.compiler.Hoister
import kr.ac.kaist.safe.compiler.Disambiguator
import kr.ac.kaist.safe.compiler.WithRewriter
import kr.ac.kaist.safe.compiler.Compiler
import kr.ac.kaist.safe.exceptions.StaticError
import kr.ac.kaist.safe.exceptions.StaticErrors
import kr.ac.kaist.safe.exceptions.UserError
import kr.ac.kaist.safe.nodes.Program
import kr.ac.kaist.safe.nodes_util.ASTIO
import kr.ac.kaist.safe.safe_util.{ JSAstToConcrete, JSIRUnparser, NodeUtil }
import kr.ac.kaist.safe.useful.Useful
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException

////////////////////////////////////////////////////////////////////////////////
// Compiler
////////////////////////////////////////////////////////////////////////////////
object CompileMain {
  /**
   * Compile files. If they compile ok, it will say "Ok".
   * If you want a dump then give -out=outfile.
   */
  def doit: Int = {
    if (Safe.config.FileNames.length == 0)
      throw new UserError("The astRewrite command needs a file to disambiguate.")
    val (ir, return_code, _) = Compiler.compile(Safe.config.FileNames)
    val ircode = new JSIRUnparser(ir).doit
    if (Safe.config.opt_OutFileName != null) {
      val outFileName = Safe.config.opt_OutFileName
      try {
        val (fw, writer): (FileWriter, BufferedWriter) = Useful.filenameToWriters(outFileName)
        ASTIO.writeJavaAst(ir, outFileName)
        writer.close
        fw.close
        System.out.println("Dumped IR to " + outFileName)
      } catch {
        case e: IOException =>
          throw new IOException("IOException " + e + "while writing to " + outFileName)
      }
    } else System.out.println(ircode)
    if (Safe.config.opt_Time) Safe.printTimeTitle = "Compilation"
    return_code
  }
}
