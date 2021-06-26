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

package kr.ac.kaist.safe.ast_rewriter

import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.errors.warning._
import kr.ac.kaist.safe.nodes.ast._
import kr.ac.kaist.safe.parser.Parser
import kr.ac.kaist.safe.util.{ NodeUtil => NU, Span }

class ClassRewriter(program: Program) {
  val result: Program = {
    NU.SimplifyWalker.walk(ClassRewriteWalker.walk(program))
  }

  def emptyFunctional(info: ASTNodeInfo, name: Id) = Functional(
    info,
    List(), // no internal function declarations
    List(), // no internal variable declarations
    Stmts(info, List(), false), // no statement in the body
    name,
    List(), // no arguments
    "", // empty stringified function body
    false // not in strict mode
  )

  def prototypeLHS(info: ASTNodeInfo, className: Id, methodName: Id): LHS = {
    // `[className].prototype`
    val proto = Dot(
      info,
      VarRef(info, className),
      Id(info, "prototype", None, false)
    )

    // `[className].prototype.[methodName]`
    Dot(info, proto, methodName)
  }

  def isConstructor(cm: ClassMethod): Boolean = cm.ftn.name.text == "constructor"

  private object ClassRewriteWalker extends ASTWalker {
    override def walk(stmt: Stmt): Stmt = stmt match {
      case ClassDeclaration(info, className, methods) =>

        // build the constructor, which is a function declaration
        val constructor = methods.find(isConstructor) match {
          case Some(cm) =>
            // splice the body of the `constructor` method into a function declaration named `className`.
            FunDecl(info, cm.ftn.copy(name = className), strict = false)

          case None =>
            // create an empty function declaration to serve as the constructor.
            FunDecl(info, emptyFunctional(info, className), strict = false)
        }

        // build the method definitions as assignments to the constructor's prototype.
        // i.e. `[className].prototype.[methodName] = [method function definition]`
        val methodDefns: List[Stmt] = methods.filterNot(isConstructor).map(method => {
          val lhs = prototypeLHS(method.info, className, method.ftn.name)
          val methodFunExpr = FunExpr(method.ftn.info, method.ftn)
          val assignment = AssignOpApp(method.info, lhs, Op(method.info, "="), methodFunExpr)
          ExprStmt(method.info, assignment, false)
        })

        // roll the constructor and the methods together into a single block of statements
        ABlock(info, constructor :: methodDefns, false)

      case _ => super.walk(stmt)
    }
  }
}
