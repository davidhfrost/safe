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

import kr.ac.kaist.safe.CmdASTRewrite
import kr.ac.kaist.safe.analyzer.domain.Map
import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.errors.warning._
import kr.ac.kaist.safe.nodes.ast._
import kr.ac.kaist.safe.nodes.ir.IRRoot
import kr.ac.kaist.safe.parser.Parser
import kr.ac.kaist.safe.util.{ Span, Useful, NodeUtil => NU }

import java.io.File
import java.nio.file.Paths
import scala.util.{ Failure, Success, Try }

private object ModuleResolver {
  // valid extensions of JS files.
  // imported files with an explicit extension outside this list will be ignored.
  private val jsExtensions = List[String](".js", ".jsx")

  // the path to the directory containing modeled NPM modules (relative to the root SAFE dir)
  private val npmModuleDirs = List("src", "main", "resources", "modules")

  // maps NPM module names with models to the files containing those models.
  // all models are contained in the directory pointed to by `npmModuleDirs`.
  private val modeledNpmModules: Map[String, String] = Map(
    ("react", "react.js"),
    ("react-dom", "react-dom.js")
  )

  // maps an NPM module name to the path to its modeled source file.
  private def npmModulePath(moduleName: String): Option[String] =
    modeledNpmModules.get(moduleName).map(n => Useful.path(npmModuleDirs ++ List(n): _*))

  // maps a *relative* module specifier path to its *absolute* canonical path.
  // returns `None` if the module specifier is referencing a non-JS file.
  def resolveModuleSpecifierPath(basePath: String, specPath: String): Option[String] = {
    // if the path starts with a period, it's a relative file path
    if (specPath.startsWith(".")) {
      // concatenate the source file's directory with the module specifier string
      val baseDir = Paths.get(new File(basePath).getParentFile.getCanonicalPath)
      val fileName = baseDir.resolve(specPath).toString

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
      npmModulePath(specPath) match {
        case Some(mp) => Some(mp)
        case None =>
          println(s"Unmodelled NPM module was imported: '${specPath}'")
          None
      }
    }
  }
}

// `basePath`: the path of the file being parsed
class ModuleRewriter(basePath: String, program: Program) {
  // the list of statements to be prepended to the original file
  // to account for translating away all module-related code.
  var modulePreamble: List[Stmt] = List()

  // a flag which records whether this file exports anything.
  // in this case, we need to declare an `___exports___` variable,
  // but we need the flag because we only want declare that variable once.
  var hasExports = false

  var nextImportedFileIdx = 0
  private def getNextFileIdx: Int = {
    nextImportedFileIdx += 1
    nextImportedFileIdx - 1
  }

  // helper methods for creating AST nodes with less boilerplate
  def id(info: ASTNodeInfo, text: String): Id = Id(info, text, None, false)
  def varRef(info: ASTNodeInfo, text: String): VarRef = VarRef(info, id(info, text))

  // generate unique ids for the functions generated by class expressions.
  // these functions need to have names that the rest of the program doesn't use.
  def importsObjId(info: ASTNodeInfo, fileIdx: Int): Id = id(info, s"___imports${fileIdx}___")
  def exportsObjId(info: ASTNodeInfo): Id = id(info, s"___exports___")
  def defaultId(info: ASTNodeInfo): Id = id(info, "___default___")

  // generates the code `___exports___ = {}`,
  // which initializes the exports object.
  def exportsObjDecl(info: ASTNodeInfo): VarStmt = {
    val exportsId = exportsObjId(info)
    val emptyObjVal = ObjectExpr(info, List())
    val decl = VarDecl(info, exportsId, Some(emptyObjVal), false)
    VarStmt(info, List(decl))
  }

  def exportsObjReturn(info: ASTNodeInfo): Return =
    Return(info, Some(VarRef(info, exportsObjId(info))))

  // generates the code `___exports___[binding] = [value]`
  def exportAssignmentStmt(info: ASTNodeInfo, binding: Id, value: Expr): Stmt = {
    // `___exports___[binding]`
    val lhs = Dot(info, VarRef(info, exportsObjId(info)), binding)
    val assignment = AssignOpApp(info, lhs, Op(info, "="), value)
    ExprStmt(info, assignment, false)
  }

  // generates the name of a function containing an imported file
  def fileFnId(info: ASTNodeInfo, fileIdx: Int): Id =
    id(info, s"___fileFn${fileIdx}___")

  // flattens the input list of `Stmts` into a single `Stmts`.
  def flattenStmts(stmtsList: List[Stmts]): Stmts =
    Stmts(
      stmtsList.head.info,
      stmtsList.foldLeft(List[Stmt]())((res, stmts) => res ++ stmts.body),
      false
    )

  // translates the `TopLevel` of an imported file to a `Functional` containing the same code.
  def topLevelToFunctional(topLevel: TopLevel, fileIdx: Int): Functional = {
    // add a `return ___exports___` statement to the functional to pass the exports object
    // from the functional to the main file body.
    var stmts = flattenStmts(topLevel.stmts)
    stmts = stmts.copy(body = stmts.body ++ List(exportsObjReturn(topLevel.info)))

    Functional(
      topLevel.info, topLevel.fds, topLevel.vds,
      stmts,
      fileFnId(topLevel.info, fileIdx),
      List(), stmts.toString(0), false
    )
  }

  // generates `___exports[fileIdx]___ = ([fileFn])()`
  def captureImportsStmt(fileFn: FunDecl, fileIdx: Int): Stmt = {
    val info = fileFn.info
    val fileFnCall = FunApp(info, VarRef(fileFn.ftn.name.info, fileFn.ftn.name), List())
    val importDecl = VarDecl(info, importsObjId(info, fileIdx), Some(fileFnCall), false)
    VarStmt(info, List(importDecl))
  }

  // translates `importClause` into a statement reading values from the object `importRef`,
  // which should contain the imported file's exports via code generated by `translateImportDecl` below.

  // when `importRef` is `None`, this creates uninitialized variable declarations for each of the imported variables.
  // this is used for import statements which import ignored files.
  // e.g. `import logo from 'logo.svg'` would declare the variable `logo` but leave it undefined.
  def translateImportClause(importClause: ImportClause, importRef: Option[VarRef]): Stmt = importClause match {
    case ImportedDefaultBinding(info, name) =>
      val defaultVal = importRef.map(Dot(info, _, defaultId(info)))
      val decl = VarDecl(info, name, defaultVal, false)
      VarStmt(info, List(decl))

    case NameSpaceImport(info, name) =>
      val decl = VarDecl(info, name, importRef, false)
      VarStmt(info, List(decl))

    case NamedImports(info, importsList) =>
      // map each import specifier to a `VarDecl`
      val decls = importsList.map {
        case SameNameImportSpecifier(info, importedBinding) =>
          val bindingVal = importRef.map(Dot(info, _, importedBinding))
          VarDecl(info, importedBinding, bindingVal, false)

        case RenamedImportSpecifier(info, binding, identifierName) =>
          val bindingId = if (binding.text == "default") defaultId(info) else binding
          val bindingVal = importRef.map(Dot(info, _, bindingId))
          VarDecl(info, identifierName, bindingVal, false)
      }
      VarStmt(info, decls)

    // translate the two "combined" import clauses by recursively combining the results
    // from translating the import subclauses.
    case DefaultAndNameSpaceImport(info, defaultImport, nameSpaceImport) =>
      ABlock(info, List(
        translateImportClause(defaultImport, importRef),
        translateImportClause(nameSpaceImport, importRef)
      ), false)

    case DefaultAndNamedImports(info, defaultImport, namedImports) =>
      ABlock(info, List(
        translateImportClause(defaultImport, importRef),
        translateImportClause(namedImports, importRef)
      ), false)
  }

  // generates a function containing the imported file's code, which will be prepended to the main file.
  // then translates the `importClause`.
  def translateImportDecl(info: ASTNodeInfo, importClause: ImportClause, importPath: Option[String]): Stmt = {
    // first, recursively perform an `astRewrite` on the imported file to get its rewritten AST.
    val result = importPath.map(path => CmdASTRewrite(List("-silent", path), false)).asInstanceOf[Option[Try[(Program, Program)]]]
    result match {
      // importing an unmodeled file.
      // the translation of the import statement should only declare the imported variables,
      // leaving those variables undefined.
      case None =>
        translateImportClause(importClause, None)

      case Some(Failure(_)) =>
        println(s"Failure translating imported file: ${importPath}")
        EmptyStmt(info)

      case Some(Success((_, Program(info, topLevel)))) =>
        // generate two statements which include the imported file's code into the main file:
        // stmt 0: `function ___fileFn[i]___() { [...imported file code...] }`
        // stmt 1: `___imports[i]___ = ___fileFn[i]___()`
        val fileIdx: Int = getNextFileIdx
        var fileFn = FunDecl(info, topLevelToFunctional(topLevel, fileIdx), false)

        val captureImport = captureImportsStmt(fileFn, fileIdx)
        modulePreamble ++= List[Stmt](fileFn, captureImport)

        // the identifier `___imports[i]___` which receives the exports of the imported file.
        val importRef = VarRef(info, importsObjId(info, fileIdx))
        translateImportClause(importClause, Some(importRef))
    }
  }

  def translateExportClause(exportClause: ExportClause): Stmt = {
    // map each exported binding to an `ExprStmt` statement which assigns
    // the appropriate value to a field of the `___exports___` object.
    val assignments = exportClause.exportsList.map {
      case SameNameImportSpecifier(info, binding) =>
        exportAssignmentStmt(info, binding, VarRef(binding.info, binding))
      case RenamedImportSpecifier(info, binding, id) =>
        exportAssignmentStmt(info, id, VarRef(binding.info, binding))
    }

    ABlock(exportClause.info, assignments, false)
  }

  def translateExportDecl(exportDecl: ExportDeclaration): Stmt = {
    val info = exportDecl.info

    // if the exports object hasn't been declared yet, add its declaration
    // to the module preamble.
    if (!hasExports) {
      modulePreamble ++= List(exportsObjDecl(info))
      hasExports = true
    }

    exportDecl match {
      //      case ExportAllFromOther(info, from) =>
      //      case ExportFromOther(info, export, from) =>
      case ExportSelf(info, export) =>
        translateExportClause(export)

      case ExportVarStmt(info, vars) =>
        // map each `VarDecl` in the export declaration to an equivalent statement.
        val stmts: List[Stmt] = vars.map {
          case decl @ VarDecl(info, name, Some(expr), strict) =>
            val declStmts = List(VarStmt(info, List(decl)), exportAssignmentStmt(info, name, expr))
            ABlock(info, declStmts, false)
          case decl @ VarDecl(info, name, None, strict) =>
            println(s"ignored exported `VarDecl`: ${decl.toString(0)}")
            EmptyStmt(info)
        }
        ABlock(info, stmts, false)

      case ExportDefaultExpr(info, expr) =>
        exportAssignmentStmt(info, defaultId(info), expr)
    }
  }

  private object ModuleRewriteWalker extends ASTWalker {
    override def walk(pgm: Program): Program = {
      val newProgram = super.walk(program)

      // add the statements generated by rewriting `ClassExpr` nodes to the
      // beginning of the program.
      val newStmts: Stmts = Stmts(program.info, modulePreamble, false)
      newProgram.copy(body = newProgram.body.copy(stmts = newStmts :: newProgram.body.stmts))
    }

    override def walk(stmt: Stmt): Stmt = stmt match {
      case FromImportDeclaration(info, importClause, FromClause(_, moduleSpecifier)) => {
        val importPath = ModuleResolver.resolveModuleSpecifierPath(basePath, moduleSpecifier.moduleName.escaped)
        translateImportDecl(info, importClause, importPath)
      }

      case e: ExportDeclaration =>
        translateExportDecl(e)

      // remove import statements that don't explicitly import any values.
      // (e.g. `import 'file.js'`)
      case ModuleImportDeclaration(info, _) =>
        EmptyStmt(info)

      case _ => super.walk(stmt)
    }
  }

  val result: Program = {
    NU.SimplifyWalker.walk(ModuleRewriteWalker.walk(program))
  }
}
