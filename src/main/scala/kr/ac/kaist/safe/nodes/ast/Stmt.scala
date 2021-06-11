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

package kr.ac.kaist.safe.nodes.ast

import kr.ac.kaist.safe.util.{ NodeUtil => NU }
import kr.ac.kaist.safe.LINE_SEP

// Program ::= Stmt*
case class Stmts(
    info: ASTNodeInfo,
    body: List[Stmt],
    strict: Boolean
) extends ASTNode {
  override def toString(indent: Int): String = ""
}

// Statements
trait Stmt extends ASTNode {
  def getIndent(indent: Int): Int = indent + 1
}

/**
 * Internally generated NoOperation
 * currently to denote the end of a file by Shell
 * Do not appear in the JavaScript source text
 */
case class NoOp(
    info: ASTNodeInfo,
    desc: String
) extends Stmt {
  override def toString(indent: Int): String = ""
}

/**
 * Internally generated statement unit by Hoister
 * Do not appear in the JavaScript source text
 */
case class StmtUnit(
    info: ASTNodeInfo,
    stmts: List[Stmt]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("{")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1, stmts,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }
}

// Stmt ::= function Id ( (Id,)* ) { Stmt* }
case class FunDecl(
    info: ASTNodeInfo,
    ftn: Functional,
    strict: Boolean
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("function ")
      .append(ftn.toString(indent))
    s.toString
  }
}

// Stmt ::= { Stmt* }
case class ABlock(
    info: ASTNodeInfo,
    stmts: List[Stmt],
    internal: Boolean
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("{")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        stmts,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }
  override def getIndent(indent: Int): Int = indent
}

// Stmt ::= var VarDecl(, VarDecl)* ;
case class VarStmt(
    info: ASTNodeInfo,
    vds: List[VarDecl]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    vds match {
      case Nil => s.toString
      case _ =>
        s.append("var ")
          .append(NU.join(indent, vds, ", ", new StringBuilder("")))
          .append(";")
        s.toString
    }
  }
}

// Stmt ::= Id (= Expr)?
case class VarDecl(
    info: ASTNodeInfo,
    name: Id,
    expr: Option[Expr],
    strict: Boolean
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(name.toString(indent))
    expr.map(e => s.append(" = ").append(e.toString(indent)))
    s.toString
  }
}

// Stmt ::= ;
case class EmptyStmt(
    info: ASTNodeInfo
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(";")
    s.toString
  }
}

// Stmt ::= Expr ;
case class ExprStmt(
    info: ASTNodeInfo,
    expr: Expr,
    internal: Boolean
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(expr.toString(indent) + ";")
    s.toString
  }
}

// Stmt ::= if ( Expr ) Stmt (else Stmt)?
case class If(
    info: ASTNodeInfo,
    cond: Expr,
    trueBranch: Stmt,
    falseBranch: Option[Stmt]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val trueIndent = trueBranch.getIndent(indent)
    s.append("if (")
      .append(cond.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(trueIndent))
      .append(trueBranch.toString(trueIndent))
    falseBranch match {
      case Some(fb) =>
        val falseIndent = fb.getIndent(indent)
        s.append(LINE_SEP)
          .append(NU.getIndent(indent))
          .append("else")
          .append(LINE_SEP)
          .append(NU.getIndent(falseIndent))
          .append(fb.toString(falseIndent))
      case _ =>
    }
    s.toString
  }
}

// Stmt ::= do Stmt while ( Expr ) ;
case class DoWhile(
    info: ASTNodeInfo,
    body: Stmt,
    cond: Expr
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    var bodyIndent = body.getIndent(indent)
    s.append("do")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
      .append("while (")
      .append(cond.toString(indent))
      .append(");")
    s.toString
  }
}

// Stmt ::= while ( Expr ) Stmt
case class While(
    info: ASTNodeInfo,
    cond: Expr,
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("while (")
    s.append(cond.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }
}

// Stmt ::= for ( Expr? ; Expr? ; Expr? ) Stmt
case class For(
    info: ASTNodeInfo,
    init: Option[Expr],
    cond: Option[Expr],
    action: Option[Expr],
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("for (")
    init.map(i => s.append(i.toString(indent)))
    s.append(";")
    cond.map(c => s.append(c.toString(indent)))
    s.append(";")
    action.map(a => s.append(a.toString(indent)))
    s.append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }
}

// Stmt ::= for ( lhs in Expr ) Stmt
case class ForIn(
    info: ASTNodeInfo,
    lhs: LHS,
    expr: Expr,
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("for (")
      .append(lhs.toString(indent))
      .append(" in ")
      .append(expr.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }
}

// Stmt ::= for ( var VarDecl(, VarDecl)* ; Expr? ; Expr? ) Stmt
case class ForVar(
    info: ASTNodeInfo,
    vars: List[VarDecl],
    cond: Option[Expr],
    action: Option[Expr],
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("for(var ")
      .append(NU.join(
        indent,
        vars,
        ", ",
        new StringBuilder("")
      ))
      .append(";")
    cond.map(c => s.append(c.toString(indent)))
    s.append(";")
    action.map(a => s.append(a.toString(indent)))
    s.append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }
}

// Stmt ::= for ( var VarDecl in Expr ) Stmt
case class ForVarIn(
    info: ASTNodeInfo,
    vd: VarDecl,
    expr: Expr,
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("for(var ")
      .append(vd.toString(indent))
      .append(" in ")
      .append(expr.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }
}

// Stmt ::= continue Label? ;
case class Continue(
    info: ASTNodeInfo,
    target: Option[Label]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("continue")
    target.map(t => s.append(" ").append(t.toString(indent)))
    s.append(";")
    s.toString
  }
}

// Stmt ::= break Label? ;
case class Break(
    info: ASTNodeInfo,
    target: Option[Label]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("break")
    target.map(t => s.append(" ").append(t.toString(indent)))
    s.append(";")
    s.toString
  }
}

// Stmt ::= return Expr? ;
case class Return(
    info: ASTNodeInfo,
    expr: Option[Expr]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("return")
    expr.map(e => s.append(" ").append(e.toString(indent)))
    s.append(";")
    s.toString
  }
}

// Stmt ::= with ( Expr ) Stmt
case class With(
    info: ASTNodeInfo,
    expr: Expr,
    stmt: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val stmtIndent = stmt.getIndent(indent)
    s.append("with (")
      .append(expr.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(stmtIndent))
      .append(stmt.toString(stmtIndent))
    s.toString
  }
}

// Stmt ::= switch ( Expr ) { CaseClause* (default : Stmt*)? CaseClause* }
case class Switch(
    info: ASTNodeInfo,
    cond: Expr,
    frontCases: List[Case],
    defopt: Option[List[Stmt]],
    backCases: List[Case]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("switch (")
      .append(cond.toString(indent))
      .append("){")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        frontCases,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
    defopt.map(d => {
      s.append(LINE_SEP)
        .append(NU.getIndent(indent + 1))
        .append("default:")
        .append(LINE_SEP)
        .append(NU.getIndent(indent + 2))
        .append(NU.join(
          indent + 2,
          d,
          LINE_SEP + NU.getIndent(indent + 2),
          new StringBuilder("")
        ))
    })
    s.append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        backCases,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }
}

// CaseClause ::= case Expr : Stmt*
case class Case(
    info: ASTNodeInfo,
    cond: Expr,
    body: List[Stmt]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("case ")
      .append(cond.toString(indent))
      .append(":")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
    s.toString
  }
}

// Stmt ::= Label : Stmt
case class LabelStmt(
    info: ASTNodeInfo,
    label: Label,
    stmt: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(label.toString(indent))
      .append(" : ")
      .append(stmt.toString(indent))
    s.toString
  }
}

// Stmt ::= throw Expr ;
case class Throw(
    info: ASTNodeInfo,
    expr: Expr
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("throw ")
      .append(expr.toString(indent))
      .append(";")
    s.toString
  }
}

// Stmt ::= try { Stmt* } (catch ( Id ) { Stmt* })? (finally { Stmt* })?
case class Try(
    info: ASTNodeInfo,
    body: List[Stmt],
    catchBlock: Option[Catch],
    fin: Option[List[Stmt]]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("try")
      .append(LINE_SEP)
      .append("{")
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}")
    catchBlock.map(c => {
      s.append(LINE_SEP)
        .append(NU.getIndent(indent))
        .append(c.toString(indent))
    })
    fin.map(f => {
      s.append(LINE_SEP)
        .append(NU.getIndent(indent))
        .append("finally")
        .append(LINE_SEP)
        .append("{")
        .append(NU.getIndent(indent + 1))
        .append(NU.join(
          indent + 1,
          f,
          LINE_SEP + NU.getIndent(indent + 1),
          new StringBuilder("")
        ))
        .append("}")
        .append(LINE_SEP)
    })
    s.toString
  }
}

// Catch ::= catch ( Id ) { Stmt* }
case class Catch(
    info: ASTNodeInfo,
    id: Id,
    body: List[Stmt]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("catch (")
      .append(id.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append("{")
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}")
      .append(LINE_SEP)
    s.toString
  }
}

// Stmt ::= debugger ;
case class Debugger(
    info: ASTNodeInfo
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("debugger;")
    s.toString
  }
}

// ES6 15.2.2: ImportDeclaration

sealed trait ImportDeclaration extends Stmt

case class FromImportDeclaration(
    info: ASTNodeInfo,
    importClause: ImportClause,
    fromClause: FromClause
) extends ImportDeclaration {
  override def toString(indent: Int): String =
    s"import ${importClause.toString(0)} ${fromClause.toString(0)}"
}

case class ModuleImportDeclaration(
    info: ASTNodeInfo,
    moduleSpecifier: ModuleSpecifier
) extends ImportDeclaration {
  override def toString(indent: Int): String =
    s"import ${moduleSpecifier.toString(0)}"
}

sealed trait ImportClause extends ASTNode

case class ImportedDefaultBinding(
    info: ASTNodeInfo,
    name: Id
) extends ImportClause {
  override def toString(indent: Int): String = name.toString(0)
}

case class NameSpaceImport(
    info: ASTNodeInfo,
    name: Id
) extends ImportClause {
  override def toString(indent: Int): String = s"* as ${name.toString(0)}"
}

sealed trait ImportSpecifier extends ASTNode {
  override def toString: String = toString(0)
}

case class SameNameImportSpecifier(
    info: ASTNodeInfo,
    importedBinding: Id
) extends ImportSpecifier {
  def toString(indent: Int): String = importedBinding.toString(0)
}

case class RenamedImportSpecifier(
    info: ASTNodeInfo,
    importedBinding: Id,
    identifierName: Id
) extends ImportSpecifier {
  def toString(indent: Int): String =
    s"${importedBinding.toString(0)} as ${identifierName.toString(0)}"
}

case class NamedImports(
    info: ASTNodeInfo,
    importsList: List[ImportSpecifier]
) extends ImportClause {
  def getInfo: ASTNodeInfo = info
  override def toString(indent: Int): String =
    s"{ ${importsList.mkString(", ")} }"
}

case class DefaultAndNameSpaceImport(
    info: ASTNodeInfo,
    defaultImport: ImportedDefaultBinding,
    nameSpaceImport: NameSpaceImport
) extends ImportClause {
  override def toString(indent: Int): String =
    s"${defaultImport.toString(0)}, ${nameSpaceImport.toString(0)}"
}

case class DefaultAndNamedImports(
    info: ASTNodeInfo,
    defaultImport: ImportedDefaultBinding,
    namedImports: NamedImports
) extends ImportClause {
  override def toString(indent: Int): String =
    s"${defaultImport.toString(0)}, ${namedImports.toString(0)}"
}

case class ModuleSpecifier(
    info: ASTNodeInfo,
    moduleName: StringLiteral
) extends ASTNode {
  override def toString(indent: Int): String = moduleName.toString(0)
}

case class FromClause(
    info: ASTNodeInfo,
    moduleSpecifier: ModuleSpecifier
) extends ASTNode {
  override def toString(indent: Int): String =
    s"from ${moduleSpecifier.toString(0)}"
}

// ES6 15.2.3: ExportDeclaration

sealed trait ExportDeclaration extends Stmt

case class ExportAllFromOther(
    info: ASTNodeInfo,
    from: FromClause
) extends ExportDeclaration {
  override def toString(indent: Int): String =
    s"export * from ${from.toString(0)}"
}

case class ExportFromOther(
    info: ASTNodeInfo,
    export: ExportClause,
    from: FromClause
) extends ExportDeclaration {
  override def toString(indent: Int): String =
    s"export ${export.toString(0)} ${from.toString(0)}"
}

case class ExportSelf(
    info: ASTNodeInfo,
    export: ExportClause
) extends ExportDeclaration {
  override def toString(indent: Int): String =
    s"export ${export.toString(0)}"
}

case class ExportVarStmt(
    info: ASTNodeInfo,
    vars: List[VarDecl]
) extends ExportDeclaration {
  override def toString(indent: Int): String =
    s"export var ${vars.map(_.toString(0)).mkString(", ")}"
}

/* TODO:
 * export Declaration
 * export default HoistableDeclaration
 * export default ClassDeclaration
 * export default AssignmentExpression
 */

case class ExportClause(
    info: ASTNodeInfo,
    exportsList: List[ImportSpecifier]
) extends ASTNode {
  override def toString(indent: Int): String =
    s"{ ${exportsList.mkString(", ")} }"

  // alternative constructor
  def this(namedImports: NamedImports) = this(namedImports.info, namedImports.importsList)
}