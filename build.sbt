import scalariform.formatter.preferences._
import java.io.File

lazy val buildParsers = taskKey[Unit]("Builds parsers")
lazy val deleteParserDir = taskKey[Unit]("Delete java parser directory")

lazy val root = (project in file(".")).
  settings(
    name := "SAFE",
    version := "2.0",
    organization := "kr.ac.kaist.safe",
    scalaVersion := "2.11.7",
    buildParsers in Compile := {
      val options = ForkOptions(
        bootJars = Seq(new File("./lib/xtc.jar"))
      )
      val srcDir = baseDirectory.value + "/src/main"
      val inDir = srcDir + "/scala/kr/ac/kaist/safe/parser/"
      val outDir = srcDir + "/java/kr/ac/kaist/safe/parser/"
      val outFile = file(outDir)
      if(!outFile.exists) IO.createDirectory(outFile)
      val arguments = Seq("-in", srcDir + "/scala", "-enc-out", "UTF-8",
                          "-out", outDir, inDir + "JS.rats")
      val mainClass = "xtc.parser.Rats"
      val cache = FileFunction.cached(outFile,
                                      FilesInfo.lastModified,
                                      FilesInfo.exists) {
        in: Set[File] => {
          Fork.java(options, mainClass +: arguments)
          Set(file(inDir + "JS.rats"))
        }
      }
      cache(file(inDir).asFile.listFiles.toSet)
    },
    deleteParserDir := {
      val srcDir = baseDirectory.value + "/src/main"
      val outDir = srcDir + "/java/kr/ac/kaist/safe/parser/"
      IO.delete(file(outDir))
    },
    compile <<= (compile in Compile) dependsOn (buildParsers in Compile),
    clean <<= deleteParserDir
  )

unmanagedJars in Compile ++= Seq(file("lib/plt.jar"), file("lib/xtc.jar"))

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test" withSources,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1" withSources
)
