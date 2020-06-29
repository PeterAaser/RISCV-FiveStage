package FiveStage
import chisel3.iotesters._
import java.io.File
import java.nio.file.Path
import scala.collection.mutable.LinkedHashMap
// import cats.effect.ContextShift

import cats.implicits._
import cats._
import cats.syntax._
import cats.Applicative._
import atto._, Atto._

object fileUtils {

  def say(word: Any)(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + Console.RESET + s" - $word")
  }

  def sayRed(word: Any)(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + Console.RED + s" - $word")
  }
  def sayGreen(word: Any)(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + Console.GREEN + s" - $word")
  }

  def getListOfFiles(dir: String): List[File] =
    (new File(dir)).listFiles.filter(_.isFile).toList

  def getListOfFiles(dir: Path): List[File] =
    dir.toFile().listFiles.filter(_.isFile).toList


  def getListOfFolders(dir: String): List[File] =
    (new File(dir)).listFiles.filter(_.isDirectory).toList

  def getListOfFolders(dir: Path): List[File] =
    dir.toFile().listFiles.filter(_.isDirectory).toList

  def getListOfFilesRecursive(dir: String): List[File] = {
    getListOfFiles(dir) ::: getListOfFolders(dir).flatMap(f =>
      getListOfFilesRecursive(f.getPath)
    )
  }

  import cats.implicits._
  import java.nio.file.Paths
  import java.util.concurrent.Executors
  import scala.concurrent.ExecutionContext

  def relativeFile(name: String) = {
    new File(getClass.getClassLoader.getResource(name).getPath)
  }

  def getTestDir: File =
    new File(getClass.getClassLoader.getResource("tests").getPath)

  def getAllTests: List[File] = getListOfFilesRecursive(getTestDir.getPath)
      .filter( f => f.getPath.endsWith(".s") )

  def getAllTestNames: List[String]        = getAllTests.map(_.toString.split("/").takeRight(1).mkString)

  // Not tested.
  def getAllWindowsTestNames: List[String] = getAllTests.map(_.toString.split("\\\\").takeRight(1).mkString)

  def clearTestResults = {
    try {
      val testResults = relativeFile("/testResults")
      testResults.delete()
    }
    catch {
      case _:Throwable => ()
    }
  }

  /**
    * Read an assembly file.
    */
  def readTest(testOptions: TestOptions): Either[String, List[String]] = {

    // Ahh, the GNU toolchain and its tabs
    val annoyingTabCharacter = '	' 

    getAllTests.filter(_.getName.contains(testOptions.testName)).headOption.toRight(s"File not found: ${testOptions.testName}").flatMap{ filename =>
      import scala.io.Source
      scala.util.Try(
        Source.fromFile(filename)
          .getLines.toList
          .map(_.replace(annoyingTabCharacter, ' ')))
        .toOption
        .toRight(s"Error reading $filename")
    }
  }
}
