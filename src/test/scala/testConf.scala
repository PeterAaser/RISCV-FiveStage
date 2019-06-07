// package FiveStage
// import chisel3._
// import chisel3.iotesters._
// import org.scalatest.{Matchers, FlatSpec}
// import spire.math.{UInt => Uint}
// import fileUtils._
// import cats.implicits._

// import RISCVutils._
// import RISCVasm._
// import riscala._

// import utilz._

// class AllTests extends FlatSpec with Matchers {

//   val results = fileUtils.getAllTests.map{f =>
//     val result = TestRunner.runTest(f.getPath, false)
//     (f.getName, result)
//   }

//   makeReport(results)
// }


// /**
//   This is for you to run more verbose testing.
//   */
// class SelectedTests extends FlatSpec with Matchers {

//   val tests = List(
//     "matMul.s"
//   )

//   if(!tests.isEmpty){
//     val results = fileUtils.getAllTests.filter(f => tests.contains(f.getName)).map{ f =>
//       val result = TestRunner.runTest(f.getPath, true)
//       (f.getName, result)
//     }

//     makeReport(results)
//   }
// }
