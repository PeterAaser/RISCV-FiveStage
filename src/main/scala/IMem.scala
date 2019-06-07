package FiveStage
import chisel3._
import chisel3.experimental.MultiIOModule

/**
  * This module is finished and does not need to be modified to complete your fivestage.
  * 
  * When setup is enabled data is written to the instruction memory.
  * In normal operation this memory is write only (no self modifying code)
  */
class IMEM() extends MultiIOModule {

  // Don't touch
  val testHarness = IO(
    new Bundle {
      val setupSignals     = Input(new IMEMsetupSignals)
      val requestedAddress = Output(UInt())
    }
  )

  
  val io = IO(
    new Bundle {
      val instructionAddress = Input(UInt(32.W))
      val instruction        = Output(UInt(32.W))
    })


  /**
    SyncReadMem will output the value of the address signal set in the previous cycle.
    */
  val instructions = SyncReadMem(4096, UInt(32.W))

  // The address we want to read at during operation. During setup it acts as a write address
  // leading to the somewhat uninformative name shown here.
  val addressSource = Wire(UInt(32.W))

  testHarness.requestedAddress := io.instructionAddress

  when(testHarness.setupSignals.setup){
    addressSource := testHarness.setupSignals.address
  }.otherwise {
    addressSource := io.instructionAddress
  }

  // For loading data
  when(testHarness.setupSignals.setup){
    instructions(addressSource) := testHarness.setupSignals.instruction
  }

  io.instruction := instructions(addressSource)
}
