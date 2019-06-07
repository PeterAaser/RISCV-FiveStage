package FiveStage

import chisel3._
import chisel3.util._
import chisel3.core.Input
import chisel3.iotesters.PeekPokeTester


/**
  * The top level module. You do not have to change anything here, 
  * however you are free to route out signals as you see fit for debugging.
  */
class Tile() extends Module{

  val io = IO(
    new Bundle {
      val DMEMWriteData          = Input(UInt(32.W))
      val DMEMAddress            = Input(UInt(32.W))
      val DMEMWriteEnable        = Input(Bool())
      val DMEMReadData           = Output(UInt(32.W))

      val regsWriteData          = Input(UInt(32.W))
      val regsAddress            = Input(UInt(5.W))
      val regsWriteEnable        = Input(Bool())
      val regsReadData           = Output(UInt(32.W))

      val regsDeviceWriteEnable  = Output(Bool())
      val regsDeviceWriteData    = Output(UInt(32.W))
      val regsDeviceWriteAddress = Output(UInt(5.W))

      val memDeviceWriteEnable   = Output(Bool())
      val memDeviceWriteData     = Output(UInt(32.W))
      val memDeviceWriteAddress  = Output(UInt(32.W))

      val IMEMWriteData          = Input(UInt(32.W))
      val IMEMAddress            = Input(UInt(32.W))

      val setup                  = Input(Bool())

      val currentPC              = Output(UInt())
    })

  val CPU = Module(new CPU).testHarness

  CPU.setupSignals.IMEMsignals.address     := io.IMEMAddress
  CPU.setupSignals.IMEMsignals.instruction := io.IMEMWriteData
  CPU.setupSignals.IMEMsignals.setup       := io.setup

  CPU.setupSignals.DMEMsignals.writeEnable := io.DMEMWriteEnable
  CPU.setupSignals.DMEMsignals.dataAddress := io.DMEMAddress
  CPU.setupSignals.DMEMsignals.dataIn      := io.DMEMWriteData
  CPU.setupSignals.DMEMsignals.setup       := io.setup

  CPU.setupSignals.registerSignals.readAddress  := io.regsAddress
  CPU.setupSignals.registerSignals.writeEnable  := io.regsWriteEnable
  CPU.setupSignals.registerSignals.writeAddress := io.regsAddress
  CPU.setupSignals.registerSignals.writeData    := io.regsWriteData
  CPU.setupSignals.registerSignals.setup        := io.setup

  io.DMEMReadData := CPU.testReadouts.DMEMread
  io.regsReadData := CPU.testReadouts.registerRead

  io.regsDeviceWriteAddress := CPU.regUpdates.writeAddress
  io.regsDeviceWriteEnable  := CPU.regUpdates.writeEnable
  io.regsDeviceWriteData    := CPU.regUpdates.writeData

  io.memDeviceWriteAddress  := CPU.memUpdates.writeAddress
  io.memDeviceWriteEnable   := CPU.memUpdates.writeEnable
  io.memDeviceWriteData     := CPU.memUpdates.writeData

  io.currentPC := CPU.currentPC
}



