package FiveStage
import chisel3._
import chisel3.core.Wire
import chisel3.util.{ BitPat, Cat }

  /**
    * Don't touch these
    */
class SetupSignals extends Bundle {
  val IMEMsignals     = new IMEMsetupSignals
  val DMEMsignals     = new DMEMsetupSignals
  val registerSignals = new RegisterSetupSignals
}

class IMEMsetupSignals extends Bundle {
  val setup       = Bool()
  val address     = UInt(32.W)
  val instruction = UInt(32.W)
}

class DMEMsetupSignals extends Bundle {
  val setup           = Bool()
  val writeEnable = Bool()
  val dataIn      = UInt(32.W)
  val dataAddress = UInt(32.W)
}

class RegisterSetupSignals extends Bundle {
  val setup = Bool()
  val readAddress  = UInt(5.W)
  val writeEnable  = Bool()
  val writeAddress = UInt(5.W)
  val writeData    = UInt(32.W)
}

class TestReadouts extends Bundle {
  val registerRead = UInt(32.W)
  val DMEMread     = UInt(32.W)
}

class RegisterUpdates extends Bundle {
  val writeEnable  = Bool()
  val writeData    = UInt(32.W)
  val writeAddress = UInt(5.W)
}

class MemUpdates extends Bundle {
  val writeEnable  = Bool()
  val writeData    = UInt(32.W)
  val writeAddress = UInt(32.W)
}
