package xyz.hyperreal.thumb

abstract class Instruction extends (CPU => Unit) {

  def disassemble(emu: Emulator): String

}

object ILLEGAL extends Instruction {

  def apply(cpu: CPU) = sys.error("illegal instruction")

  def disassemble(emu: Emulator): String = "ILLEGAL"

}
