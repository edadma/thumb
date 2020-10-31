package xyz.hyperreal.thumb

import java.io.PrintStream

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}

case class Interrupt(level: Int, vector: Option[Int]) extends Ordered[Interrupt] {
  def compare(that: Interrupt): Int = level - that.level
}

class CPU {

  val UNDERLINED_OFF = "\u001B[24m"
  val LABEL_BG = "\u001B[48;5;238m"
  val BREAKPOINT_BG = "\u001B[48;5;124m"
  val DEFAULT_BG = "\u001B[49m"

  var memory: Memory = _
  val R = new Array[Int](8)
  var PC = 0
  var SP = 0
  var C = false
  var V = false
  var Z = false
  var N = false
  var X = false
  var opcode = 0
  var prog: Addressable = _
  var reverseSymbols: mutable.HashMap[Int, String] = mutable.HashMap()
  var debug: Map[Int, (String, String)] = Map()

  private val interrupts = new mutable.PriorityQueue[Interrupt]
  private var interruptsAvailable = false
  var curpc = 0
  val devices = new ListBuffer[Device]

  var counter = 0L

  val breakpointMap = new mutable.HashMap[Int, Boolean]
  val watchMap = new mutable.HashMap[Int, ListBuffer[Int]]
  var trace = false
  var tracewrite: Option[(Long, Int, Int, Size)] = None
  var traceout = Console.out
  var tracestart = -1L
  var tracelimit = Long.MaxValue

  var running = false
  var stopped = false

  def resettable(dev: Device): Unit = {
    devices += dev
  }

  def service: Unit = synchronized {
//    if (interrupts nonEmpty) {
//      val req = interrupts.head
//
//      if (req.level == 7 || req.level > ((SR & SRBit.I) >> SRBit.I_shift)) {
//        interrupts.dequeue match {
//          case Interrupt(level, None)         => exception(level, VectorTable.autoVectors + ((level - 1) << 2))
//          case Interrupt(level, Some(vector)) => exception(level, VectorTable.interruptVectors + (vector << 2))
//        }
//
//        service
//      }
//    }
//
//    if (interrupts isEmpty)
//      interruptsAvailable = false
  }

  def trap(vector: Int) = false

  def jump(address: Int): Unit = {
    prog = memory.find(address)
    PC = address
  }

  def isRunning = running

  def disassemble(compact: Boolean, out: PrintStream) = {
    0
//    def ansi(s: String): Unit =
//      if (out != Console.out)
//        out.print(s)
//
//    if (memory.valid(PC)) {
//      val pc = PC
//
//      prog = memory.find(PC)
//      fetch
//
//      val extension = PC
//      val disassembly = opcodes(opcode).disassemble(this)
//      val words = (PC - extension) / 2
//
//      PC = extension
//      out.print(f"${pc.toHexString.toUpperCase}%6s  ${hexShort(opcode)} ")
//
//      for (_ <- 0 until words)
//        out.print(hexShort(fetchShort) + " ")
//
//      out.print(" " * ((4 - words) * 5) + " ")
//
//      val label =
//        reverseSymbols get pc match {
//          case None    => ""
//          case Some(l) => l + ": "
//        }
//
//      val bp = isBreakpoint(pc)
//
//      if (bp)
//        ansi(BREAKPOINT_BG)
//      else if (label nonEmpty)
//        ansi(LABEL_BG)
//
//      out.print(label + " " * (15 - label.length min 15))
//
//      if (label.nonEmpty && !bp)
//        ansi(DEFAULT_BG)
//
//      out.print(disassembly + " " * (25 - disassembly.length min 25))
//
//      debug get pc match {
//        case None                 =>
//        case Some((lineno, file)) => out.print(s"    $lineno: $file")
//      }
//
//      if (bp)
//        ansi(DEFAULT_BG)
//
//      out.println
//      PC = pc
//      (words + 1) * 2
//    } else {
//      out.print(f"PC=${PC.toHexString.toUpperCase}%6s")
//      0
//    }
  }

  def registers(out: PrintStream) = {
//    for (i <- 0 to 7)
//      out.print(s"D$i=${hexInt(D(i))} ")
//
//    out.println
//
//    for (i <- 0 to 7)
//      out.print(s"A$i=${hexInt(readA(i).asInstanceOf[Int])} ")
//
//    out.println
//    out.println("T S  III   XNZVC")
//
//    def star(bit: Int) = if ((SR & bit) != 0) "*" else " "
//
//    def cond(on: Boolean) = if (on) "*" else " "
//
//    out.print(s"${star(SRBit.T)} ${star(SRBit.S)}  ${star(SRBit.I2)}${star(SRBit.I1)}${star(SRBit.I0)}   ${cond(X)}${cond(
//      N)}${cond(Z)}${cond(V)}${cond(C)}\n")
  }

  def problem(error: String) = {
    registers(Console.err)
    sys.error(s"error at ${PC.toHexString} (${"%08x".format(opcode)}): $error")
  }

  def stop: Unit = {
    running = false
    stopped = false
  }

  def reset: Unit = {
    stop
    memory.reset
    resetSignal
  }

  def resetSignal: Unit = {
    for (d <- devices)
      d.reset
  }

  def isWatch(addr: Int) = watchMap contains addr

  def watchEvent(addr: Int, pc: Int) = watchMap(addr).append(pc)

  def setWatch(addr: Int) = watchMap(addr) = new ListBuffer

  def clearWatch(addr: Int) = watchMap remove addr

  def clearWatches = watchMap.clear

  def watches = watchMap.toList sortBy (_._1) map { case (addr, buf) => (addr, buf.toList) }

  def isBreakpoint(addr: Int) = breakpointMap contains addr

  def setBreakpoint(addr: Int) = breakpointMap(addr) = false

  def setSingleShotBreakpoint(addr: Int) = breakpointMap(addr) = true

  def clearBreakpoint(addr: Int) = breakpointMap remove addr

  def clearBreakpoints = breakpointMap.clear

  def breakpoints = breakpointMap.keysIterator.toList.sorted

  def execute: Unit = {
    if (trace)
      Console.withOut(traceout) {
        if (tracestart == -1)
          tracestart = counter

        tracewrite = None
        registers(traceout)
        disassemble(false, traceout)
        traceout.println
        traceout.flush
      }

    curpc = PC
    fetch
    opcodes(opcode)(this)
    counter += 1

    if (trace) {
      tracewrite match {
        case None =>
        case Some((address, oldvalue, newvalue, _)) =>
          Console.withOut(traceout) {
            println(f"${address.toHexString.toUpperCase}%6s  ${hexInt(oldvalue)} -> ${hexInt(newvalue)}")
            traceout.flush
          }
      }

      if (counter > tracestart + tracelimit)
        trace = false
    }
  }

  def fetch = opcode = fetchShort & 0xFFFF

  def fetchByte = fetchShort.asInstanceOf[Byte].asInstanceOf[Int]

  def fetchShort = {
    val res = prog.readShort(PC)

    PC += 2
    res
  }

  def fetchInt = {
    val res = prog.readInt(PC)

    PC += 4
    res
  }

  def step =
    if (running)
      sys.error("already running")
    else {
      running = true
      execute
      stop
    }

  def stepOver(out: PrintStream): Unit = {
//    val inst = opcodes(prog.readShort(PC) & 0xFFFF)
//
//    step
//
//    if (inst.isInstanceOf[JSR] || inst.isInstanceOf[BSR]) {
//      setSingleShotBreakpoint(subroutineReturnAddress)
//      run(out)
//    } else if (inst.isInstanceOf[TRAP] || inst == TRAPV) {
//      setSingleShotBreakpoint(exceptionReturnAddress)
//      run(out)
//    }
  }

  def printWatches(out: PrintStream) =
    out.println(
      watches map { case (addr, list) => hexAddress(addr) + ": " + list.map(hexAddress).mkString(", ") } mkString "\n")

  def run(out: PrintStream): Unit = {
    try {
      running = true

      def run: Unit = {
        if (breakpointMap.contains(PC)) {
          if (breakpointMap(PC))
            breakpointMap.remove(PC)

          running = false
        } else {
          if (running) {
            if (interruptsAvailable)
              service

            if (stopped)
              Thread.sleep(5)
            else
              execute

            run
          }
        }
      }

      if (breakpointMap.contains(PC) && !stopped)
        execute

      run
    } catch {
      case e: Exception =>
        e.printStackTrace(out)
        running = false
        resetSignal
    }

    printWatches(out)
  }

  def memoryReadAddress(address: Int) = memory.readInt(address)

  def memoryRead(address: Int, size: Size, aligned: Boolean) =
    size match {
      case BitSize | ByteSize if aligned => memory.readShort(address).asInstanceOf[Byte].asInstanceOf[Int]
      case BitSize | ByteSize            => memory.readByte(address)
      case ShortSize                     => memory.readShort(address)
      case IntSize                       => memory.readInt(address)
    }

  def memoryWrite(data: Int, address: Int, size: Size, aligned: Boolean) = {
    if (trace)
      tracewrite = Some((address, memoryRead(address, size, aligned), data, size))

    if (isWatch(address))
      watchEvent(address, curpc)

    size match {
      case BitSize | ByteSize if aligned => memory.writeShort(address, data)
      case BitSize | ByteSize            => memory.writeByte(address, data)
      case ShortSize                     => memory.writeShort(address, data)
      case IntSize                       => memory.writeInt(address, data)
    }
  }
}

object CPU {

  private val opcodes = Array.fill[Instruction](0x10000)(ILLEGAL)
  private var built = false

  private def populate(pattern: String, inst: Map[Char, Int] => Instruction) =
    for ((idx, m) <- generate(pattern))
      opcodes(idx) = inst(m)

  private def populate(insts: List[(String, Map[Char, Int] => Instruction)]): Unit =
    for ((p, c) <- insts)
      populate(p, c)

  def opcodeTable: IndexedSeq[Instruction] = synchronized {
    if (!built) {
      populate(
        List[(String, Map[Char, Int] => Instruction)](
          ))
      built = true
    }

    ArraySeq.unsafeWrapArray(opcodes)
  }

  private def generate(pattern: String) = {
    case class Variable(v: Char, seq: collection.Seq[Int], bits: List[Int])

    val Range = "([a-zA-Z]):(?:([0-9]+)-([0-9]+)((?:-[0-9]+)*)|([0-9]+(?:,[0-9]+)*))" r
    val p = pattern replace (" ", "") split ";" toIndexedSeq

    require(p.nonEmpty, "empty pattern")

    val bits = p(0)

    require(bits.length > 0, "pattern should comprise at least one bit")
    require(bits.forall(c => c == '0' || c == '1' || c.isLetter || c == '-'),
            "pattern should comprise only 0's, 1's, letters or -'s")

    val ranges = Map[Char, collection.Seq[Int]](p drop 1 map {
      case Range(v, lower, upper, null, null) => v.head -> (lower.toInt to upper.toInt)
      case Range(v, lower, upper, exceptions, null) =>
        val remove = exceptions split "-" drop 1 map (_.toInt)

        v.head -> (lower.toInt to upper.toInt).filterNot(remove contains _)
      case Range(v, null, null, null, list) => v.head -> (list split "," map (_.toInt)).toSeq
    }: _*)

    val (constant, variables) = {
      def scan(acc: Int, pos: Int, chars: List[Char], vars: Map[Char, List[Int]]): (Int, Map[Char, List[Int]]) =
        chars match {
          case Nil                       => (acc, vars)
          case '0' :: t                  => scan(acc, pos << 1, t, vars)
          case '1' :: t                  => scan(acc | pos, pos << 1, t, vars)
          case v :: t if vars contains v => scan(acc, pos << 1, t, vars + (v -> (vars(v) :+ pos)))
          case v :: t                    => scan(acc, pos << 1, t, vars + (v -> List(pos)))
        }

      scan(0, 1, bits.reverse.toList, Map())
    }

    val enumeration = new ListBuffer[(Int, Map[Char, Int])]

    def enumerate(acc: Int, vars: List[Variable], vals: Map[Char, Int]): Unit =
      vars match {
        case Nil => enumeration += ((acc, vals))
        case v :: t =>
          for (i <- v.seq)
            enumerate(acc | int2bits(0, i, v.bits), t, vals + (v.v -> i))
      }

    def int2bits(res: Int, n: Int, bits: List[Int]): Int =
      bits match {
        case Nil                   => res
        case b :: t if (n & 1) > 0 => int2bits(res | b, n >> 1, t)
        case b :: t                => int2bits(res, n >> 1, t)
      }

    enumerate(
      constant,
      variables.toList map {
        case (v, b) =>
          if (ranges contains v) {
            require(ranges(v).last < (1 << b.length), "second value of range must be less than 2^#bits")
            Variable(v, ranges(v), b)
          } else
            Variable(v, 0 until (1 << b.length), b)
      },
      Map()
    )
    enumeration.toList
  }

}
