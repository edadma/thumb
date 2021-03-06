package xyz.hyperreal.thumb

import java.io.{File, PrintWriter}

import scala.collection.mutable.ArrayBuffer

object SREC {

  def write(m: Memory, f: File, header: Seq[Byte]): Unit = {
    val out = new PrintWriter(f)
    var s1count = 0

    def record(typ: Char, items: Any*) = {
      var sum = 0
      var count = 0
      val buf = new StringBuilder

      def add(b: Int): Unit = {
        sum += b
        count += 1
        buf append hexByte(b)
      }

      for (i <- items)
        i match {
          case byte: Int =>
            add(byte)
          case bytes: Seq[_] =>
            for (b <- bytes)
              add(b.asInstanceOf[Number].intValue)
        }

      out.println("S" + typ + hexByte(count + 1) + buf + hexByte((sum + count + 1) ^ 0xFF))
    }

//		def text( s: Seq[Byte] ) = s map (b => hexByte( b )) mkString

    record('0', 0x00, 0x00, header)

    val roms = m.seqROM

    if (roms isEmpty)
      sys.error("There's no ROM to be saved.")

    for (rom <- roms) {
      val end = rom.start + rom.size

      for (rec <- rom.start until end by 16) {
        val len = 16 min (end - rec)

        record('1', rec >> 8, rec, for (i <- 0 until len) yield rom.readByte(rec + i))
        s1count += 1
      }
    }

    record('5', s1count >> 8, s1count)
    record('9', 0, 0)
    out.close
  }

  def apply(m: Memory, s: File): Int = apply(m, io.Source.fromFile(s))

  def apply(m: Memory, s: String): Int = apply(m, io.Source.fromString(s))

  def apply(m: Memory, s: io.Source): Int = {
    var startAddr = 0
    val buf = new ArrayBuffer[Byte]
    var base = 0
    var segment = 0

    def header(bytes: Vector[Byte]): Unit = {}

    def data(addr: Int, bytes: Vector[Byte]): Unit = {
      if (!buf.isEmpty && addr != base + buf.length) {
        m add ROM("SREC" + segment, base, buf.toIndexedSeq)
        segment += 1
        base = addr
        buf.clear
      } else if (buf.isEmpty)
        base = addr

      buf ++= bytes
    }

    def start(addr: Int) = startAddr = addr

    apply(s, header, data, start)

    if (!buf.isEmpty)
      m add ROM("SREC" + segment, base, buf.toIndexedSeq)

    startAddr
  }

  def apply(s: io.Source, header: Vector[Byte] => Unit, data: (Int, Vector[Byte]) => Unit, start: Int => Unit): Unit = {
    var headerSeen = false
    var count = 0

    s.getLines.zipWithIndex foreach {
      case (line0, num) =>
        val line = line0.trim

        def problem(col: Int, msg: String) =
          sys.error("error on line " + (num + 1) + ": " + msg + '\n' + line + '\n' + " " * col + '^')

        def hexb(index: Int) = {
          for (i <- index until index + 2)
            if (!("0123456789abcdefABCDEF" contains line(i))) problem(index + i, "non-hexadecial character")

          Integer.parseInt(line.substring(index, index + 2), 16)
        }

        if (!line.isEmpty) {
          if (line.length < 10) problem(0, "line too short")

          if (line.length % 2 != 0) problem(0, "line has an odd number of characters")

          if (line(0) != 'S') problem(0, "expected 'S'")

          val binary = for (i <- 2 until line.length - 2 by 2) yield hexb(i).toByte

          def byte(index: Int) = (binary(index) & 0xFF)

          def word(index: Int) = (byte(index) << 8) | byte(index + 1)

          if (((~binary.sum) & 0xFF) != hexb(line.length - 2)) problem(line.length - 2, "incorrect checksum")

          if (binary(0) != binary.length) problem(2, "incorrect count")

          line(1) match {
            case '0' =>
              if (headerSeen) problem(1, "duplicate header record")

              if (word(1) != 0) problem(4, "address field should be 0000 for header")

              headerSeen = true
              header((binary drop 3).asInstanceOf[Vector[Byte]])
            case '1' =>
              count += 1
              data(word(1), (binary drop 3).asInstanceOf[Vector[Byte]])
            case '5' =>
              if (count != word(1))
                problem(9, "incorrect record count")
            case '9' => start(word(1))
            case _   => problem(1, "unknown record type")
          }
        }
    }
  }

}
