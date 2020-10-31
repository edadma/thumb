package xyz.hyperreal.thumb

import java.time.LocalTime
import java.time.temporal.ChronoField

class CPUWithServices extends CPU {

//  override def trap(vector: Int) = {
//    def prt =
//      for (c <- A(1) to A(1) + D(1) & 0xFFFF)
//        print(memoryRead(c, ByteSize, false).toChar)
//
//    def prtz = {
//      def prtz(addr: Int): Unit =
//        memoryRead(addr, ByteSize, false) match {
//          case 0 =>
//          case c =>
//            print(c.toChar)
//            prtz(addr + 1)
//        }
//
//      prtz(A(1))
//    }
//
//    vector match {
//      case 15 =>
//        R(0).toShort match {
//          case 0 =>
//            prt
//            println
//          case 1 => prt
//          case 2 =>
//            val line = io.StdIn.readLine
//
//            for ((c, i) <- line zipWithIndex)
//              memoryWrite(c, A(1) + i, ByteSize, false)
//
//            memoryWrite(0, A(1) + line.length, ByteSize, false)
//            R(1) = line.length
//          case 3 => print(R(1))
//          case 4 => R(1) = io.StdIn.readInt
//          case 5 => R(1) = io.StdIn.readChar
//          case 6 => print(R(1).toChar)
//          case 7 =>
//            val time = System.currentTimeMillis
//
//            R(0) = (time >> 32).toInt
//            R(1) = time.toInt
//          case 8  => R(1) = LocalTime.now.get(ChronoField.MILLI_OF_DAY) / 10
//          case 9  => stop
//          case 10 => print(java.lang.Double.longBitsToDouble((R(1).toLong << 32) | (R(2) & 0xFFFFFFFFL)))
//          case 11 => print(R(1) & 0xFFFFFFFFL)
//          case 12 => print(R(1).toHexString)
//          case 13 =>
//            prtz
//            println
//          case 14 => prtz
//          case 15 => print((R(1).toLong << 32) | (R(2) & 0xFFFFFFFFL))
//          case 16 => println(R(1))
//          case _  => sys.error(s"unknown task number: ${R(0)}")
//        }
//
//        true
//      case _ => false
//    }
//  }

}
