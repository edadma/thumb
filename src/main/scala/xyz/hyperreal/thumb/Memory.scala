package xyz.hyperreal.thumb

import collection.mutable.ArrayBuffer

trait Addressable {

  def name: String

  def start: Int

  def size: Int

  def readByte(addr: Int): Int

  def writeByte(addr: Int, value: Int): Unit

  def isRAM = isInstanceOf[RAM]

  def isROM = isInstanceOf[ROM]

  def isDevice = isInstanceOf[Device]

  def programByte(addr: Int, value: Int) = writeByte(addr, value)

  def programShort(addr: Int, value: Int): Unit = {
    programByte(addr, value & 0xFF)
    programByte(addr + 1, value >> 8)
  }

  def programInt(addr: Int, value: Int) = {
    programShort(addr, value & 0xFFFF)
    programShort(addr + 2, value >> 16)
  }

  def programLong(addr: Int, value: Long) = {
    programInt(addr, value.asInstanceOf[Int])
    programInt(addr + 4, (value >> 32).asInstanceOf[Int])
  }

  def readShort(addr: Int) = (readByte(addr) << 8) | (readByte(addr + 1) & 0xFF)

  def writeShort(addr: Int, value: Int): Unit = {
    writeByte(addr, value >> 8)
    writeByte(addr + 1, value & 0xFF)
  }

  def readInt(addr: Int) = (readShort(addr) << 16) | (readShort(addr + 2) & 0xFFFF)

  def writeInt(addr: Int, value: Int): Unit = {
    writeShort(addr, value >> 16)
    writeShort(addr + 2, value & 0xFFFF)
  }

  def readLong(addr: Int) = (readInt(addr).asInstanceOf[Long] << 32) | (readInt(addr + 4) & 0xFFFFFFFFL)

  def writeLong(addr: Int, value: Long): Unit = {
    writeInt(addr, (value >> 32).asInstanceOf[Int])
    writeInt(addr + 4, value.asInstanceOf[Int])
  }
}

class RAM(val name: String, val start: Int, end: Int) extends Addressable {

  require(start >= 0)
  require(end >= start)

  val size = (end - start + 1)

  protected val mem = new Array[Byte](size)

  def clear =
    for (i <- 0 until size)
      mem(i) = 0

  def readByte(addr: Int) = mem((addr - start))

  def writeByte(addr: Int, value: Int) = mem((addr - start)) = value.asInstanceOf[Byte]

  override def toString = s"$name RAM: ${hexAddress(start.toInt)}-${hexAddress(end.toInt)}"
}

class ROM(val name: String, val start: Int, end: Int) extends Addressable {

  require(start >= 0)
  require(end >= start)

  val size = (end - start + 1)

  protected val mem = new Array[Byte](size)

  def readByte(addr: Int) = mem((addr - start))

  def writeByte(addr: Int, value: Int) =
    sys.error(
      "read only memory: " + (addr & 0xffff).toHexString + " (tried to write " + (value & 0xff).toHexString + ")")

  override def programByte(addr: Int, value: Int) = mem((addr - start)) = value.asInstanceOf[Byte]

  override def toString = s"$name ROM: ${hexAddress(start.toInt)}-${hexAddress(start.toInt + size - 1)}"

}

object ROM {
  def apply(name: String, data: String): ROM = {
    apply(name, 0, Hex(data))
  }

  def apply(name: String, start: Int, data: Seq[Byte]) = {
    new ROM(name, start, start + data.length - 1) {
      data.copyToArray(mem)
    }
  }

  def code(name: String, start: Int, data: Seq[Int]) = {
    new ROM(name, start, start + data.length * 2 - 1) {
      for ((inst, idx) <- data zipWithIndex)
        programShort(start + idx * 2, inst)
    }
  }
}

trait Device extends Addressable {

  protected var cpu: CPU = null

  def connectTo(cpu: CPU): Unit = {
    this.cpu = cpu
    cpu.resettable(this)
  }

  def init = {}

  def reset = {}

  override def toString = s"$name device: ${hexAddress(start)}-${hexAddress(start + size - 1)}"

}

abstract class SingleAddressDevice extends Device {

  val size = 1

  override def toString = s"$name device: ${hexAddress(start)}"

}

abstract class ReadOnlyDevice extends SingleAddressDevice {

  def writeByte(addr: Int, value: Int) = sys.error("read only device")

}

abstract class WriteOnlyDevice extends SingleAddressDevice {

  def readByte(addr: Int) = sys.error("write only device")

}

abstract class Memory extends Addressable {

  val name = "System memory"
  protected val regions = new ArrayBuffer[Addressable]
  protected var first: Int = 0
  protected var end: Int = 0

  def init: Unit

  init

  protected def lookup(addr: Int) =
    regions.indexWhere(r => r.start <= addr && r.start + r.size > addr) match {
      case -1  => null
      case ind => regions(ind)
    }

  def valid(addr: Int) = lookup(addr) ne null

  def find(addr: Int) =
    lookup(addr) match {
      case null => sys.error(addr.toHexString + " is not an addressable memory location")
      case r    => r
    }

  def start = first

  def size = (end - first)

  def readByte(addr: Int) = find(addr).readByte(addr)

  def writeByte(addr: Int, value: Int) = find(addr).writeByte(addr, value)

  override def readShort(addr: Int) = find(addr).readShort(addr)

  override def writeShort(addr: Int, value: Int) = find(addr).writeShort(addr, value)

  override def readInt(addr: Int) = find(addr).readInt(addr)

  override def writeInt(addr: Int, value: Int) = find(addr).writeInt(addr, value)

  override def readLong(addr: Int) = find(addr).readLong(addr)

  override def writeLong(addr: Int, value: Long) = find(addr).writeLong(addr, value)

  override def programByte(addr: Int, value: Int) = find(addr).programByte(addr, value)

  def addressable(addr: Int) = lookup(addr) ne null

  protected def find(addr: Int, pred: Addressable => Boolean) =
    lookup(addr) match {
      case null => false
      case r    => pred(r)
    }

  def device(addr: Int) = find(addr, _.isDevice)

  def memory(addr: Int) = find(addr, r => r.isRAM || r.isROM)

  def rom(addr: Int) = find(addr, _.isROM)

  def remove(name: String): Unit = {
    regions.indexWhere(_.name == name) match {
      case -1 => sys.error("not found: " + name)
      case ind =>
        if (regions(ind) isDevice)
          regions(ind).asInstanceOf[Device].reset

        regions remove ind
    }
  }

  def seqDevice = (regions filter (_.isDevice)).asInstanceOf[collection.Seq[Device]]

  def seqROM = regions filter (_.isROM)

  def removeROM =
    for (r <- seqROM)
      regions -= r

  def removeRAM =
    for (r <- regions filter (r => r.isRAM && !r.isDevice))
      regions -= r

  def removeDevices =
    for (r <- seqDevice) {
      r.reset
      regions -= r
    }

  def code = {
    val roms = seqROM

    if (roms isEmpty)
      0
    else
      roms.head.start
  }

  def clearRAM =
    for (r <- regions filter (_.isRAM))
      r.asInstanceOf[RAM].clear

  def addHexdump(src: io.Source) =
    for (Hexdump.Section(name, start, data) <- Hexdump.read(src))
      add(ROM(name, start, data))

  def add(region: Addressable): Unit = {
    regions find (r => r.start <= region.start && region.start < r.start + r.size) match {
      case Some(r) =>
        sys.error(
          hexAddress(region.start) + ", " + hexAddress(region.size) + " overlaps " + hexAddress(r.start) + ", " + hexAddress(
            r.size))
      case None =>
    }

    regions find (_.name == region.name) match {
      case Some(r) => sys.error("duplicate region: " + region.name)
      case None    =>
    }

    regions indexWhere (_.start >= region.start + region.size) match {
      case -1 =>
        regions += region
        end = region.size + region.start

        if (regions isEmpty)
          first = region.start
      case ind =>
        regions.insert(ind, region)

        if (ind == 0)
          first = region.start
    }
  }

  def reset = seqDevice foreach (_.init)

  override def toString = regions mkString "\n"
}
