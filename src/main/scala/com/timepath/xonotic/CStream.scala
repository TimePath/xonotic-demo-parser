package com.timepath.xonotic

import java.io.{EOFException, InputStream}

import org.apache.commons.io.input.SwappedDataInputStream

class CStream(input: InputStream) extends SwappedDataInputStream(input) {

  def readString(delim: Int = 0): String = {
    var c = read
    if (c == -1) throw new EOFException
    val builder = new StringBuilder
    while (c != -1 && c != delim) {
      builder.append(c.toChar)
      c = read
    }
    builder.toString()
  }

  override def readLine = readString(10)

  def read32 = readInt

  def readu32: Long = readInt & 0xFFFFFFFF

  def read16 = readShort

  def readu16: Int = readUnsignedShort

  def read8: Int = readByte

  def readu8 = readUnsignedByte
}
