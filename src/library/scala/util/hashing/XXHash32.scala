package scala.util.hashing

import java.lang.Integer.rotateLeft

class XXHash32(seed: Int) {

  var v1: Int
  var v2: Int
  var v3: Int
  var v4: Int
  var memSize: Int
  var totalLen: Long

  var memory: Array[Byte] = new Array(16)
  reset()

  private final val PRIME1: Int = -1640531535
  private final val PRIME2: Int = -2048144777
  private final val PRIME3: Int = -1028477379
  private final val PRIME4: Int = 668265263
  private final val PRIME5: Int = 374761393

  def reset() {
    v1 = seed + PRIME1 + PRIME2
    v2 = seed + PRIME2
    v3 = seed + 0
    v4 = seed - PRIME1
    totalLen = 0
    memSize = 0
  }

  @inline
  def readIntLE(buf: Array[Byte], i: Int): Int = {
    (buf(i) & 0xFF) | ((buf(i+1) & 0xFF) << 8) | ((buf(i+2) & 0xFF) << 16) | ((buf(i+3) & 0xFF) << 24);
  }


  @inline
  def writeIntLE(buf: Array[Byte], off:Int, i:Int) = {
    buf(off) = (i & 0xFF).toByte
    buf(off+1) = ((i >> 8) & 0xFF).toByte
    buf(off+2) = ((i >> 16) & 0xFF).toByte
    buf(off+3) = ((i >> 24) & 0xFF).toByte
  }

  final def getValue(): Int = {
    var h32: Int =
      if (totalLen >= 16) {
        rotateLeft(v1, 1) + rotateLeft(v2, 7) + rotateLeft(v3, 12) + rotateLeft(v4, 18)
      } else {
        seed + PRIME5
      }

    h32 += totalLen.toInt

    var off: Int = 0;
    while (off <= memSize - 4) {
      h32 += readIntLE(memory, off) * PRIME3
      h32 = rotateLeft(h32, 17) * PRIME4
      off += 4
    }

    while (off < memSize) {
      h32 += (memory(off) & 0xFF) * PRIME5
      h32 = rotateLeft(h32, 11) * PRIME1
      off += 2
    }

    h32 ^= h32 >>> 15
    h32 *= PRIME2
    h32 ^= h32 >>> 13
    h32 *= PRIME3
    h32 ^= h32 >>> 16

    h32
  }

  private final def checkRange(buf: Array[Byte], off: Int): Unit = {
    if (off < 0 || off >= buf.length) {
      throw new ArrayIndexOutOfBoundsException(off);
    }
  }

  private final def checkRange(buf: Array[Byte], off: Int, len: Int): Unit = {
    checkLength(len)
    if (len > 0) {
      checkRange(buf, off)
      checkRange(buf, off + len - 1)
    }
  }

  private final def checkLength(len: Int) {
    if (len < 0) {
      throw new IllegalArgumentException("lengths must be >= 0")
    }
  }

  final def update(buf: Array[Byte], offs: Int, len: Int): Unit = {
    var off = offs
    checkRange(buf, off, len)

    totalLen += len

    if (memSize + len < 16) { // fill in tmp buffer
      System.arraycopy(buf, off, memory, memSize, len)
      memSize += len
      return
    }

    val end = off + len

    if (memSize > 0) { // data left from previous update
      System.arraycopy(buf, off, memory, memSize, 16 - memSize)

      v1 += readIntLE(memory, 0) * PRIME2
      v1 = rotateLeft(v1, 13)
      v1 *= PRIME1

      v2 += readIntLE(memory, 4) * PRIME2
      v2 = rotateLeft(v2, 13)
      v2 *= PRIME1

      v3 += readIntLE(memory, 8) * PRIME2
      v3 = rotateLeft(v3, 13)
      v3 *= PRIME1

      v4 += readIntLE(memory, 12) * PRIME2
      v4 = rotateLeft(v4, 13)
      v4 *= PRIME1

      off += 16 - memSize
      memSize = 0
    }

    {
      val limit: Int = end - 16
      var v1 = this.v1
      var v2 = this.v2
      var v3 = this.v3
      var v4 = this.v4

      while (off <= limit) {
        v1 += readIntLE(buf, off) * PRIME2
        v1 = rotateLeft(v1, 13)
        v1 *= PRIME1
        off += 4

        v2 += readIntLE(buf, off) * PRIME2
        v2 = rotateLeft(v2, 13)
        v2 *= PRIME1
        off += 4

        v3 += readIntLE(buf, off) * PRIME2
        v3 = rotateLeft(v3, 13)
        v3 *= PRIME1
        off += 4

        v4 += readIntLE(buf, off) * PRIME2
        v4 = rotateLeft(v4, 13)
        v4 *= PRIME1
        off += 4
      }

      this.v1 = v1
      this.v2 = v2
      this.v3 = v3
      this.v4 = v4
    }

    if (off < end) {
      System.arraycopy(buf, off, memory, 0, end - off)
      memSize = end - off
    }
  }

  private def intToByteArray(a: Int): Array[Byte] = {
    val ret = new Array[Byte](4)
    writeIntLE(ret, 0, a)
    ret
}

  final def update(b: Int): Unit = {
        update(intToByteArray(b), 0, 1)
  }

  final def mix(hash: Int, data: Int): Int = {
    reset()
    memory = intToByteArray(hash)
    update(data)
    readIntLE(memory,0)
  }

  final def mixLast(hash: Int, data: Int, length: Int): Int = {
	  reset
	  memory = intToByteArray(hash)
	  update(data)
	  getValue()
  }

  final def finalizeHash(hash: Int, length: Int): Int = {
	  ???
  }


}