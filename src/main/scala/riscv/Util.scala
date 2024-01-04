package riscv

import java.io.PrintWriter
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Paths}

object Util {
    def convertBinToHex(path: String): Unit = {
        val bytes = Files.readAllBytes(Paths.get(path + ".bin"))
        val pw = new PrintWriter(path + ".hex")
        for (i <- 0 until bytes.length by 4) {
            val chunk = if (i + 4 <= bytes.length) bytes.slice(i, i + 4) else {
                // Handle the case where the remaining bytes are less than 4
                bytes.slice(i, bytes.length) ++ Array.fill(4 - (bytes.length - i))(0.toByte)
            }

            val wrappedChunk = ByteBuffer.wrap(chunk).order(ByteOrder.LITTLE_ENDIAN)
            val value = wrappedChunk.getInt()
            pw.println(f"$value%08x")
        }
        pw.close()
    }
    def readBin(path: String): Array[Int] = {
        val bytes = Files.readAllBytes(Paths.get(path + ".bin"))

        val arr = new Array[Int](math.max(1, bytes.length / 4))
        if (bytes.isEmpty) {
            arr(0)
        }
        for (i <- 0 until bytes.length / 4) {
            var word = 0
            for (j <- 0 until 4) {
                word >>>= 8
                word += (bytes(i * 4 + j).toInt & 0xff) << 24
            }
            // printf("%08x\n", word)
            arr(i) = word
        }


        arr
    }
}
