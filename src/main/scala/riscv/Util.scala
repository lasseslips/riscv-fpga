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

}
