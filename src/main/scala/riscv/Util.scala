import java.io.PrintWriter
import java.nio.file.{Files, Paths}

object Util {
    def convertBinToHex(path: String): Unit = {
        val bytes = Files.readAllBytes(Paths.get(path + ".bin"))
        val pw = new PrintWriter(path + ".hex")
        bytes.foreach { byte =>
            pw.println(f"$byte%02x")
        }
        pw.close()
    }

}
