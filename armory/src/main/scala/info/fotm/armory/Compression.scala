package info.fotm.armory

import java.io._
import java.nio.charset.{StandardCharsets, Charset}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import Control._

object Compression {

  def copy(in: InputStream, out: OutputStream) = {
    val buffer = new Array[Byte](1024 * 10)
    var len: Int = 0
    while (len != -1) {
      len = in.read(buffer)
      if (len != -1)
        out.write(buffer, 0, len)
    }
  }

  def zip (str: String): Array[Byte] = {
    val bytes: Array[Byte] = str.getBytes(StandardCharsets.UTF_8)
    using(new ByteArrayInputStream(bytes)) { inputStream =>
      using (new ByteArrayOutputStream()) { outputStream =>
        using(new GZIPOutputStream(outputStream)) { _.write(bytes) } // close before output!
        outputStream.toByteArray
      }
    }
  }

  def unzip(bytes: Array[Byte]): String = {
    using (new ByteArrayInputStream(bytes)) { inputStream =>
      using (new ByteArrayOutputStream()) { outputStream =>
        using(new GZIPInputStream(inputStream)) { copy(_, outputStream) }
        new String(outputStream.toByteArray, StandardCharsets.UTF_8)
      }
    }
  }

  def toBase64(bytes: Array[Byte]): String = com.microsoft.azure.storage.core.Base64.encode(bytes)
  def fromBase64(str: String): Array[Byte] = com.microsoft.azure.storage.core.Base64.decode(str)

  def zipToBase64(str: String) = toBase64(zip(str))
  def unzipFromBase64(base64zip: String) = unzip(fromBase64(base64zip))
}
