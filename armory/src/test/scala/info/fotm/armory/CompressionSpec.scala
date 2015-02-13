package info.fotm.armory

import java.nio.charset.StandardCharsets

import info.fotm.armory.models.{Twos, US}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, FlatSpec}

class CompressionSpec extends FlatSpec with Matchers with ScalaFutures {

  "zip" should "not throw" in {
    val uncompressed = "Hello, world"
    val zipped = Compression.zip(uncompressed)
  }

  "zip/unzip" should "receive same object when converted back and forth" in {
    val uncompressed = "Hello, world"
    val zipped = Compression.zip(uncompressed)
    val unzipped = Compression.unzip(zipped)
    unzipped should equal (uncompressed)
  }

  "base64" should "encode user:pass as dXNlcjpwYXNz" in {
    val base = "user:pass"
    val base64 = Compression.toBase64(base.getBytes(StandardCharsets.UTF_8))
    base64 should equal ("dXNlcjpwYXNz")
  }

  it should "decode dXNlcjpwYXNz as user:pass" in {
    val base64 = "dXNlcjpwYXNz"
    val decoded = Compression.fromBase64(base64)
    new String(decoded, StandardCharsets.UTF_8) should equal ("user:pass")
  }

  "zip64/unzip64" should "receive same object when converted back and forth" in {
    val uncompressed = "Hello, world"
    val zipped = Compression.zipToBase64(uncompressed)
    val unzipped = Compression.unzipFromBase64(zipped)
    unzipped should equal (uncompressed)
  }
}