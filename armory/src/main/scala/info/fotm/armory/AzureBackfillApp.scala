package info.fotm.armory

import java.io._
import java.util.zip.GZIPInputStream
import java.util.{Calendar, Date}
import scala.collection.JavaConversions._
import com.microsoft.azure.storage._
import com.microsoft.azure.storage.blob._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import scala.io.BufferedSource


object AzureBackfillApp extends App {

  val azure_account_name = Settings.azureStorageAccount
  val azure_account_key = Settings.azureStorageKey

  val storageConnectionString =
    s"DefaultEndpointsProtocol=http;AccountName=$azure_account_name;AccountKey=$azure_account_key"

  val account: CloudStorageAccount = CloudStorageAccount.parse(storageConnectionString)
  val serviceClient: CloudBlobClient = account.createCloudBlobClient()

  // Container name must be lower case.
  val container: CloudBlobContainer = serviceClient.getContainerReference("snapshots")

  def getDir(path: String) = container.listBlobs(path).toSeq.head.asInstanceOf[CloudBlobDirectory]

  println("Connected...")

  val root: CloudBlobDirectory = getDir("EU/3v3")
  // CloudBlob, CloudBlobDirectory

  val blobs = for {
    item <- root.listBlobs.view
    blob = item.asInstanceOf[CloudBlob]
  } yield blob

  val snapshotBlob: CloudBlob = blobs.take(1).toList.head
  println("Downloading file...")
  //snapshotBlob.downloadToFile("1.txt")
  println("Reading file...")
  val base64zip: String = io.Source.fromFile("1.txt").mkString
  println("Uncompressing...")
  val uncompressed: String = Compression.unzipFromBase64(base64zip)
  println(uncompressed)

  def printForLastMonth = {
    val monthAgo = {
      val cal = Calendar.getInstance()
      cal.add(Calendar.MONTH, -1)
      cal.getTime()
    }

    var i = 0

    println("Listing blobs...")
    val lastMonth = for {
      item <- root.listBlobs.view
      blob = item.asInstanceOf[CloudBlob]
      props = blob.getProperties if props.getLastModified.after(monthAgo)
    } yield (blob.getUri, props.getLastModified)

    println("Printing...")
    lastMonth.foreach { case (blobUri, blobModified) =>
      println(blobUri, blobModified)
      i += 1
      println(i)
    }
  }

  //val blob: CloudBlockBlob = container.getBlockBlobReference("image1.jpg")
  //val destinationFile: File = new File(sourceFile.getParentFile(), "image1Download.tmp")
  //blob.downloadToFile(destinationFile.getAbsolutePath())
}

import java.nio.charset.StandardCharsets
import sun.misc._

