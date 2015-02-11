package info.fotm.armory

import java.io.File
import java.util.{Calendar, Date}
import scala.collection.JavaConversions._
import com.microsoft.azure.storage._
import com.microsoft.azure.storage.blob._

object AzureBackfillApp extends App {

  val azure_account_name = ""
  val azure_account_key = ""

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

  //val blob: CloudBlockBlob = container.getBlockBlobReference("image1.jpg")
  //val destinationFile: File = new File(sourceFile.getParentFile(), "image1Download.tmp")
  //blob.downloadToFile(destinationFile.getAbsolutePath())
}

