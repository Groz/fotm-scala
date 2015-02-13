package info.fotm.armory

import akka.actor._
import com.typesafe.config.{ConfigFactory, Config}
import info.fotm.armory.{Loader => ArmoryLoader}
import info.fotm.armory.models._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class ScannerActor(val loader: ArmoryLoader[Locale], val bracket: Bracket, minerActor: ActorRef) extends Actor {

  import scala.concurrent.ExecutionContext.Implicits.global

  def receive: Receive = {

    case "scan" =>
      loader.leaderboards(bracket).onComplete {
        case Success(leaderboard: Leaderboard) => minerActor ! leaderboard
        case Failure(ex) => println(ex)
      }

  }

}

class MinerActor(val bracket: Bracket) extends Actor {

  override def receive: Receive = {
    case leaderboard: Leaderboard => println(leaderboard)
  }

}

object MainApp extends App {
  val loader = new ArmoryLoader(Settings.apiKey, US)

  val system = ActorSystem("the-actor-system")
  val bracket = Twos
  val minerActor: ActorRef = system.actorOf(Props(classOf[MinerActor], bracket))
  val scannerActor: ActorRef = system.actorOf(Props(classOf[ScannerActor], loader, bracket, minerActor))

  import system.dispatcher

  system.scheduler.schedule(0 milliseconds,
    1 second,
    scannerActor,
    "scan")

  /* Wait for message processing and shutdown
  // (not needed in normal application)
  java.lang.Thread.sleep(5000)
  system.shutdown()
  */
}


