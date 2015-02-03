package info.fotm.watcher

import akka.actor.{ActorSystem, ActorRef, Props, Actor}
import akka.actor.Actor.Receive
import info.fotm.armory.{Loader => ArmoryLoader}
import info.fotm.armory.models._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class WatcherActor(val loader: ArmoryLoader[Locale], val bracket: Bracket) extends Actor {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def receive: Receive = {

    case "scan" =>
      println(s"Received message: scan")

      loader.leaderboards(bracket).onComplete {
        case Success(entries) => println(entries)
        case Failure(ex) => println(ex)
      }

  }

}

object Main extends App {
  val apiKey = ""
  val loader = new ArmoryLoader(apiKey, US)

  val system = ActorSystem("the-actor-system")
  val watcherActor: ActorRef = system.actorOf(Props(classOf[WatcherActor], loader, Twos))

  import system.dispatcher

  //Schedules a function to be executed (send a message to the testActor) after 50ms
  system.scheduler.schedule(0 milliseconds,
    500 milliseconds,
    watcherActor,
    "scan");

  // Wait for message processing and shutdown
  // (not needed in normal application)
  java.lang.Thread.sleep(5000)
  system.shutdown()
}