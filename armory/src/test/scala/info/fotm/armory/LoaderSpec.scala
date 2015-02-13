package info.fotm.armory

import com.typesafe.config.ConfigFactory
import info.fotm.armory.models._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.json4s._, org.json4s.native.JsonMethods._

class LoaderSpec extends FlatSpec with Matchers with ScalaFutures {

  val apiKey = Settings.apiKey


  "Loader" should "generate correct url for 2v2 leaderboards" in {
    val loader = new Loader(apiKey, US)
    val url = loader.leaderboardsUrl(Twos).url
    url should equal (s"https://us.api.battle.net/wow/leaderboard/2v2?locale=en_US&apikey=$apiKey")
  }

  it should "generate correct url for non default locale" in {
    val loader = new Loader(apiKey, US, Some(EsMx))
    val url = loader.leaderboardsUrl(Threes).url
    url should equal (s"https://us.api.battle.net/wow/leaderboard/3v3?locale=es_MX&apikey=$apiKey")
  }

  it should "load correct bracket" in {
    val loader = new Loader(apiKey, US)
    val t = timeout(Span(40, Seconds))
    val i = interval(Span(0.5, Seconds))

    whenReady(loader.leaderboards(Fives), t, i) { lb =>
      lb.rows.length should be > (3000)
      val first = lb.rows.head
      first.ranking should equal (1)

      val definedSpecs: Int = lb.rows.count(p => p.characterInfo.spec.spec.isDefined)
      definedSpecs should equal (lb.rows.length)
    }
  }

}
