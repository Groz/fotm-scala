package info.fotm.armory.models

import org.scalatest._

class ModelsSpec extends FlatSpec with Matchers {

  "All mages" should "be ranged" in {
    Characters.mages.map(Characters.isRanged) should contain only (true)
  }

  it should "not be healers" in {
    Characters.mages.map(Characters.isHealer) should contain only (false)
  }

  "Create" should "create proper arms warrior" in {
    Characters.create(1, Some(71)) should equal (ArmsWarrior)
  }

  it should "create nowarrior if spec is missing" in {
    Characters.create(1, None) should equal (NoWarrior)
  }

  it should "throw if class id is incorrect" in {
    a [NoSuchElementException] should be thrownBy {
      Characters.create(0, None)
    }
  }

  it should "throw if spec is incorrect" in {
    a [NoSuchElementException] should be thrownBy {
      Characters.create(1, Some(33))
    }
  }

  it should "throw if spec is for another class" in {
    a [NoSuchElementException] should be thrownBy {
      Characters.create(1, Some(268))
    }
  }

  "Brackets" should "have correct url for 2v2" in {
    Twos.url should equal ("2v2")
  }

  it should "have correct url for rbg" in {
    Rbg.url should equal ("rbg")
  }

  "Gender" should "be buildable from id" in {
    Gender(1) should equal (Female)
  }

  "Gender" should "throw if id is outside of range" in {
    a [NoSuchElementException] should be thrownBy {
      Gender(3)
    }
  }

}
