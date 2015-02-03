package info.fotm.armory.models

sealed trait CharacterClass[+TSpec <: CharacterSpec] {
  val id: Int
  val spec: Option[TSpec]
}

sealed abstract class Warrior(val spec: Option[WarriorSpec]) extends CharacterClass[WarriorSpec] { val id = 1 }
case object ArmsWarrior extends Warrior(Some(ArmsWarriorSpec))
case object FuryWarrior extends Warrior(Some(FuryWarriorSpec))
case object ProtectionWarrior extends Warrior(Some(ProtectionWarriorSpec))
case object NoWarrior extends Warrior(None)

sealed abstract class Paladin(val spec: Option[PaladinSpec]) extends CharacterClass[PaladinSpec] { val id = 2 }
case object HolyPaladin extends Paladin(Some(HolyPaladinSpec))
case object ProtectionPaladin extends Paladin(Some(ProtectionPaladinSpec))
case object RetributionPaladin extends Paladin(Some(RetributionPaladinSpec))
case object NoPaladin extends Paladin(None)

sealed abstract class Hunter(val spec: Option[HunterSpec]) extends CharacterClass[HunterSpec] { val id = 3 }
case object BeastMasteryHunter extends Hunter(Some(BeastMasteryHunterSpec))
case object MarksmanshipHunter extends Hunter(Some(MarksmanshipHunterSpec))
case object SurvivalHunter extends Hunter(Some(SurvivalHunterSpec))
case object NoHunter extends Hunter(None)

sealed abstract class Rogue(val spec: Option[RogueSpec]) extends CharacterClass[RogueSpec] { val id = 4 }
case object AssassinationRogue extends Rogue(Some(AssassinationRogueSpec))
case object CombatRogue extends Rogue(Some(CombatRogueSpec))
case object SubtletyRogue extends Rogue(Some(SubtletyRogueSpec))
case object NoRogue extends Rogue(None)

sealed abstract class Priest(val spec: Option[PriestSpec]) extends CharacterClass[PriestSpec] { val id = 5 }
case object DisciplinePriest extends Priest(Some(DisciplinePriestSpec))
case object HolyPriest extends Priest(Some(HolyPriestSpec))
case object ShadowPriest extends Priest(Some(ShadowPriestSpec))
case object NoPriest extends Priest(None)

sealed abstract class DeathKnight(val spec: Option[DeathKnightSpec]) extends CharacterClass[DeathKnightSpec] { val id = 6 }
case object BloodDeathKnight extends DeathKnight(Some(BloodDeathKnightSpec))
case object FrostDeathKnight extends DeathKnight(Some(FrostDeathKnightSpec))
case object UnholyDeathKnight extends DeathKnight(Some(UnholyDeathKnightSpec))
case object NoDeathKnight extends DeathKnight(None)

sealed abstract class Shaman(val spec: Option[ShamanSpec]) extends CharacterClass[ShamanSpec] { val id = 7 }
case object ElementalShaman extends Shaman(Some(ElementalShamanSpec))
case object EnhancementShaman extends Shaman(Some(EnhancementShamanSpec))
case object RestorationShaman extends Shaman(Some(RestorationShamanSpec))
case object NoShaman extends Shaman(None)

sealed abstract class Mage(val spec: Option[MageSpec]) extends CharacterClass[MageSpec] { val id = 8 }
case object ArcaneMage extends Mage(Some(ArcaneMageSpec))
case object FireMage extends Mage(Some(FireMageSpec))
case object FrostMage extends Mage(Some(FrostMageSpec))
case object NoMage extends Mage(None)

sealed abstract class Warlock(val spec: Option[WarlockSpec]) extends CharacterClass[WarlockSpec] { val id = 9 }
case object AfflictionWarlock extends Warlock(Some(AfflictionWarlockSpec))
case object DemonologyWarlock extends Warlock(Some(DemonologyWarlockSpec))
case object DestructionWarlock extends Warlock(Some(DestructionWarlockSpec))
case object NoWarlock extends Warlock(None)

sealed abstract class Monk(val spec: Option[MonkSpec]) extends CharacterClass[MonkSpec] { val id = 10 }
case object BrewmasterMonk extends Monk(Some(BrewmasterMonkSpec))
case object WindwalkerMonk extends Monk(Some(WindwalkerMonkSpec))
case object MistweaverMonk extends Monk(Some(MistweaverMonkSpec))
case object NoMonk extends Monk(None)

sealed abstract class Druid(val spec: Option[DruidSpec]) extends CharacterClass[DruidSpec] { val id = 11 }
case object BalanceDruid extends Druid(Some(BalanceDruidSpec))
case object FeralDruid extends Druid(Some(FeralDruidSpec))
case object GuardianDruid extends Druid(Some(GuardianDruidSpec))
case object RestorationDruid extends Druid(Some(RestorationDruidSpec))
case object NoDruid extends Druid(None)

sealed trait CharacterSpec {
  val id: Int
}

sealed abstract class WarriorSpec(val id: Int) extends CharacterSpec
case object ArmsWarriorSpec extends WarriorSpec(71)
case object FuryWarriorSpec extends WarriorSpec(72)
case object ProtectionWarriorSpec extends WarriorSpec(73)

sealed abstract class PaladinSpec(val id: Int) extends CharacterSpec
case object HolyPaladinSpec extends PaladinSpec(65)
case object ProtectionPaladinSpec extends PaladinSpec(66)
case object RetributionPaladinSpec extends PaladinSpec(70)

sealed abstract class HunterSpec(val id: Int) extends CharacterSpec
case object BeastMasteryHunterSpec extends HunterSpec(253)
case object MarksmanshipHunterSpec extends HunterSpec(254)
case object SurvivalHunterSpec extends HunterSpec(255)

sealed abstract class RogueSpec(val id: Int) extends CharacterSpec
case object AssassinationRogueSpec extends RogueSpec(259)
case object CombatRogueSpec extends RogueSpec(260)
case object SubtletyRogueSpec extends RogueSpec(261)

sealed abstract class PriestSpec(val id: Int) extends CharacterSpec
case object DisciplinePriestSpec extends PriestSpec(256)
case object HolyPriestSpec extends PriestSpec(257)
case object ShadowPriestSpec extends PriestSpec(258)

sealed abstract class DeathKnightSpec(val id: Int) extends CharacterSpec
case object BloodDeathKnightSpec extends DeathKnightSpec(250)
case object FrostDeathKnightSpec extends DeathKnightSpec(251)
case object UnholyDeathKnightSpec extends DeathKnightSpec(252)

sealed abstract class ShamanSpec(val id: Int) extends CharacterSpec
case object ElementalShamanSpec extends ShamanSpec(262)
case object EnhancementShamanSpec extends ShamanSpec(263)
case object RestorationShamanSpec extends ShamanSpec(264)

sealed abstract class MageSpec(val id: Int) extends CharacterSpec
case object ArcaneMageSpec extends MageSpec(62)
case object FireMageSpec extends MageSpec(63)
case object FrostMageSpec extends MageSpec(64)

sealed abstract class WarlockSpec(val id: Int) extends CharacterSpec
case object AfflictionWarlockSpec extends WarlockSpec(265)
case object DemonologyWarlockSpec extends WarlockSpec(266)
case object DestructionWarlockSpec extends WarlockSpec(267)

sealed abstract class MonkSpec(val id: Int) extends CharacterSpec
case object BrewmasterMonkSpec extends MonkSpec(268)
case object WindwalkerMonkSpec extends MonkSpec(269)
case object MistweaverMonkSpec extends MonkSpec(270)

sealed abstract class DruidSpec(val id: Int) extends CharacterSpec
case object BalanceDruidSpec extends DruidSpec(102)
case object FeralDruidSpec extends DruidSpec(103)
case object GuardianDruidSpec extends DruidSpec(104)
case object RestorationDruidSpec extends DruidSpec(105)

object Characters {
  lazy val warriors: Set[CharacterClass[CharacterSpec]] = Set(ArmsWarrior, FuryWarrior, ProtectionWarrior, NoWarrior)
  lazy val paladins = Set(HolyPaladin, ProtectionPaladin, RetributionPaladin, NoPaladin)
  lazy val hunters = Set(BeastMasteryHunter, MarksmanshipHunter, SurvivalHunter, NoHunter)
  lazy val rogues = Set(AssassinationRogue, CombatRogue, SubtletyRogue, NoRogue)
  lazy val priests = Set(DisciplinePriest, HolyPriest, ShadowPriest, NoPriest)
  lazy val dks = Set(BloodDeathKnight, FrostDeathKnight, UnholyDeathKnight, NoDeathKnight)
  lazy val shamans = Set(ElementalShaman, EnhancementShaman, RestorationShaman, NoShaman)
  lazy val warlocks = Set(AfflictionWarlock, DemonologyWarlock, DestructionWarlock, NoWarlock)
  lazy val mages = Set(ArcaneMage, FireMage, FrostMage, NoMage)
  lazy val monks = Set(BrewmasterMonk, WindwalkerMonk, MistweaverMonk, NoMonk)
  lazy val druids = Set(BalanceDruid, FeralDruid, GuardianDruid, RestorationDruid, NoDruid)

  lazy val all: Set[CharacterClass[CharacterSpec]] =
    warriors ++ paladins ++ hunters ++ rogues ++ priests ++
      dks ++ shamans ++ warlocks ++ mages ++ monks ++ druids

  lazy val ranged = Set(
    ArcaneMage, FireMage, FrostMage, NoMage,
    BalanceDruid,
    BeastMasteryHunter, MarksmanshipHunter, SurvivalHunter,
    ShadowPriest,
    ElementalShaman,
    AfflictionWarlock, DemonologyWarlock, DestructionWarlock)

  lazy val healers = Set(RestorationDruid, MistweaverMonk, HolyPaladin, DisciplinePriest, HolyPriest, RestorationShaman)

  def isRanged = ranged.contains _
  def isHealer = healers.contains _

  private def specMatches(cs: Option[CharacterSpec], specId: Option[Int]): Boolean =
    (!cs.isDefined && !specId.isDefined) ||
    (for { sid <- specId; spec <- cs } yield sid == spec.id).getOrElse(false)

  def create(classId: Int, specId: Option[Int]): CharacterClass[CharacterSpec] =
    all.find(charClass => charClass.id == classId && specMatches(charClass.spec, specId)).
      getOrElse(throw new NoSuchElementException(s"classId: $classId, specId: $specId"))

}