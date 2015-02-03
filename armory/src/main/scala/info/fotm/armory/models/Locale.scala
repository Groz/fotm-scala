package info.fotm.armory.models

sealed abstract class Locale(val name: String)

sealed abstract class EuLocale(name: String) extends Locale(name)
case object EnGb extends EuLocale("en_GB")
case object DeDe extends EuLocale("de_DE")
case object EsEs extends EuLocale("es_ES")
case object FrFr extends EuLocale("fr_FR")
case object ItIt extends EuLocale("it_IT")
case object PlPl extends EuLocale("pl_PL")
case object PtPt extends EuLocale("pt_PT")
case object RuRu extends EuLocale("ru_RU")

sealed abstract class KrLocale(name: String) extends Locale(name)
case object KoKr extends KrLocale("ko_KR")

sealed abstract class TwLocale(name: String) extends Locale(name)
case object ZhTw extends TwLocale("zh_TW")

sealed abstract class UsLocale(name: String) extends Locale(name)
case object EnUs extends UsLocale("en_US")
case object PtBr extends UsLocale("pt_BR")
case object EsMx extends UsLocale("es_MX")


