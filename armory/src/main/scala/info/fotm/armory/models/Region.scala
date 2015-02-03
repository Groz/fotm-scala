package info.fotm.armory.models

sealed abstract class Region[T <: Locale](val name: String, val locales: T*)

case object EU extends Region[EuLocale]("EU", EnGb, DeDe, EsEs, FrFr, ItIt, PlPl, PtPt, RuRu)
case object KR extends Region[KrLocale]("KR", KoKr)
case object TW extends Region[TwLocale]("TW", ZhTw)
case object US extends Region[UsLocale]("US", EnUs, PtBr, EsMx)
// TODO: add CN when available

object Region {
  val all = Set(EU, KR, TW, US/*, CN */)
}