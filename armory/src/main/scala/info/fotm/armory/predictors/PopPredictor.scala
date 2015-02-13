package info.fotm.armory.predictors

import info.fotm.armory.Common.Diff
import info.fotm.armory.TeamPredictor
import info.fotm.armory.models.{Team, Bracket}

trait PopPredictor extends TeamPredictor {
   protected val iterations = 10

   abstract override def apply(bracket: Bracket, diffs: Set[Diff]): Set[Team] = {
     def calc = super.apply(bracket, diffs)

     (0 until iterations)
       .flatMap(_ => calc)
       .groupBy(identity).mapValues(_.size).toSeq
       .sortBy { case (team, count) => -count }
       .foldLeft(List[(Team, Int)]()) { (teams, candidate) =>
         if (teams.exists(t => t._1.chars.intersect(candidate._1.chars).size != 0))
           teams
         else
           candidate :: teams // note: reverses order
       }
       .take(diffs.size / bracket.size)
       .map { case (team, count) =>
       team
     }.toSet
   }
 }
