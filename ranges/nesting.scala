/*
# rangeNestings(rangeHash) => nesting
#
# See the block comment at the end of this file for an example run.
#
*/

import scalaz._, Scalaz._

object RangeNestingDemo extends App {
  def collectChildren(
    ranges: List[(String, (Int, Int))]
  ): Stream[Tree[String]] = ranges match {
    case Nil => Stream.empty
    case (label, (_, last)) :: rest =>
      val (inside, outside) = rest.span(_._2._1 <= last)

      Tree.node(label, collectChildren(inside)) #:: collectChildren(outside)
  }

  def rangeNestings(rangeHash: Map[String, (Int, Int)]) = collectChildren(
    rangeHash.toList.sortBy {
      case (_, (first, last)) => (first, -last)
    }
  )

  val ranges = Map(
    "Apple" -> (0, 10),
    "Banana" -> (11, 20),
    "appleArtichoke" -> (0, 3),
    "appleBanana" -> (4, 7),
    "appleCranberry" -> (8, 11),
    "appleArtichokeApricot" -> (0, 2),
    "appleArtichokeBBQ" -> (3, 4),
    "appleBananaApricot" -> (5, 7),
    "appleCranberryApricot" -> (8, 9),
    "appleArtichokeBBQ" -> (10, 11)
  )

  println()
  println(ranges)
  println()
  rangeNestings(ranges).foreach(range => println(range.drawTree))
  println()
}

/*
This should result in the following output (reformatted and 'List' and 'RangeNesting' removed for readability):

Map(
  appleBanana -> RangeLimits(4,7), 
  Banana -> RangeLimits(11,20), 
  appleArtichoke -> RangeLimits(0,3), 
  appleArtichokeBBQ -> RangeLimits(10,11), 
  appleBananaApricot -> RangeLimits(5,7), 
  appleCranberryApricot -> RangeLimits(8,9), 
  Apple -> RangeLimits(0,10), 
  appleArtichokeApricot -> 
  RangeLimits(0,2), 
  appleCranberry -> RangeLimits(8,11)
)

(
  (
    Apple, (
      (
        appleArtichoke, (
          (
            appleArtichokeApricot,()
          )
        )
      ), 
      (
        appleBanana, (
          (
            appleBananaApricot,()
          )
        )
      ), 
      (
        appleCranberry, (
          (
            appleCranberryApricot,()
          ), 
          (
            appleArtichokeBBQ,()
          )
        )
      )
    )
  ), 
  (
    Banana,()
  )
)
*/

