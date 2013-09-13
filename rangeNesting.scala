/*
# rangeNestings(rangeHash) => nesting
#
# See the block comment at the end of this file for an example run.
#
*/

case class RangeNesting(label: String, children: List[RangeNesting])
case class RangeLimits(first: Int, last: Int)

def pretty(nestings: List[RangeNesting], depth: Int = 0): String = {
  def each(nesting: RangeNesting) = "%s(\n%s%s %s%s)".format(
    " " * (depth + 1) * 2,
    " " * (depth + 2) * 2,
    nesting.label,
    pretty(nesting.children, depth + 2),
    " " * (depth + 1) * 2
  )

  "(%s%s)\n".format(
    nestings.map(nesting => "\n" + each(nesting)).mkString(","),
    if (nestings.isEmpty) "" else "\n" + " " * depth * 2
  )
}

// Type aliases for our typing convenience.
type Range = (String, RangeLimits)
type Ranges = List[Range]

def collectChildren(ranges: Ranges): List[RangeNesting] = ranges match {
  case Nil => Nil
  case (label, RangeLimits(first, last)) :: rest =>
    val (inside, outside) = rest.span(_._2.first < last)
    RangeNesting(label, collectChildren(inside)) :: collectChildren(outside)
}

def rangeNestings(rangeHash: Map[String, RangeLimits]) = collectChildren(
  rangeHash.toList.sortBy {
    case (_, limits) => (limits.first, -limits.last)
  }
)

// Now test this

val ranges = Map(
  "Apple" -> RangeLimits(0, 10),
  "Banana" -> RangeLimits(11, 20),
  "appleArtichoke" -> RangeLimits(0,3),
  "appleBanana" -> RangeLimits(4,7),
  "appleCranberry" -> RangeLimits(8,11),
  "appleArtichokeApricot" -> RangeLimits(0,2),
  "appleArtichokeBBQ" -> RangeLimits(3,4),
  "appleBananaApricot" -> RangeLimits(5,7),
  "appleCranberryApricot" -> RangeLimits(8,9),
  "appleArtichokeBBQ" -> RangeLimits(10,11)
)

println()
println(ranges)
println()
println(pretty(rangeNestings(ranges)))
println()

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
