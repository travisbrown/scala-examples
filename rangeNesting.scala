/*
# rangeNestings(rangeHash) => nesting
#
# See the block comment at the end of this file for an example run.
#
*/

case class RangeNesting(label: String, children: List[RangeNesting])
case class RangeLimits(first: Int, last: Int)

def rangeNestings(rangeHash: Map[String,RangeLimits]) = {
  /* given a set of ranges, how are they nested?
  #
  # rangeHash['label'] = { first: ..., last: ... }
  */
  def buildRangeNestings(nestings: Map[String, List[String]], pos: Int, sequence: List[String]): List[RangeNesting] =
    sequence.filter(nestings(_).length == pos).map(
      (root) => {
        RangeNesting(
          root, 
          buildRangeNestings(
            nestings, 
            pos+1, 
            (sequence.filter(
              (target) => {
                nestings(target).length > pos && (
                  nestings(root).isEmpty ||
                  nestings(root).length <= nestings(target).length && 
                    (nestings(root), nestings(target)).zipped.forall(_==_)
                )
              }
            ))
          )
        )
      }
    )

  def truncateStack(stack: List[String], label: String, ranges: Map[String,RangeLimits]): List[String] =
    if(stack.isEmpty || ranges(stack.head).last < ranges(label).first) {
      List()
    }
    else {
      stack.head :: truncateStack(stack.tail, label, ranges)
    }


  // first, sort the range labels by their 'first' value and reverse by their 'last' if they have the same 'first'  

  val sortedLabels = rangeHash.keys.toList.sortWith(
    (a,b) => {
      if(rangeHash(a).first == rangeHash(b).first) {
        (rangeHash(b).last  < rangeHash(a).last)
      }
      else {
        (rangeHash(a).first < rangeHash(b).first)
      }
    }
  )

  var nestings = Map.empty[String, List[String]]

  // we know that the container of a particular label comes before the label
  // we want to record *all* of the ranges that contain a particular label
  var labelStack: List[String] = List()

  sortedLabels.foreach(
    (label) => {
      // we want to go down the labelStack until we find something that we're not in, and then truncate the list before appending ourselves
      labelStack = truncateStack(labelStack, label, rangeHash)
      labelStack = labelStack :+ label
      nestings += (label -> labelStack)
    }
  )

  buildRangeNestings(nestings, 1, sortedLabels)
}
  

// Now test this

var ranges = Map(
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
println(rangeNestings(ranges))
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
