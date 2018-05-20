package quickcheck

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{const, oneOf}
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
      element <- Arbitrary.arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(element, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

//  lazy val genIntList      = Gen.containerOf[List,Int](Arbitrary.arbitrary[Int])


  def isSorted(h: H, lastValue: A): Boolean = {
    if (isEmpty(h)) true else {
      val min = findMin(h)

      if (min < lastValue) false else isSorted(deleteMin(h), min)
    }
  }

  def getAllValue(h: H, c: List[Int]): List[Int] = {
    if (isEmpty(h)) c.reverse
    else {
      val min = findMin(h)
      getAllValue(deleteMin(h), min :: c)
    }
  }

  def insertContinuously(h: H, c: List[Int]): H = c match {
    case Nil => h
    case x :: xs => insertContinuously(insert(x, h), xs)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert then delete") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("insert continuously") = forAll { l: List[Int] =>
//    val heap = l foldLeft(empty) { (h: this.H, value: Int) => insert(value, h) }
    val heap = insertContinuously(empty, l)

    val sortedList = l.sorted
    println(sortedList)
    getAllValue(heap, List()) == sortedList
  }
  property("heap should be sorted") = forAll { (h:H) =>
    isSorted(deleteMin(h), Int.MinValue)
  }

  property("melding two heaps, min is correct") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) & isEmpty(h2)) isEmpty(meld(h1, h2)) == true
    else {
      val min1 = if (isEmpty(h1)) Int.MinValue else findMin(h1)
      val min2 = if (isEmpty(h2)) Int.MinValue else findMin(h2)
      val min = findMin(meld(h1, h2))

      if (min1 == min2) min == min1
      else if (min1 > min2) min == min2
      else min == min1
    }
  }

  property("melding two heaps, the new heap is sorted") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    isSorted(deleteMin(h), Int.MinValue)
  }

  property("melding two heaps, gen1") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)

    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
}

