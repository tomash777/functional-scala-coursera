package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    isEmpty <- arbitrary[Boolean]
    heap <- if (isEmpty) emptyHeaps else nonEmptyHeaps} yield heap
  def emptyHeaps = const(empty)
  def nonEmptyHeaps = for {
    head <- arbitrary[Int]
    tail <- genHeap
  } yield insert(head, tail)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
 

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("gen2") = forAll { (a: A, b: A) =>{
    val c = a min b
    findMin(insert(a, insert(b, empty))) == c
  }
  }
  
  property("gen3") = forAll { (a: A, b: A) =>
    val c = a max b
    findMin(deleteMin(insert(a, insert(b, empty)))) == c
  }
  
  property("gen4") = forAll { (h1: H) => {
    
      def orderAcc (h: H): List[Int] = if (isEmpty(h)) Nil 
      else { val m = findMin(h); m :: orderAcc(deleteMin(h))}        
      
      def isSorted(xs: List[Int]): Boolean = xs match {
        case x::y::ys => (x<=y) && isSorted(y::ys)
        case _ => true
      }
      isSorted(orderAcc(h1))
   
  }
  }
  
  
  
  property("gen2bis") = forAll { (h: H) =>
    findMin(deleteMin(h)) >= findMin(h)
  }
  
  property("gen5") = forAll { (h1: H, h2: H) =>
    (findMin(meld(h1, h2)) == findMin(h1)) || (findMin(meld(h1, h2)) == findMin(h2))
  }
  
  

}
