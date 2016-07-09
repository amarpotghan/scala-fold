package tests

import org.specs2._
import fold._
import scala.collection._

class FoldSpecs extends Specification {
  def is = sequential ^ s2"""Fold Specs
            $lengthSpec

            $isEmptySpec1
            $isEmptySpec2

            $sumSpec
            $productSpec

            $allSpec1
            $allSpec2
            $allSpec3
            $allSpec4
            $allSpec5
            $allSpec6
            $allSpec7

            $anySpec1
            $anySpec2
            $anySpec3
            $anySpec4
            $anySpec5
            $anySpec6
            $anySpec7

            $orSpec1
            $orSpec2
            $orSpec3

            $andSpec1
            $andSpec2
            $andSpec3


            $headSpec1
            $headSpec2

            $headOrElseSpec1
            $headOrElseSpec2

            $lastSpec
            $lastOrElseSpec

            $reverseSpec
            $dedupSpec
         """

  def lengthSpec = Foldl.length[String, Int].foldl(Seq("1", "2", "3", "4"))  must_== 4

  def isEmptySpec1 = Foldl.isEmpty[String].foldl(Seq[String]())  must_== true
  def isEmptySpec2 = Foldl.isEmpty[String].foldl(Seq[String](""))  must_== false

  def sumSpec = Foldl.sum[Int].foldl(Seq(1, 1, 1)) must_== 3
  def productSpec = Foldl.product[Int].foldl(Seq(3, 3, 3)) must_== 27

  def allSpec1 = Foldl.all(identity: Boolean => Boolean).foldl(Seq(true, true, true)) must_== true
  def allSpec2 = Foldl.all(identity: Boolean => Boolean).foldl(Seq[Boolean]()) must_== true
  def allSpec3 = Foldl.all(identity: Boolean => Boolean).foldl(Seq[Boolean](true, false)) must_== false
  def allSpec4 = Foldl.all((x: Int) => x % 2 == 0).foldl(Seq(2, 4, 6)) must_== true
  def allSpec5 = Foldl.all((x: Int) => x % 2 == 0).foldl(Seq(2, 3, 6)) must_== false
  def allSpec6 = Foldl.all((x: Int) => x % 2 == 0).foldl(Seq()) must_== true
  def allSpec7 = Foldl.all(Function.const(true)).foldl(Seq()) must_== true

  def anySpec1 = Foldl.any(identity: Boolean => Boolean).foldl(Seq(true, true, false)) must_== true
  def anySpec2 = Foldl.any(identity: Boolean => Boolean).foldl(Seq[Boolean]()) must_== false
  def anySpec3 = Foldl.any(identity: Boolean => Boolean).foldl(Seq[Boolean](false, false)) must_== false
  def anySpec4 = Foldl.any((x: Int) => x % 2 == 0).foldl(Seq(1, 2, 3)) must_== true
  def anySpec5 = Foldl.any((x: Int) => x % 2 == 0).foldl(Seq(1, 1, 3)) must_== false
  def anySpec6 = Foldl.any((x: Int) => x % 2 == 0).foldl(Seq()) must_== false
  def anySpec7 = Foldl.any(Function.const(true)).foldl(Seq()) must_== false


  def orSpec1 = Foldl.or.foldl(Seq(true, true, false)) must_== true
  def orSpec2 = Foldl.or.foldl(Seq(false, false)) must_== false
  def orSpec3 = Foldl.or.foldl(Seq[Boolean]()) must_== false


  def andSpec1 = Foldl.and.foldl(Seq(true, true, false)) must_== false
  def andSpec2 = Foldl.and.foldl(Seq(true, true)) must_== true
  def andSpec3 = Foldl.and.foldl(Seq[Boolean]()) must_== true

  def headSpec1 = Foldl.head.foldl(Seq[String]("1", "2")) must_== Some("1")
  def headSpec2 = Foldl.head.foldl(Seq[String]()) must_== None

  def headOrElseSpec1 = Foldl.headOrElse("head").foldl(Seq[String]()) must_== "head"
  def headOrElseSpec2 = Foldl.headOrElse("head").foldl(Seq[String]("actualHead", "somethingElse")) must_== "actualHead"

  def lastSpec = Foldl.last.foldl(Seq(1, 2, 3)) must_== Some(3)
  def lastOrElseSpec = Foldl.lastOrElse(0).foldl(Seq[Int]()) must_== 0

  def reverseSpec = Foldl.reverse.foldl(List[Int](1, 2, 3)) must_== List(3, 2 , 1)

  def dedupSpec = Foldl.dedup.foldl(Seq[Int](1, 2, 3, 3)) must_== List(1, 2, 3)

}
