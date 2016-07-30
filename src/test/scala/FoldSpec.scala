package tests

import org.specs2._
import fold._
import scala.collection.{Seq}
import canfold._
import scalaz._
import std.list._
import std.string._
import scala.math._

class FoldSpecs extends Specification {
  import CanFold._
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

            $lastSpec
            $lastOrElseSpec

            $reverseSpec
            $dedupSpec
            $avgSpec

            $containsSpec
            $containsSpec2
            $containsSpec3


            $doesNotContainSpec
            $doesNotContainSpec2
            $doesNotContainSpec3

            $minimumBySpec
            $maximumBySpec

            $cProp
            $foldMapSpec
            $syntaxSpec1

            $sumScanSpec
         """

  def lengthSpec = Foldl.length[String, Int].foldl(Seq("1", "2", "3", "4"))  must_== 4

  def isEmptySpec1 = Foldl.isEmpty[String].foldl(Seq[String]())  must_== true
  def isEmptySpec2 = Foldl.isEmpty[String].foldl(Seq(""))  must_== false

  def sumSpec = Foldl.sum[Int].foldl(Seq(1, 1, 1)) must_== 3
  def productSpec = Foldl.product[Int].foldl(Seq(3, 3, 3)) must_== 27

  def allSpec1 = Foldl.all(identity: Boolean => Boolean).foldl(Seq(true, true, true)) must_== true
  def allSpec2 = Foldl.all(identity: Boolean => Boolean).foldl(Seq[Boolean]()) must_== true
  def allSpec3 = Foldl.all(identity: Boolean => Boolean).foldl(Seq(true, false)) must_== false
  def allSpec4 = Foldl.all((x: Int) => x % 2 == 0).foldl(Seq(2, 4, 6)) must_== true
  def allSpec5 = Foldl.all((x: Int) => x % 2 == 0).foldl(Seq(2, 3, 6)) must_== false
  def allSpec6 = Foldl.all((x: Int) => x % 2 == 0).foldl(Seq[Int]()) must_== true
  def allSpec7 = Foldl.all[String](Function.const(true)).foldl(Seq[String]()) must_== true

  def anySpec1 = Foldl.any(identity: Boolean => Boolean).foldl(Seq(true, true, false)) must_== true
  def anySpec2 = Foldl.any(identity: Boolean => Boolean).foldl(Seq[Boolean]()) must_== false
  def anySpec3 = Foldl.any(identity: Boolean => Boolean).foldl(Seq(false, false)) must_== false
  def anySpec4 = Foldl.any((x: Int) => x % 2 == 0).foldl(Seq(1, 2, 3)) must_== true
  def anySpec5 = Foldl.any((x: Int) => x % 2 == 0).foldl(Seq(1, 1, 3)) must_== false
  def anySpec6 = Foldl.any((x: Int) => x % 2 == 0).foldl(Seq[Int]()) must_== false
  def anySpec7 = Foldl.any[String](Function.const(true)).foldl(Seq[String]()) must_== false


  def orSpec1 = Foldl.or.foldl(Seq(true, true, false)) must_== true
  def orSpec2 = Foldl.or.foldl(Seq(false, false)) must_== false
  def orSpec3 = Foldl.or.foldl(Seq[Boolean]()) must_== false


  def andSpec1 = Foldl.and.foldl(Seq(true, true, false)) must_== false
  def andSpec2 = Foldl.and.foldl(Seq(true, true)) must_== true
  def andSpec3 = Foldl.and.foldl(Seq[Boolean]()) must_== true

  def headSpec1 = Foldl.head.foldl(Seq("1", "2")) must_== Some("1")
  def headSpec2 = Foldl.head.foldl(Seq[String]()) must_== None

  def lastSpec = Foldl.last.foldl(Seq(1, 2, 3)) must_== Some(3)
  def lastOrElseSpec = Foldl.lastOrElse(0).foldl(Seq[Int]()) must_== 0

  def reverseSpec = Foldl.reverse.foldl(List(1, 2, 3)) must_== List(3, 2 , 1)

  def dedupSpec = Foldl.dedup.foldl(Seq(1, 2, 3, 3)) must_== List(1, 2, 3)
  def dedupOnFoldableSpec = Foldl.dedup.foldl(List(1, 2, 3, 3)) must_== List(1, 2, 3)

  def avgSpec = {
    val avgFold = Foldl.sum[Double] / Foldl.length[Double, Double]
    avgFold.foldl(Seq(1.0, 2.0, 3.0, 4.0, 5.0)) must_== 3.0
  }

  def syntaxSpec1 = {
    import fold.syntax.Syntax._

    Seq(1, 2, 3).foldWith(Foldl.length[Int, Int]) must_== 3

    def avg = Foldl.sum[Double] / Foldl.length[Double, Double]
    Seq(1.0, 2.0, 3.0) foldWith avg must_== 2

  }


  def containsSpec = Foldl.contains(1).foldl(Seq(1, 2, 3)) must_== true
  def containsSpec2 = Foldl.contains(4).foldl(Seq(1, 2, 3)) must_== false
  def containsSpec3 = Foldl.contains(4).foldl(Seq[Int]()) must_== false


  def doesNotContainSpec = Foldl.doesNotContain(1).foldl(Seq(1, 2, 3)) must_== false
  def doesNotContainSpec2 = Foldl.doesNotContain(4).foldl(Seq(1, 2, 3)) must_== true
  def doesNotContainSpec3 = Foldl.doesNotContain(4).foldl(Seq[Int]()) must_== true

  def cProp = Foldl.doesNotContain(-1331431553).foldl(Seq[Int]()) must_== ! (Foldl.contains(-1331431553).foldl(Seq[Int]()))

  def foldMapSpec = {
    val x = Foldl.foldMap((x: Int) => x.toString)((x: String) => x.length())
    x.foldl(Seq(10, 20, 30, 40)) must_== 8
  }

  def maximumBySpec = {
    Foldl.maximumBy((x: String) => x.toInt).foldl(Seq("1", "2", "3")) must_== Some("3")
    Foldl.maximumBy((x: String) => x.toInt).foldl(Seq[String]()) must_== None
  }

  def minimumBySpec = {
    Foldl.minimumBy((x: String) => x.toInt).foldl(Seq("1", "2", "3")) must_== Some("1")
    Foldl.minimumBy((x: String) => x.toInt).foldl(Seq[String]()) must_== None
  }

  def sumScanSpec = {
    import fold.syntax.Syntax._
    Seq(1, 2, 3).scanWith(Foldl.sum[Int]) must_== Seq(0, 1, 3, 6)
  }
}
