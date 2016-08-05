package tests

import org.specs2._
import fold._
import Fold.{length => foldLength, _}
import scala.collection.{Seq}
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
            $takeWhileSpec
            $dropWhileSpec
         """

  def lengthSpec = foldLength[String, Int].foldl(Seq("1", "2", "3", "4"))  must_== 4

  def isEmptySpec1 = isEmpty[String].foldl(Seq[String]())  must_== true
  def isEmptySpec2 = isEmpty[String].foldl(Seq(""))  must_== false

  def sumSpec = sum[Int].foldl(Seq(1, 1, 1)) must_== 3
  def productSpec = product[Int].foldl(Seq(3, 3, 3)) must_== 27

  def allSpec1 = all(identity: Boolean => Boolean).foldl(Seq(true, true, true)) must_== true
  def allSpec2 = all(identity: Boolean => Boolean).foldl(Seq[Boolean]()) must_== true
  def allSpec3 = all(identity: Boolean => Boolean).foldl(Seq(true, false)) must_== false
  def allSpec4 = all((x: Int) => x % 2 == 0).foldl(Seq(2, 4, 6)) must_== true
  def allSpec5 = all((x: Int) => x % 2 == 0).foldl(Seq(2, 3, 6)) must_== false
  def allSpec6 = all((x: Int) => x % 2 == 0).foldl(Seq[Int]()) must_== true
  def allSpec7 = all[String](Function.const(true)).foldl(Seq[String]()) must_== true

  def anySpec1 = any(identity: Boolean => Boolean).foldl(Seq(true, true, false)) must_== true
  def anySpec2 = any(identity: Boolean => Boolean).foldl(Seq[Boolean]()) must_== false
  def anySpec3 = any(identity: Boolean => Boolean).foldl(Seq(false, false)) must_== false
  def anySpec4 = any((x: Int) => x % 2 == 0).foldl(Seq(1, 2, 3)) must_== true
  def anySpec5 = any((x: Int) => x % 2 == 0).foldl(Seq(1, 1, 3)) must_== false
  def anySpec6 = any((x: Int) => x % 2 == 0).foldl(Seq[Int]()) must_== false
  def anySpec7 = any[String](Function.const(true)).foldl(Seq[String]()) must_== false


  def orSpec1 = or.foldl(Seq(true, true, false)) must_== true
  def orSpec2 = or.foldl(Seq(false, false)) must_== false
  def orSpec3 = or.foldl(Seq[Boolean]()) must_== false


  def andSpec1 = and.foldl(Seq(true, true, false)) must_== false
  def andSpec2 = and.foldl(Seq(true, true)) must_== true
  def andSpec3 = and.foldl(Seq[Boolean]()) must_== true

  def headSpec1 = head.foldl(Seq("1", "2")) must_== Some("1")
  def headSpec2 = head.foldl(Seq[String]()) must_== None

  def lastSpec = last.foldl(Seq(1, 2, 3)) must_== Some(3)
  def lastOrElseSpec = lastOrElse(0).foldl(Seq[Int]()) must_== 0

  def reverseSpec = reverse.foldl(List(1, 2, 3)) must_== List(3, 2 , 1)

  def dedupSpec = dedup.foldl(Seq(1, 2, 3, 3)) must_== List(1, 2, 3)
  def dedupOnFoldableSpec = dedup.foldl(List(1, 2, 3, 3)) must_== List(1, 2, 3)

  def avgSpec = {
    val avgFold = sum[Double] / foldLength[Double, Double]
    avgFold.foldl(Seq(1.0, 2.0, 3.0, 4.0, 5.0)) must_== 3.0
  }

  def syntaxSpec1 = {
    Seq(1, 2, 3).foldWith(foldLength[Int, Int]) must_== 3

    def avg = sum[Double] / foldLength[Double, Double]
    Seq(1.0, 2.0, 3.0) foldWith avg must_== 2

  }


  def containsSpec = contains(1).foldl(Seq(1, 2, 3)) must_== true
  def containsSpec2 = contains(4).foldl(Seq(1, 2, 3)) must_== false
  def containsSpec3 = contains(4).foldl(Seq[Int]()) must_== false


  def doesNotContainSpec = doesNotContain(1).foldl(Seq(1, 2, 3)) must_== false
  def doesNotContainSpec2 = doesNotContain(4).foldl(Seq(1, 2, 3)) must_== true
  def doesNotContainSpec3 = doesNotContain(4).foldl(Seq[Int]()) must_== true

  def cProp = doesNotContain(-1331431553).foldl(Seq[Int]()) must_== ! (contains(-1331431553).foldl(Seq[Int]()))

  def foldMapSpec = {
    val x = foldMap((x: Int) => x.toString)((x: String) => x.length())
    x.foldl(Seq(10, 20, 30, 40)) must_== 8
  }

  def maximumBySpec = {
    maximumBy((x: String) => x.toInt).foldl(Seq("1", "2", "3")) must_== Some("3")
    maximumBy((x: String) => x.toInt).foldl(Seq[String]()) must_== None
  }

  def minimumBySpec = {
    minimumBy((x: String) => x.toInt).foldl(Seq("1", "2", "3")) must_== Some("1")
    minimumBy((x: String) => x.toInt).foldl(Seq[String]()) must_== None
  }

  def sumScanSpec = {
    Seq(1, 2, 3).scanWith(sum[Int]) must_== Seq(0, 1, 3, 6)
  }

  def takeWhileSpec = {
    Seq(1, 2, 3).foldWith(takeWhile[Int](x => x >= 2)) must_== Seq(1, 2)
    Seq[Int]().foldWith(takeWhile[Int](x => x >= 2)) must_== Seq()
    Seq(1, 2, 3, 4, 5).foldWith(takeWhile[Int](x => x >= 5)) must_== Seq(1, 2, 3, 4, 5)
  }


  def dropWhileSpec = {
    Seq(1, 2, 3).foldWith(dropWhile[Int](x => x == 2)) must_== Seq(3)
    Seq[Int]().foldWith(dropWhile[Int](x => x >= 2)) must_== Seq()
    Seq(1, 2, 3, 4, 5).foldWith(dropWhile[Int](x => x <= 3)) must_== Seq(4, 5)
    Seq(1, 2, 3, 4, 5).foldWith(dropWhile[Int](x => x < 0)) must_== Seq(1, 2, 3, 4, 5)
    Seq(1, 2, 1, 3, 4, 5).foldWith(dropWhile[Int](x => x < 2)) must_== Seq(2, 1, 3, 4, 5)
    Seq(1, 2, 1, 3, 4, 5).foldWith(dropWhile[Int](x => x < 0)) must_== Seq(1, 2, 1, 3, 4, 5)
  }
}
