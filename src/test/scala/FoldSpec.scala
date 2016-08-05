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
            $isEmptySpec
            $sumSpec
            $productSpec
            $allSpec
            $anySpec
            $orSpec
            $andSpec
            $headSpec
            $lastSpec
            $lastOrElseSpec
            $reverseSpec
            $dedupSpec
            $avgSpec
            $containsSpec
            $doesNotContainSpec
            $minimumBySpec
            $maximumBySpec
            $cProp
            $foldMapSpec
            $syntaxSpec
            $sumScanSpec
            $takeWhileSpec
            $dropWhileSpec
         """

  def lengthSpec = foldLength[String, Int].foldl(Seq("1", "2", "3", "4"))  must_== 4

  def isEmptySpec = {
    isEmpty[String].foldl(Seq[String]())  must_== true
    isEmpty[String].foldl(Seq(""))  must_== false
  }

  def sumSpec = sum[Int].foldl(Seq(1, 1, 1)) must_== 3
  def productSpec = product[Int].foldl(Seq(3, 3, 3)) must_== 27

  def allSpec = {
    all(identity: Boolean => Boolean).foldl(Seq(true, true, true)) must_== true
    all(identity: Boolean => Boolean).foldl(Seq[Boolean]()) must_== true
    all(identity: Boolean => Boolean).foldl(Seq(true, false)) must_== false
    all((x: Int) => x % 2 == 0).foldl(Seq(2, 4, 6)) must_== true
    all((x: Int) => x % 2 == 0).foldl(Seq(2, 3, 6)) must_== false
    all((x: Int) => x % 2 == 0).foldl(Seq[Int]()) must_== true
    all[String](Function.const(true)).foldl(Seq[String]()) must_== true
  }

  def anySpec = {
    any(identity: Boolean => Boolean).foldl(Seq(true, true, false)) must_== true
    any(identity: Boolean => Boolean).foldl(Seq[Boolean]()) must_== false
    any(identity: Boolean => Boolean).foldl(Seq(false, false)) must_== false
    any((x: Int) => x % 2 == 0).foldl(Seq(1, 2, 3)) must_== true
    any((x: Int) => x % 2 == 0).foldl(Seq(1, 1, 3)) must_== false
    any((x: Int) => x % 2 == 0).foldl(Seq[Int]()) must_== false
    any[String](Function.const(true)).foldl(Seq[String]()) must_== false
  }

  def orSpec = {
    or.foldl(Seq(true, true, false)) must_== true
    or.foldl(Seq(false, false)) must_== false
    or.foldl(Seq[Boolean]()) must_== false
  }

  def andSpec = {
    and.foldl(Seq(true, true, false)) must_== false
    and.foldl(Seq(true, true)) must_== true
    and.foldl(Seq[Boolean]()) must_== true
  }

  def headSpec = {
    head.foldl(Seq("1", "2")) must_== Some("1")
    head.foldl(Seq[String]()) must_== None
  }

  def lastSpec = last.foldl(Seq(1, 2, 3)) must_== Some(3)
  def lastOrElseSpec = lastOrElse(0).foldl(Seq[Int]()) must_== 0
  def reverseSpec = reverse.foldl(List(1, 2, 3)) must_== List(3, 2 , 1)
  def dedupSpec = dedup.foldl(Seq(1, 2, 3, 3)) must_== List(1, 2, 3)
  def dedupOnFoldableSpec = dedup.foldl(List(1, 2, 3, 3)) must_== List(1, 2, 3)

  def avgSpec = {
    val avgFold = sum[Double] / foldLength[Double, Double]
    avgFold.foldl(Seq(1.0, 2.0, 3.0, 4.0, 5.0)) must_== 3.0
  }

  def syntaxSpec = {
    Seq(1, 2, 3).foldWith(foldLength[Int, Int]) must_== 3

    def avg = sum[Double] / foldLength[Double, Double]
    Seq(1.0, 2.0, 3.0) foldWith avg must_== 2

  }

  def containsSpec = {
    contains(1).foldl(Seq(1, 2, 3)) must_== true
    contains(4).foldl(Seq(1, 2, 3)) must_== false
    contains(4).foldl(Seq[Int]()) must_== false
  }

  def doesNotContainSpec = {
    doesNotContain(1).foldl(Seq(1, 2, 3)) must_== false
    doesNotContain(4).foldl(Seq(1, 2, 3)) must_== true
    doesNotContain(4).foldl(Seq[Int]()) must_== true
  }

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
