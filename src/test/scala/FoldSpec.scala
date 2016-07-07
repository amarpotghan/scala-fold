package tests

import org.specs2._
import fold._

class FoldSpecs extends Specification {
  def is = sequential ^ s2"""Fold Specs
            $lengthSpec

            $isEmptySpec1
            $isEmptySpec2

            $sumSpec

            $andSpec1
            $andSpec2
            $andSpec3

            $orSpec1
            $orSpec2
            $orSpec3
         """

  def lengthSpec = Foldl.length[String, Int].foldl(Seq("1", "2", "3", "4"))  must_== 4

  def isEmptySpec1 = Foldl.isEmpty[String].foldl(Seq[String]())  must_== true
  def isEmptySpec2 = Foldl.isEmpty[String].foldl(Seq[String](""))  must_== false

  def sumSpec = Foldl.sum[Int].foldl(Seq(1, 1, 1)) must_== 3

  def andSpec1 = Foldl.and.foldl(Seq(true, true, true)) must_== true
  def andSpec2 = Foldl.and.foldl(Seq[Boolean]()) must_== true
  def andSpec3 = Foldl.and.foldl(Seq[Boolean](true, false)) must_== false

  def orSpec1 = Foldl.or.foldl(Seq(true, true, false)) must_== true
  def orSpec2 = Foldl.or.foldl(Seq[Boolean]()) must_== false
  def orSpec3 = Foldl.or.foldl(Seq[Boolean](false, false)) must_== false


}
