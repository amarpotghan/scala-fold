package foldprops

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import fold._
import Fold._
import scala.collection.{Seq}
import scalaz._
import std.list._
import scala.math._

object FoldProps extends Properties("Fold") {

  property("contains = !(doesNotContain)") = forAll { (x: Int, xs: List[Int]) =>
    contains(x).foldl(xs) || doesNotContain(x).foldl(xs)
  }
}
