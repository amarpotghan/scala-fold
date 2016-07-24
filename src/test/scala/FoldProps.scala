package foldprops

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import fold._
import scala.collection.{Seq}
import canfold._
import scalaz._
import std.list._
import scala.math._

object FoldProps extends Properties("Fold") {

  property("contains = !(doesNotContain)") = forAll { (x: Int, xs: List[Int]) =>
    Foldl.contains(x).foldl(xs) || Foldl.doesNotContain(x).foldl(xs)
  }
}
