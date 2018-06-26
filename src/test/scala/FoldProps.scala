package foldprops

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import fold._
import Fold._

object FoldProps extends Properties("Fold") {

  property("contains = !(doesNotContain)") = forAll { (x: Int, xs: List[Int]) =>
    contains(x).foldl(xs) || doesNotContain(x).foldl(xs)
  }
}
