# scala-fold
Composable folds in Scala

# Introduction

This library defines the `Foldl` data type (a left fold) which can be combined in the applicative style such that resulting
fold requires only one traversal over the given structure.

Library comes with common folds. You can always define your own fold by providing a step function and initial value.

Library also comes with an extension method on scala's standard collections `foldWith`. You can use that on standard scala collections as follows,

```scala
import fold._
import Fold._

List(1, 2, 3).foldWith(sum[Int])

```

# Examples

## Simple sum of integers

```scala

scala> import fold.Foldl._
import fold.Foldl._

scala> import fold.syntax.Syntax._
import fold.syntax.Syntax._

scala> Seq(1, 2, 3).foldWith(sum[Int])
res1: Int = 6

```

## `Foldl`s are Applicatives, so we can compose `Foldl`s using applicative style:

```scala

scala> import fold.Foldl._
import fold.Foldl._

scala> import fold.syntax.Syntax._
import fold.syntax.Syntax._

scala> import scalaz._
import scalaz._

scala> type Fold[A] = Foldl[Double, A]
defined type alias Fold

scala> def mean = Apply[Fold].apply2(sum[Double], length[Double, Double])(_ / _)
mean: Fold[Double]

scala> Seq(1.0, 2.0, 3.0).foldWith(mean)
res1: Double = 2.0

```
Note that combined mean fold traverses List only once!

## Using Applicative syntax of Scalaz

```scala

scala> import fold._
scala> import Fold._

scala>  import scalaz.syntax.apply._
import scalaz.syntax.apply._

scala> def mean = (sum[Double] |@| length[Double, Double]) (_ / _)
mean: fold.Foldl[Double,Double]

scala> Seq(1.0, 2.0, 3.0).foldWith(mean)
res3: Double = 2.0

```
## You can also conveniently use numeric operations on `Foldl`:

```scala

scala> import fold.Foldl._
import fold.Foldl._

scala> import fold.syntax.Syntax._
import fold.syntax.Syntax._

scala> def mean = sum[Double] / length[Double, Double]
mean: fold.Foldl[Double,Double]

scala> Seq(1.0, 2.0, 3.0).foldWith(mean)
res2: Double = 2.0

```
`(/)` function uses Foldl's applicative instance, so again List is traversed only once.


# Credits

* Gabriel Gonzalez(https://github.com/Gabriel439)'s foldl library(https://hackage.haskell.org/package/foldl)
* Phil Freeman's purescript-folds(https://github.com/paf31/purescript-folds)
* Max Rabkin’s "Beautiful folding"(http://squing.blogspot.sg/2008/11/beautiful-folding.html)
* Conal Elliott's blogpost(http://conal.net/blog/posts/another-lovely-example-of-type-class-morphisms)


Feedbacks welcome!
