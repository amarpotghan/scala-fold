# scala-fold
scala port to Gabriel's haskell-foldl-library for Applicative'ly composable folds

# Examples

## Simple folds

```scala

scala> import fold._

scala> Foldl.sum[Int].foldl(Seq(1, 2, 3))
res1: Int = 6

```

## `Foldl`s are Applicatives, so we can compose `Foldl`s using applicative style:

```scala

scala> import fold._
import fold._

scala> import scalaz._
import scalaz._

scala> type Fold[A] = Foldl[Double, A]
defined type alias Fold

scala> def mean = Apply[Fold].apply2(Foldl.sum[Double], Foldl.length[Double, Double])(_ / _)
mean: Fold[Double]

scala> mean.foldl(Seq(1.0, 2.0, 3.0))
res1: Double = 2.0

```
Note that combined mean fold traverses List only once!

## Using Applicative syntax of Scalaz

```scala

scala> import fold._
import fold._

scala>  import scalaz.syntax.apply._
import scalaz.syntax.apply._

scala> def mean = (Foldl.sum[Double] |@| Foldl.length[Double, Double]) (_ / _)
mean: fold.Foldl[Double,Double]

scala> mean.foldl(Seq(1.0, 2.0, 3.0))
res3: Double = 2.0

```
## You can also conveniently use numeric operations on `Foldl`:

```scala

scala> import fold._
import fold._

scala> def mean = Foldl.sum[Double] / Foldl.length[Double, Double]
mean: fold.Foldl[Double,Double]

scala> mean.foldl(Seq(1.0, 2.0, 3.0))
res2: Double = 2.0

```
`(/)` function uses Foldl's applicative instance, so again List is traversed only once.

## Well instead of using `mean.foldl..` syntax, you can also use syntactic extension `foldWith` on standard scala collections like:
```scala

scala> import fold._
import fold._

scala> import fold.syntax.Syntax._
import fold.syntax.Syntax._

scala> Seq(1.0, 2.0, 3.0).foldWith(Foldl.sum[Double])
res4: Double = 6.0

```

## or even like:

```scala

scala> import fold._
import fold._

scala> import fold.syntax.Syntax._
import fold.syntax.Syntax._

scala> Seq(1.0, 2.0, 3.0).foldWith(Foldl.sum[Double] / Foldl.length[Double, Double])
res4: Double = 2.0

```

Feedbacks welcome!
