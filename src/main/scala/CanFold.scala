package canfold

import fold._
import scalaz.{Foldable => SFoldable}
import scala.collection.{ GenTraversableLike => G }
import scala.collection._

trait CanFold[F[_], B] {
  def fold[A](fa: F[B])(fo: Foldl[B, A]): A
}

object CanFold extends CanFoldInstances

trait CanFoldInstances {

  // implicit def ScalazFoldable[F[_]: SFoldable, B]: CanFold[F, B] = new CanFold[F, B] {
  //   def fold[A](xs: F[B])(fo: Foldl[B, A]): A =
  //     implicitly[SFoldable[F]].foldLeft(xs, fo)((f: Foldl[B, A], b) => f.step(b)).extract
  // }

  implicit def StdIterable[F[B] <: Traversable[B], B]: CanFold[F, B] = new CanFold[F, B] {
    def fold[A](xs: F[B])(fo: Foldl[B, A]): A =
      xs.foldLeft(fo)(_ step _).extract

  }
}
