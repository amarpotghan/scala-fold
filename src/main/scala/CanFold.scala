package fold

import Foldl._
import Extensions._

import scalaz.{Foldable => SFoldable}
import scala.collection.{ GenTraversableLike => G }
import scala.collection._

trait CanFold[F[_], B] {
  def fold[A](fa: F[B])(fo: Foldl[B, A]): A
  def scan[A](fa: F[B])(fo: Foldl[B, A]): Seq[A]
}

object CanFold extends CanFoldInstances

trait CanFoldInstances {

  implicit def StdIterable[F[B] <: GenTraversableLike[B, F[B]], B]: CanFold[F, B] = new CanFold[F, B] {
    def fold[A](xs: F[B])(fo: Foldl[B, A]): A =
      xs.foldWith(fo)

    def scan[A](xs: F[B])(fo: Foldl[B, A]): Seq[A] =
      xs.scanWith(fo)
  }
}
