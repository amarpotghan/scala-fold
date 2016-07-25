package fold

import canfold._
import scala.math.{Ordering => DefaultOrdering}
import scalaz._

sealed class Foldl[B, A](val step: B => Foldl[B, A], val done: Unit => A) {

  def foldl[G[_]](xs: G[B])(implicit Can: CanFold[G, B]): A =
    Can.fold(xs)(this)

  def extract: A = done(())

  def ap[C](other: Foldl[B, A => C]): Foldl[B, C] =
    Foldl(b => this.step(b).ap(other.step(b)), unit => other.done(unit)(this.done(unit)) )

  def map[C](f: A => C): Foldl[B, C] = Foldl((b: B) => step(b) map f, f compose done)

  def map2[C, D](other: Foldl[B, C])(f: (A, C) => D): Foldl[B, D] = other.ap(map(f.curried))

  def duplicate: Foldl[B, Foldl[B, A]] = this.map(Function.const(this))

  def dimap[C, D](f: C => B, g: A => D): Foldl[C, D] =
    Foldl[C, D]((c: C) => step(f(c)).dimap(f, g), g compose done)

  def lmap[C](f: C => B): Foldl[C, A] = dimap(f, identity)

  def rmap[D](g: A => D): Foldl[B, D] = dimap(identity, g)

}

object Foldl extends FoldlFunctions with FoldlInstances {

  def create[S, B, A](istep: S => B => S, init: S, done: S => A): Foldl[B, A] = {
    def construct(init1: S): Foldl[B, A] = new Foldl(b => construct(istep(init1)(b)), (x => done(init1)))
    construct(init)
  }

  def apply[B, A](istep: A => B => A, init: A): Foldl[B, A] =
    create(istep, init, (identity: A => A))

  def apply[S, B, A](istep: (S, B) => S, init: S, done: S => A): Foldl[B, A] =
    create(istep.curried, init, done)

  def createWith[B, A](init: A)(istep: (A, B) => A): Foldl[B, A] =
    create(istep.curried, init, (identity : A => A))

  def apply[B, A](s: B => Foldl[B, A], done: Unit => A): Foldl[B, A] =
    new Foldl(s, done)

  def pure[B, A](a: A): Foldl[B, A] = {
    def con: Foldl[B, A] = Foldl(_ => con, unit => a)
    con
  }
}

trait FoldlFunctions {
  import Foldl._

  def helperFold[A](f: (A, A) => A): Foldl[A, Option[A]] =
    createWith[A, Option[A]](None)((acc: Option[A], a: A) => acc.map(x => f(x, a)).orElse(Some(a)))

  def length[B, A](implicit a: Numeric[A]): Foldl[B, A] =
    createWith(a.zero)((x: A, _: B) => a.plus(x, a.one))

  def length[B]: Foldl[B, Int] =
    createWith(0)((x: Int, _: B) => x + 1)

  def sum[B](implicit N: Numeric[B]): Foldl[B, B] =
    createWith(N.zero)((x: B, y: B) => N.plus(x, y))

  def product[B](implicit N: Numeric[B]): Foldl[B, B] =
    createWith(N.one)((x: B, y: B) => N.times(x, y))

  def isEmpty[B]: Foldl[B, Boolean] =
    createWith(true)((_: Boolean, _: B) => false)

  def head[A]: Foldl[A, Option[A]] =
    createWith[A, Option[A]](None)((acc: Option[A], e: A) => acc orElse Some(e))

  def any[A](p: A => Boolean): Foldl[A, Boolean] =
    createWith(false)((acc: Boolean, e: A) => acc || p(e))

  def all[A](p: A => Boolean): Foldl[A, Boolean] =
    createWith(true)((acc: Boolean, e: A) => acc && p(e))

  def and[A]: Foldl[Boolean, Boolean] = createWith(true)(_ && _)

  def or[A]: Foldl[Boolean, Boolean] = createWith(false)(_ || _)

  def maximum[A: DefaultOrdering]: Foldl[A, Option[A]] = helperFold(implicitly[DefaultOrdering[A]].max _)

  def minimum[A: DefaultOrdering]: Foldl[A, Option[A]] = helperFold(implicitly[DefaultOrdering[A]].min _)

  def last[A]: Foldl[A, Option[A]] = helperFold((_: A, y: A) => y)

  def lastOrElse[A](a: A): Foldl[A, A] = createWith(a)((_: A, e: A) => e)

  def reverse[A]: Foldl[A, List[A]] = createWith(Nil: List[A])((x: List[A], y:A) => y :: x)

  def contains[A](e: A): Foldl[A, Boolean] = any(_.equals(e))

  def doesNotContain[A](e: A): Foldl[A, Boolean] = all(!_.equals(e))

  def dedup[A]: Foldl[A, List[A]] = {
    val es = Set[A]()
    createWith((es, identity: List[A] => List[A]))(
      (tup: (Set[A], List[A] => List[A]), y:A) => tup match {
        case (set: Set[A], f: (List[A] => List[A])) => if(set.contains(y)) (set, f) else (set + y, f compose (y :: _))
      }) map { case (_, f: (List[A] => List[A])) => f(List()) }
  }

}

trait FoldlInstances {

  implicit class FoldlNumeric[B, A: Fractional](x: Foldl[B, A]){
    def -(y: fold.Foldl[B,A]): fold.Foldl[B,A] =
      x.map2(y)(implicitly[Numeric[A]].minus)

    def negate: fold.Foldl[B,A] =
      x.map(implicitly[Numeric[A]].negate)

    def +(y: fold.Foldl[B,A]): fold.Foldl[B,A] =
      x.map2(y)(implicitly[Numeric[A]].plus)

    def *(y: fold.Foldl[B,A]): fold.Foldl[B,A] =
      x.map2(y)(implicitly[Numeric[A]].times)

    def /(y: fold.Foldl[B,A]): fold.Foldl[B,A] =
      x.map2(y)(implicitly[Fractional[A]].div)

    def compare(y: fold.Foldl[B,A]): Int =
      x.map2(y)(implicitly[Numeric[A]].compare).extract
  }

  implicit def FoldFunctorApplyApplicative[B] =
    new Applicative[({type f[a] = Foldl[B, a]})#f] with Apply[({type f[a] = Foldl[B, a]})#f] with Functor[({type f[a] = Foldl[B, a]})#f] {
      override def map[A, C](a: Foldl[B, A])(f: A => C) =
        a map f

      def ap[A, C](a: => Foldl[B, A])(f: => Foldl[B, A => C]): Foldl[B, C] =
        a ap f

      def point[A](a: => A): Foldl[B, A] =
        Foldl.pure(a)
  }

  implicit def FoldProfunctor =
    new Profunctor[Foldl] {
      override def mapfst[B, A, C](fab: fold.Foldl[B, A])(f: C => B): fold.Foldl[C, A] =
        fab lmap f

      override def mapsnd[B, A, C](fab: fold.Foldl[B, A])(f: A => C): fold.Foldl[B, C] =
        fab rmap f
    }

  implicit def FoldlMonoid[B, A: Monoid] =
    new Monoid[Foldl[B, A]]{
      def zero =
        Applicative[({type f[a] = Foldl[B, a]})#f].point(implicitly[Monoid[A]].zero)

      def append(a: Foldl[B, A], b: => Foldl[B, A]) =
        Applicative[({type f[a] = Foldl[B, a]})#f].apply2(a, b)(implicitly[Monoid[A]].append(_, _))
    }

  implicit def FoldComonad[B] =
    new Comonad[({type f[a] = Foldl[B, a]})#f] {
      def cobind[A, C](fa: Foldl[B,A])(f: Foldl[B,A] => C): fold.Foldl[B, C] =
        Foldl((b: B) => fa.step(b).duplicate, (unit: Unit) => fa).map(f)

      def copoint[A](p: fold.Foldl[B,A]): A =
        p.extract

      def map[A, C](fa: fold.Foldl[B,A])(f: A => C): fold.Foldl[B,C] =
        fa map f

    }

}
