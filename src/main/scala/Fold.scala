package fold

import scala.math.{Ordering => DefaultOrdering}
import scalaz._

sealed class Foldl[B, A](val step: B => Foldl[B, A], val done: Unit => A) {

  def foldl[F[_]: Foldable](xs: F[B]): A =
    (implicitly[Foldable[F]].foldLeft(xs, this)((f: Foldl[B, A], b) => f.step(b))).extract

  // TODO: Remove me
  def foldl(xs: Traversable[B]): A =
    (xs.foldLeft(this)((f: Foldl[B, A], b) => f.step(b))).extract

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

  def apply[S, B, A](istep: S => B => S, init: S, done: S => A): Foldl[B, A] = {
    def construct(init1: S): Foldl[B, A] = new Foldl(b => construct(istep(init1)(b)), (x => done(init1)))
    construct(init)
  }

  def apply[B, A](istep: A => B => A, init: A): Foldl[B, A] =
    Foldl(istep, init, (identity: A => A))

  def apply[S, B, A](istep: (S, B) => S, init: S, done: S => A): Foldl[B, A] =
    Foldl(istep.curried, init, done)

  def apply[B, A](init: A)(istep: (A, B) => A): Foldl[B, A] =
    Foldl(istep.curried, init, (identity : A => A))

  def apply[B, A](s: B => Foldl[B, A], done: Unit => A): Foldl[B, A] =
    new Foldl(s, done)

  // TODO: Shall it be an another apply?
  def pure[B, A](a: A): Foldl[B, A] = {
    def con: Foldl[B, A] = Foldl(_ => con, unit => a)
    con
  }
}

trait FoldlFunctions {
  def helperFold[A](f: (A, A) => A): Foldl[A, Option[A]] =
    Foldl[A, Option[A]](None)((acc: Option[A], a: A) => acc.map(x => f(x, a)).orElse(Some(a)))

  def length[B, A](implicit a: Numeric[A]): Foldl[B, A] =
    Foldl(a.zero)((x: A, _: B) => a.plus(x, a.one))

  def sum[B](implicit N: Numeric[B]): Foldl[B, B] =
    Foldl(N.zero)((x: B, y: B) => N.plus(x, y))

  def product[B](implicit N: Numeric[B]): Foldl[B, B] =
    Foldl(N.one)((x: B, y: B) => N.times(x, y))

  def isEmpty[B]: Foldl[B, Boolean] =
    Foldl(true)((_: Boolean, _: B) => false)

  def head[A]: Foldl[A, Option[A]] =
    Foldl[A, Option[A]](None)((acc: Option[A], e: A) => acc orElse Some(e))

  def headOrElse[A](a: A): Foldl[A, A] =
    Foldl[A, A](a)((acc: A, _: A) => acc)

  def any[A](p: A => Boolean): Foldl[A, Boolean] =
    Foldl(false)((acc: Boolean, e: A) => acc || p(e))

  def all[A](p: A => Boolean): Foldl[A, Boolean] =
    Foldl(true)((acc: Boolean, e: A) => acc && p(e))

  def and[A]: Foldl[Boolean, Boolean] = Foldl(true)(_ && _)

  def or[A]: Foldl[Boolean, Boolean] = Foldl(false)(_ || _)

  def maximum[A: DefaultOrdering]: Foldl[A, Option[A]] = helperFold(implicitly[DefaultOrdering[A]].max _)

  def minimum[A: DefaultOrdering]: Foldl[A, Option[A]] = helperFold(implicitly[DefaultOrdering[A]].min _)

  def last[A]: Foldl[A, Option[A]] = helperFold((_: A, y: A) => y)

  def lastOrElse[A](a: A): Foldl[A, A] = Foldl(a)((_: A, e: A) => e)

  def reverse[A]: Foldl[A, List[A]] = Foldl(Nil: List[A])((x: List[A], y:A) => y :: x)

  def dedup[A]: Foldl[A, List[A]] = {
    val es = Set[A]()
    Foldl((es, identity: List[A] => List[A]))(
      (tup: (Set[A], List[A] => List[A]), y:A) => tup match {
        case (set: Set[A], f: (List[A] => List[A])) => if(set.contains(y)) (set, f) else (set + y, f compose (y :: _))
      }) map { case (_, f: (List[A] => List[A])) => f(List()) }
  }

}

trait FoldlInstances {

  implicit def FoldFunctorApplyApplicative[B]:
      Applicative[({type f[a] = Foldl[B, a]})#f] with
      Apply[({type f[a] = Foldl[B, a]})#f] with
      Functor[({type f[a] = Foldl[B, a]})#f] =

  new Applicative[({type f[a] = Foldl[B, a]})#f] with
      Apply[({type f[a] = Foldl[B, a]})#f] with
      Functor[({type f[a] = Foldl[B, a]})#f] {

    override def map[A, C](a: Foldl[B, A])(f: A => C) = a map f

    def ap[A, C](a: => Foldl[B, A])(f: => Foldl[B, A => C]): Foldl[B, C] = a ap f

    def point[A](a: => A): Foldl[B, A] = Foldl.pure(a)
  }

  implicit def FoldProfunctor: Profunctor[Foldl] =
    new Profunctor[Foldl] {
      override def mapfst[B, A, C](fab: fold.Foldl[B, A])(f: C => B): fold.Foldl[C, A] = fab lmap f
      override def mapsnd[B, A, C](fab: fold.Foldl[B, A])(f: A => C): fold.Foldl[B, C] = fab rmap f
    }
  implicit def FoldlMonoid[B, A: Monoid]: Monoid[Foldl[B, A]] =
    new Monoid[Foldl[B, A]]{
      def zero = Applicative[({type f[a] = Foldl[B, a]})#f].point(implicitly[Monoid[A]].zero)
      def append(a: Foldl[B, A], b: => Foldl[B, A]) = Applicative[({type f[a] = Foldl[B, a]})#f].apply2(a, b)(implicitly[Monoid[A]].append(_, _))
    }

  implicit def FoldComonad[B]: Comonad[({type f[a] = Foldl[B, a]})#f] =
    new Comonad[({type f[a] = Foldl[B, a]})#f] {
      def cobind[A, C](fa: Foldl[B,A])(f: Foldl[B,A] => C): fold.Foldl[B, C] = Foldl((b: B) => fa.step(b).duplicate, (unit: Unit) => fa).map(f)
      def copoint[A](p: fold.Foldl[B,A]): A = p.extract
      def map[A, C](fa: fold.Foldl[B,A])(f: A => C): fold.Foldl[B,C] = fa map f
    }
}
