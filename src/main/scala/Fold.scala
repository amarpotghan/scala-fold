package fold

import scala.collection._
import scala.math._

sealed class Foldl[B, A](val step: B => Foldl[B, A], val done: Unit => A) {

  def foldl(xs: Traversable[B]): A
    = (xs.foldLeft(this)((f : Foldl[B, A], b) => f.step(b))).extract

  def extract: A = done(())

  def ap[C](other: Foldl[B, A => C]): Foldl[B, C]
    = Foldl(b => this.step(b).ap(other.step(b)), unit => other.done(unit)(this.done(unit)) )

  def map[C](f: A => C): Foldl[B, C]
    = Foldl((b: B) => step(b) map f, f compose done)

  def duplicate: Foldl[B, Foldl[B, A]]
    = this.map(Function.const(this))

  def dimap[C, D](f: C => B, g: A => D): Foldl[C, D]
    = Foldl[C, D]((c: C) => step(f(c)).dimap(f, g), g compose done)

  def lmap[C](f: C => B): Foldl[C, A] = dimap(f, identity)

  def rmap[D](g: A => D): Foldl[B, D] = dimap(identity, g)

}

object Foldl extends FoldlFunctions {

  def apply[S, B, A](istep: S => B => S, init: S, done: S => A): Foldl[B, A] = {
    def construct(init1: S): Foldl[B, A] = new Foldl(b => construct(istep(init1)(b)), (x => done(init1)))
    construct(init)
  }

  def apply[B, A](istep: A => B => A, init: A): Foldl[B, A]
    = Foldl(istep, init, (identity : A => A))

  def apply[S, B, A](istep: (S, B) => S, init: S, done: S => A): Foldl[B, A]
    = Foldl(istep.curried, init, done)

  def apply[B, A](init: A)(istep: (A, B) => A): Foldl[B, A]
    = Foldl(istep.curried, init, (identity : A => A))

  def apply[B, A](s: B => Foldl[B, A], done: Unit => A): Foldl[B, A]
    = new Foldl(s, done)

  def pure[B, A](a: A): Foldl[B, A] = {
    def con:Foldl[B, A] = Foldl(_ => con, unit => a)
    con
  }

  // TODO: remove me after adding instances
  def ap1[C, B, A](first: Foldl[B, A => C], second: Foldl[B, A]): Foldl[B, C]
    = second.ap(first)
}

trait FoldlFunctions {
  def helperFold[A](f: (A, A) => A): Foldl[A, Option[A]]
    = Foldl[A, Option[A]](None)((acc: Option[A], a: A) => acc.map(x => f(x, a)).orElse(Some(a)))

  def length[B, A](implicit a: Numeric[A]): Foldl[B, A]
    = Foldl(a.zero)((x: A, _:B) => a.plus(x, a.one))

  def sum[B](implicit a: Numeric[B]): Foldl[B, B]
    = Foldl(a.zero)((x: B, y:B) => a.plus(x, y))

  def product[B](implicit a: Numeric[B]): Foldl[B, B]
    = Foldl(a.one)((x: B, y:B) => a.times(x, y))

  def isEmpty[B]: Foldl[B, Boolean]
    = Foldl(true)((_: Boolean, _:B) => false)

  def head[A]: Foldl[A, Option[A]]
    = Foldl[A, Option[A]](None)((acc: Option[A], e: A) => acc orElse Some(e))

  def any[A](p: A => Boolean): Foldl[A, Boolean]
    = Foldl(false)((acc: Boolean, e: A) => acc || p(e))

  def all[A](p: A => Boolean): Foldl[A, Boolean]
    = Foldl(true)((acc: Boolean, e: A) => acc && p(e))

  def and[A]: Foldl[Boolean, Boolean]
    = Foldl(true)(_ && _)

  def or[A]: Foldl[Boolean, Boolean]
    = Foldl(false)(_ || _)

  def maximum[A](implicit ord: Ordering[A]): Foldl[A, Option[A]]
    = helperFold(ord.max _)

  def minimum[A](implicit ord: Ordering[A]): Foldl[A, Option[A]]
    = helperFold(ord.min _)
}
