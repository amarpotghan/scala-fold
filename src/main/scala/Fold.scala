package Foldl

import scala.collection._
import scala.math._

sealed class Foldl[B, A](val step: B => Foldl[B, A], val done: Unit => A) {
  def foldl(xs: Traversable[B]): A = (xs.foldLeft(this)((f : Foldl[B, A], b) => f.step(b))).extract
  def extract: A = done(())

  def ap[C](other: Foldl[B, A => C]): Foldl[B, C] = Foldl(b => this.step(b).ap(other.step(b)), unit => other.done(unit)(this.done(unit)) )
  def map[C](f: A => C): Foldl[B, C] = Foldl(b => step(b) map f, unit => f(done(unit)))

  def duplicate: Foldl[B, Foldl[B, A]] = this.map(Function.const(this))
}

object Foldl extends FoldlFunctions {
  def apply[S, B, A](istep: S => B => S, init: S, done: S => A): Foldl[B, A] = {
    def construct(init1: S): Foldl[B, A] = new Foldl(b => construct(istep(init1)(b)), (x => done(init1)))
    construct(init)
  }

  def apply[B, A](istep: A => B => A, init: A): Foldl[B, A] = Foldl(istep, init, (identity : A => A))

  def apply[S, B, A](istep: (S, B) => S, init: S, done: S => A): Foldl[B, A] = Foldl(istep.curried, init, done)
  def apply[B, A](init: A)(istep: (A, B) => A): Foldl[B, A] = Foldl(istep.curried, init, (identity : A => A))
  def apply[B, A](s: B => Foldl[B, A], done: Unit => A): Foldl[B, A] = new Foldl(s, done)

  def pure[B, A](a: A): Foldl[B, A] = {
    def con:Foldl[B, A] = Foldl(_ => con, unit => a)
    con
  }
  // TODO: remove me after adding instances
  def ap1[C, B, A](first: Foldl[B, A => C], second: Foldl[B, A]): Foldl[B, C] = second.ap(first)
}

trait FoldlFunctions {
  def length[B, A](implicit a: Numeric[A]): Foldl[B, A]
    = Foldl(a.zero)((x: A, _:B) => a.plus(x, a.one))

  def sum[B](implicit a: Numeric[B]): Foldl[B, B]
    = Foldl(a.zero)((x: B, y:B) => a.plus(x, y))

  def isEmpty[B]: Foldl[B, Boolean] = Foldl(true)((_: Boolean, _:B) => false)

}
