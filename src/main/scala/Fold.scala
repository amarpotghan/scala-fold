package Foldl

sealed class Foldl[B, A](val step: B => Foldl[B, A], done: Unit => A) {
  def foldl(xs: List[B]): A = (xs.foldLeft(this)((f : Foldl[B, A], b) => f.step(b))).extract
  def extract: A = ???
}

object Foldl {
  def apply[S, B, A](istep: S => B => S, init: S, done: S => A): Foldl[B, A] = {
    def construct(init1: S): Foldl[B, A] = new Foldl(b => construct(istep(init1)(b)), (x => done(init1)))
    construct(init)
  }

  def apply[B, A](istep: A => B => A, init: A): Foldl[B, A] = Foldl(istep, init, (identity : A => A))
  def apply[S, B, A](istep: (S, B) => S, init: S, done: S => A): Foldl[B, A] = Foldl(istep.curried, init, done)
  def apply[B, A](istep: (A, B) => A, init: A): Foldl[B, A] = Foldl(istep.curried, init, (identity : A => A))

 }
