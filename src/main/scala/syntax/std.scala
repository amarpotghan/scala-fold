package fold
package syntax

import fold._
import scala.collection.generic.{ CanBuildFrom, IsTraversableLike}
import scala.collection.{ GenTraversable, GenTraversableLike }

class FoldableCollection[A, R <% GenTraversableLike[_,R]](val r: GenTraversableLike[A,R]) {
  def foldWith[S](fo: Foldl[A, S]): S = {
    r.foldLeft(fo)(_ step _).extract
  }
}
object Syntax {
  implicit def toFold[A, R <% GenTraversableLike[_,R]](r: R)(implicit fr: IsTraversableLike[R]): FoldableCollection[fr.A, R] =
    new FoldableCollection(fr conversion r)
}