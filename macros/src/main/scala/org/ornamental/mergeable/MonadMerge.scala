package org.ornamental.mergeable

import cats.data.EitherT
import cats.{Applicative, Functor, Monad}

import scala.language.implicitConversions

/**
 * Allows cross-monad composition of dependent effectful functions.
 *
 * @tparam F
 *   the parameterized family of combinable monadic types; the left type parameter is the
 *   parameterizing type, the right type parameter is the monadic computation result type
 * @tparam M
 *   the type family defining the way to merge the effectful computation contexts; the first and
 *   second type parameters are the parameterizing types of the monads being merged; the third
 *   type parameter is the parameterizing type of the monad resulting from the merge;<br/> the
 *   type family must be closed under this type merge operation; <br/> the merge must be
 *   idempotent: `∃ M[X, Y, Z] => ∃! M[Z, Y, Z]`
 */
trait MonadMerge[F[_, _], M[_, _, _]] {

  /**
   * Cross-monad generalization of monadic `flatMap`.
   */
  def flatMapMerge[X, A, Y, B, R](fa: F[X, A])(f: A => F[Y, B])(implicit m: M[X, Y, R]): F[R, B]

  /**
   * Cross-monad generalization of monadic `flatten`.
   */
  def flattenMerge[X, Y, A, R](ffa: F[X, F[Y, A]])(implicit m: M[X, Y, R]): F[R, A] =
    flatMapMerge[X, F[Y, A], Y, A, R](ffa)(identity)

  /**
   * Coerces the parameterizing type to its canonical form (exists due to merge idempotence
   * property).
   */
  def normalize[X, A, S](
      fa: F[X, A])(implicit a: Applicative[F[X, *]], m: M[X, X, S]): F[S, A] =
    flattenMerge(a.pure(fa))

  /**
   * Cross-monad generalization of monadic `flatTap`.
   */
  def flatTapMerge[X, A, Y, B, R](fa: F[X, A])(
      f: A => F[Y, B])(implicit fc: Functor[F[Y, *]], m: M[X, Y, R]): F[R, A] =
    flatMapMerge(fa)(a => fc.as(f(a), a))
}

object MonadMerge {

  @inline def apply[F[_, _], M[_, _, _]](
      implicit instance: MonadMerge[F, M]): MonadMerge[F, M] = instance

  trait Ops[F[_, _], M[_, _, _], X, A] extends Serializable {

    def self: F[X, A]

    val typeClassInstance: MonadMerge[F, M]

    def flatMapMerge[Y, B, R](f: A => F[Y, B])(implicit m: M[X, Y, R]): F[R, B] =
      typeClassInstance.flatMapMerge[X, A, Y, B, R](self)(f)

    def >>+[Y, B, R](f: A => F[Y, B])(implicit m: M[X, Y, R]): F[R, B] =
      typeClassInstance.flatMapMerge[X, A, Y, B, R](self)(f)

    def >+[Y, B, R](fb: F[Y, B])(implicit m: M[X, Y, R]): F[R, B] =
      typeClassInstance.flatMapMerge[X, A, Y, B, R](self)(_ => fb)

    def flatTapMerge[Y, B, R](
        f: A => F[Y, B])(implicit fc: Functor[F[Y, *]], m: M[X, Y, R]): F[R, A] =
      typeClassInstance.flatTapMerge[X, A, Y, B, R](self)(f)

    def normalize[S](implicit a: Applicative[F[X, *]], m: M[X, X, S]): F[S, A] =
      typeClassInstance.normalize(self)
  }

  implicit def mc[F[_]: Monad]: MonadMerge[EitherT[F, *, *], MergeEither.Aux] =
    new MonadMerge[EitherT[F, *, *], MergeEither.Aux] {

      override def flatMapMerge[X, A, Y, B, R](fa: EitherT[F, X, A])(f: A => EitherT[F, Y, B])(
          implicit m: MergeEither.Aux[X, Y, R]): EitherT[F, R, B] =
        fa.biflatMap(
          left => EitherT.leftT[F, B](m.fromA(left)),
          right => f(right).leftMap(m.fromB))
    }
}
