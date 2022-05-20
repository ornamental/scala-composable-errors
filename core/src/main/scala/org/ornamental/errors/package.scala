package org.ornamental

import cats.{Functor, Monad}
import cats.data.EitherT
import org.ornamental.errors.EitherTOps.{
  PureNoErrorPartiallyApplied,
  RaiseErrorPartiallyApplied
}
import shapeless.{:+:, <:!<, =:!=, CNil, Coproduct, Inl}

import scala.language.implicitConversions

package object errors {

  /**
   * Introduced to give a better compile-time error message than provided by the underlying
   * `shapeless.=:!=`.
   */
  sealed trait !=:=[A, B]

  object !=:= {

    implicit def summon[A, B](implicit e: A =:!= B): A !=:= B = new !=:=[A, B] {}
  }

  object syntax {

    /**
     * Alias for Coproduct final element.
     */
    type |+:[A, B] = A :+: B :+: CNil

    type Only[A] = A :+: CNil

    type NoErrors = CNil

    /**
     * Alias for `pure` of `EitherT[F, CNil, *]` applicative.
     */
    def pureNoError[F[_]]: PureNoErrorPartiallyApplied[F] = new PureNoErrorPartiallyApplied[F]

    /**
     * For given error instance of type `E`, lifts the error into `EitherT[F, E :+: CNil, *]`
     * applicative.
     */
    def raiseError[F[_]]: RaiseErrorPartiallyApplied[F] = new RaiseErrorPartiallyApplied[F]

    /**
     * Converts `EitherT[F, E, A]` to `EitherT[F, E :+: CNil, A]`. `E` must not be a `Coproduct`
     * type.
     */
    def leftToCoproduct[F[_]: Functor, E, A](either: EitherT[F, E, A])(
        implicit notCoproduct: E <:!< Coproduct): EitherT[F, E :+: CNil, A] =
      either.leftMap(Inl(_))

    implicit def toEitherTOps[F[_]: Monad, E <: Coproduct, A](
        self: EitherT[F, E, A]): EitherTOps[F, E, A] = new EitherTOps(self)

    implicit def toEitherTLiftOps[F[_], A](self: F[A]): EitherTLiftOps[F, A] =
      new EitherTLiftOps[F, A](self)

    implicit def toEitherTValueOps[A](self: A): EitherTValueOps[A] =
      new EitherTValueOps[A](self)
  }
}
