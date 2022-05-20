package org.ornamental.errors

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import cats.{Applicative, ApplicativeError, Functor, Monad}
import cats.data.EitherT
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.ornamental.mergeable.MergeEither
import shapeless._

final class EitherTOps[F[_], E <: Coproduct, A](val self: EitherT[F, E, A]) extends AnyVal {

  /**
   * Handles the left values of the specified type `C`. The coproduct elements being subtypes of
   * `C` are handled (no runtime type inspection is performed). At least one coproduct element
   * must be of type `C` or its subtype.<br/> Subtypes of `C` are excluded from the left types
   * coproduct.
   */
  def handle[C]: EitherTOps.HandlePartiallyApplied[F, E, A, C] =
    new EitherTOps.HandlePartiallyApplied(self)

  /**
   * Handles the left values of the specified type `C` satisfying additional conditions. The
   * coproduct elements being subtypes of `C` are handled (no runtime type inspection is
   * performed). At least one coproduct element must be of type `C` or its subtype.<br/>
   * Subtypes of `C` are not excluded from the left types coproduct.
   */
  def handleIf[C]: EitherTOps.HandleIfPartiallyApplied[F, E, A, C] =
    new EitherTOps.HandleIfPartiallyApplied(self)

  /**
   * Lifts the error from the underlying monad `F` into the left channel of `EitherT`.
   */
  def liftInnerError[E2, R](
      implicit me: ApplicativeError[F, E2],
      mergeEither: MergeEither.Aux[E, E2 :+: CNil, R]): EitherT[F, R, A] =
    liftInnerErrorAs(identity[E2])

  /**
   * Maps error from the underlying monad `F` and lifts it into the left channel of `EitherT`.
   */
  def liftInnerErrorAs[E2, EM, R](m: E2 => EM)(
      implicit me: ApplicativeError[F, E2],
      mergeEither: MergeEither.Aux[E, EM :+: CNil, R]): EitherT[F, R, A] =
    EitherT(
      me.attempt(self.value).map {
        case Left(errorF) => Left(mergeEither.fromB(Inl(m(errorF))))
        case Right(Left(errorEitherT)) => Left(mergeEither.fromA(errorEitherT))
        case Right(Right(value)) => Right(value)
      }
    )

  /**
   * Remaps (with replacement) errors of the specified type to another type.
   */
  def mapError[E2, EM, E_ <: Coproduct, Sub <: Coproduct, Rst <: Coproduct, Q](m: E2 => EM)(
      implicit fm: Monad[F],
      normalize: MergeEither.Aux[E, CNil, E_],
      partition: PartitionCovariant.Aux[E_, E2, Sub, Rst],
      @implicitNotFound(
        "The normalization of ${E} does not contain provable subtypes of ${C}. No cases to handle.")
      canHandle: Sub !=:= CNil,
      mergeEither: MergeEither.Aux[Rst, EM :+: CNil, Q]): EitherT[F, Q, A] =
    new EitherTOps.HandlePartiallyApplied[F, E, A, E2](self)
      .apply[E_, EM :+: CNil, Sub, Rst, Q, A](e2 => EitherT.leftT[F, A](Inl(m(e2))))

  /**
   * Widens the possible set of error types.
   *
   * @tparam T
   *   the error types required to be present in the resulting type
   */
  def widenError[T <: Coproduct]: EitherTOps.WidenErrorPartiallyApplied[F, E, A, T] =
    new EitherTOps.WidenErrorPartiallyApplied[F, E, A, T](self)

  /**
   * Converts `EitherT[F, CNil, A]` to `F[A]`, since the left channel is necessarily empty.
   */
  def allHandled(implicit fn: Functor[F], nil: E =:= CNil): F[A] =
    self.leftMap(x => nil(x).impossible).merge
}

final class EitherTValueOps[A](val self: A) extends AnyVal {

  /**
   * Alias for `pure` of `EitherT[F, CNil, *]` applicative.
   */
  def pureNoError[F[_]](implicit ap: Applicative[F]): EitherT[F, CNil, A] =
    org.ornamental.errors.syntax.pureNoError[F](self)

  /**
   * For given error instance of type `E`, lifts the error into `EitherT[F, E :+: CNil, *]`
   * applicative.
   */
  def raiseError[F[_], B](implicit ap: Applicative[F]): EitherT[F, A :+: CNil, B] =
    Functor[EitherT[F, A :+: CNil, *]]
      .widen[Nothing, B](org.ornamental.errors.syntax.raiseError[F](self))
}

final class EitherTLiftOps[F[_], A](val self: F[A]) extends AnyVal {

  /**
   * Lifts `F[A]` into `EitherT[F, CNil, A]` (empty left channel).
   */
  def liftNoError(implicit fn: Functor[F]): EitherT[F, CNil, A] = EitherT.right[CNil](self)

  /**
   * Lifts `F[A]` into `EitherT[F, E, A]` handling possible error of type `E` from `F[A]`.
   */
  def liftError[E](
      implicit applicativeError: ApplicativeError[F, E]): EitherT[F, E :+: CNil, A] =
    self.attemptT.leftMap(Inl(_))

  /**
   * Lifts `F[A]` into `EitherT[F, E2, A]` handling possible error of type `E` from `F[A]` and
   * mapping it to `E2`.
   */
  def liftErrorAs[E, E2](errorMap: E => E2)(
      implicit applicativeError: ApplicativeError[F, E]): EitherT[F, E2 :+: CNil, A] =
    self.attemptT.leftMap(e => Inl(errorMap(e)))
}

object EitherTOps {

  private[errors] final class HandlePartiallyApplied[F[_], E <: Coproduct, A, C](
      val fa: EitherT[F, E, A])
      extends AnyVal {

    def apply[E_ <: Coproduct, E2 <: Coproduct, Sub <: Coproduct, Rst <: Coproduct, Q, B <: A](
        handler: C => EitherT[F, E2, B])(
        implicit fm: Monad[F],
        normalize: MergeEither.Aux[E, CNil, E_],
        partition: PartitionCovariant.Aux[E_, C, Sub, Rst],
        @implicitNotFound(
          "The normalization of ${E} does not contain provable subtypes of ${C}. No cases to handle.")
        canHandle: Sub !=:= CNil,
        mergeEither: MergeEither.Aux[Rst, E2, Q]): EitherT[F, Q, A] = {

      val unwrapped = fa.value >>= {
        case Left(err) =>
          partition(normalize.fromA(err)) match {
            case Right(matching) =>
              val errC = Coproduct.unsafeGet(matching).asInstanceOf[C]
              handler(errC).widen[A].leftMap(mergeEither.fromB).value
            case Left(nonMatching) =>
              Left(mergeEither.fromA(nonMatching)).withRight[A].pure[F]
          }
        case Right(value) => Right(value).withLeft[Q].pure[F]
      }

      EitherT(unwrapped)
    }
  }

  private[errors] final class HandleIfPartiallyApplied[F[_], E <: Coproduct, A, C](
      val fa: EitherT[F, E, A])
      extends AnyVal {

    def apply[E_ <: Coproduct, E2 <: Coproduct, Sub <: Coproduct, Rst <: Coproduct, Q, B <: A](
        handler: PartialFunction[C, EitherT[F, E2, B]])(
        implicit fm: Monad[F],
        normalize: MergeEither.Aux[E, E, E_],
        partition: PartitionCovariant.Aux[E_, C, Sub, Rst],
        @implicitNotFound(
          "The ation of ${E} does not contain provable subtypes of ${C}. No cases to handle.")
        canHandle: Sub !=:= CNil,
        mergeEither: MergeEither.Aux[E_, E2, Q]): EitherT[F, Q, A] = {

      val unwrapped = fa.value >>= {
        case Left(err) =>
          val normalized = normalize.fromA(err)
          partition(normalized) match {
            case Right(matching) =>
              val errC = Coproduct.unsafeGet(matching).asInstanceOf[C]
              handler.lift(errC) match {
                case None => Left(mergeEither.fromA(normalized)).withRight[A].pure[F]
                case Some(h) => h.widen[A].leftMap(mergeEither.fromB).value
              }
            case _ => Left(mergeEither.fromA(normalized)).withRight[A].pure[F]
          }
        case Right(value) => Right(value).withLeft[Q].pure[F]
      }

      EitherT(unwrapped)
    }
  }

  private[errors] final class PureNoErrorPartiallyApplied[F[_]](val dummy: Boolean = true)
      extends AnyVal {

    def apply[A](a: A)(implicit ap: Applicative[F]): EitherT[F, CNil, A] =
      EitherT.pure[F, CNil](a)
  }

  private[errors] final class RaiseErrorPartiallyApplied[F[_]](val dummy: Boolean = true)
      extends AnyVal {

    def apply[E](e: E)(implicit ap: Applicative[F]): EitherT[F, E :+: CNil, Nothing] =
      EitherT.leftT[F, Nothing](Inl(e))
  }

  private[errors] final class WidenErrorPartiallyApplied[
      F[_],
      E <: Coproduct,
      A,
      T <: Coproduct](val fa: EitherT[F, E, A])
      extends AnyVal {

    def apply[N <: Coproduct]()(
        implicit fn: Functor[F],
        mergeEither: MergeEither.Aux[E, T, N]): EitherT[F, N, A] = fa.leftMap(mergeEither.fromA)
  }
}
