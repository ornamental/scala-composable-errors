package org.ornamental.errors

import shapeless.{:+:, <:!<, CNil, Coproduct, Inl, Inr}

trait PartitionCovariant[C <: Coproduct, U] {

  type Subtypes <: Coproduct

  type Rest <: Coproduct

  type Out = Either[Rest, Subtypes]

  def apply(c: C): Out
}

object PartitionCovariant {

  type Aux[C <: Coproduct, U, S <: Coproduct, R <: Coproduct] = PartitionCovariant[C, U] {

    type Subtypes = S

    type Rest = R
  }

  def apply[C <: Coproduct, U](
      implicit
      partition: PartitionCovariant[C, U]): Aux[C, U, partition.Subtypes, partition.Rest] =
    partition

  implicit def cNilPartitionCovariant[U]: Aux[CNil, U, CNil, CNil] =
    new PartitionCovariant[CNil, U] {

      override type Subtypes = CNil

      override type Rest = CNil

      override def apply(c: CNil): Out = c.impossible
    }

  implicit def cConsPartitionCovariantMatch[
      U,
      H <: U,
      T <: Coproduct,
      TS <: Coproduct,
      TR <: Coproduct](implicit base: Aux[T, U, TS, TR]): Aux[H :+: T, U, H :+: TS, TR] =
    new PartitionCovariant[H :+: T, U] {

      override type Subtypes = H :+: TS

      override type Rest = TR

      override def apply(c: H :+: T): Out = c match {
        case Inl(head) => Right(Inl(head))
        case Inr(tail) => base(tail).map(Inr(_))
      }
    }

  implicit def cConsPartitionCovariantNoMatch[
      U,
      H,
      T <: Coproduct,
      TS <: Coproduct,
      TR <: Coproduct](
      implicit base: Aux[T, U, TS, TR],
      evidence: H <:!< U): Aux[H :+: T, U, TS, H :+: TR] =
    new PartitionCovariant[H :+: T, U] {

      override type Subtypes = TS

      override type Rest = H :+: TR

      override def apply(c: H :+: T): Out = c match {
        case Inl(head) => Left(Inl(head))
        case Inr(tail) => base(tail).left.map(Inr(_))
      }
    }

}
