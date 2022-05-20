package org.ornamental.mergeable

import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.tools.nsc.Global

import shapeless.{:+:, CNil, Coproduct}

@implicitNotFound("Could not find an instance of MergeEither for ${A} and ${B}.")
trait MergeEither[A, B] {

  type Combination

  def fromA(a: A): Combination

  def fromB(a: B): Combination
}

object MergeEither {

  type Aux[A, B, C] = MergeEither[A, B] { type Combination = C }

  def apply[A, B](implicit c: MergeEither[A, B]): Aux[A, B, c.Combination] = c

  implicit def materializeForCoproduct[A <: Coproduct, B <: Coproduct, C <: Coproduct]: Aux[
    A,
    B,
    C] =
    macro MergeMacros.materializeForCoproduct[A, B, C]
}

final class MergeMacros(val c: whitebox.Context) {

  import c.universe._

  private val global: Global = c.universe.asInstanceOf[scala.tools.nsc.Global]

  private val typeCoproduct: Type = typeOf[Coproduct]

  private val typeConstCoproductOr: Type = typeOf[_ :+: _].typeConstructor

  private val typeCNil: Type = typeOf[CNil]

  private val typeSymbolOrdering: Ordering[c.universe.Symbol] =
    Ordering
      .by[Symbol, String](_.fullName)
      .orElse(Ordering.fromLessThan((s1, s2) =>
        s1.asInstanceOf[global.Symbol].isLess(s2.asInstanceOf[global.Symbol])))

  private implicit val ordering: Ordering[Type] = Ordering.by(_.toString)

  def materializeForCoproduct[A: WeakTypeTag, B: WeakTypeTag, C: WeakTypeTag]: Tree = {
    val typeA: Type = weakTypeOf[A]
    val typeB: Type = weakTypeOf[B]
    checkIsCoproduct(typeA)
    checkIsCoproduct(typeB)

    val (typeC, aMapping, bMapping) = makeCombinedType(typeA, typeB)

    val fromA =
      if (aMapping.isEmpty) q"""a.impossible"""
      else q"""
           val pair: (Int, Any) = a match { case ..${buildRemapping(aMapping)} }
           shapeless.Coproduct.unsafeMkCoproduct(pair._1, pair._2).asInstanceOf[$typeC]
         """
    val fromB =
      if (bMapping.isEmpty) q"""b.impossible"""
      else q"""
           val pair: (Int, Any) = b match { case ..${buildRemapping(bMapping)} }
           shapeless.Coproduct.unsafeMkCoproduct(pair._1, pair._2).asInstanceOf[$typeC]
         """

    q"""
       new org.ornamental.mergeable.MergeEither[$typeA, $typeB] {

         type Combination = $typeC

         override def fromA(a: $typeA): $typeC = $fromA

         override def fromB(b: $typeB): $typeC = $fromB
       }
     """
  }

  private def checkIsCoproduct(tpe: Type): Unit =
    if (!(tpe <:< typeCoproduct)) {
      c.abort(
        c.enclosingPosition,
        s"Expected a Coproduct type, found $tpe."
      )
    }

  private def makeCombinedType(tpeA: Type, tpeB: Type): (Type, List[Int], List[Int]) = {
    val aTypes = getCoproductElements(tpeA)
    val bTypes = getCoproductElements(tpeB)

    val types = (aTypes ++ bTypes).foldLeft(Set.empty[Type]) { (acc, tpe) =>
      // remove all subtypes from acc, add tpe if no supertype was found
      val excludeSubtypes = acc.filterNot(_ <:< tpe)
      val hasSupertype = excludeSubtypes.exists(tpe <:< _)
      excludeSubtypes ++ Option.unless(hasSupertype)(tpe)
    }

    val sorted = types.toList.sortBy(_.typeSymbol)(typeSymbolOrdering)
    val combinedType = sorted.foldRight(typeCNil) { (tpe, acc) =>
      appliedType(typeConstCoproductOr, tpe, acc)
    }
    // c.info(c.enclosingPosition, s"Combined type: $combinedType", force = false)

    val indexed = sorted.zipWithIndex
    // for types in aTypes and bTypes .get will succeed (follows from construction of `types`)
    val mapping = (tpe: Type) => indexed.find(tpe <:< _._1).get._2
    val aMapping = aTypes.map(mapping)
    val bMapping = bTypes.map(mapping)
    // с.info(c.enclosingPosition, s"Mappings type: a: $aMapping, b: $bMapping", force = false)

    (combinedType, aMapping, bMapping)
  }

  private def buildRemapping(remapping: List[Int]): List[Tree] = {
    if (remapping.isEmpty) {
      c.abort(c.enclosingPosition, "Assertion failed: no constituent types to remap.")
    }

    cq"""${constructPattern(
        remapping.size - 1,
        innermostPattern = pq"shapeless.Inr(x)")} => x.impossible""" ::
      remapping.zipWithIndex.map {
        case (mappedIdx, originalIdx) =>
          cq"""${constructPattern(
              originalIdx,
              innermostPattern = pq"shapeless.Inl(x)")} => ($mappedIdx, x: Any)"""
      }
  }

  private def constructPattern(i: Int, innermostPattern: Tree): Tree =
    (0 until i).foldLeft(innermostPattern)((acc, _) => pq"shapeless.Inr($acc)")

  private def getCoproductElements(coproduct: Type): List[Type] = {
    @tailrec
    def unwrap(tpe: Type, acc: List[Type]): List[Type] =
      tpe.dealias match {
        case TypeRef(_, cons, List(head, tail))
            if cons.asType.toType.typeConstructor =:= typeConstCoproductOr =>
          unwrap(tail, head :: acc)
        case `typeCNil` => acc
        case _ => c.abort(c.enclosingPosition, s"Bad Coproduct type $coproduct.")
      }

    unwrap(coproduct, Nil).reverse
  }
}
