package org.ornamental

import org.ornamental.mergeable.MonadMerge.Ops

package object mergeable {

  object syntax {

    implicit def toOps[F[_, _], M[_, _, _], X, A](fa: F[X, A])(
        implicit monadMerge: MonadMerge[F, M]): Ops[F, M, X, A] =
      new Ops[F, M, X, A] {

        override def self: F[X, A] = fa

        override val typeClassInstance: MonadMerge[F, M] = monadMerge
      }
  }
}
