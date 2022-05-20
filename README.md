# [POC] Functional Error Handling with Mergeable Monads

## Problem Statement

When dealing with errors in monadic computations, common practice is to use a single most common error type to define
the possible error outcomes (via `EitherT` or a like monad transformer or by incorporating the error type directly in
the monad, like `Throwable` for `IO` of cats-effect or user-specified type for ZIO's `ZIO`).

As the result, to preserve monad composability, all errors must be subtypes of the chosen common type, even though they
may belong to different areas of the business domain.

Error handling assumes the handling code takes an argument of the common error type, which is essentially as bad a
practice as `try { .. } catch { case ex: Throwable => .. }` in imperative Scala.

This project is a proof of concept bringing a more type-safe error handling, close to one provided by Java's checked
exceptions.

## Using Coproduct

The proposed approach is to use coproduct type (here, `Coproduct` from shapeless library) to encode the possible error
outcomes in `EitherT` monad transformer (the concept may be generalized onto monad families such as `ZIO`, which are
already parameterized by error type).

It will then be possible to handle a particular type of error, which would exclude it and its subtypes from the
coproduct type, much like `catch` statement in Java excludes types from method's `throws` declaration. The difference
from `catch` statement is that no runtime checks for the type are performed: a type element of coproduct is considered
handled if and only if it is provable at compile time that the handled error type is the supertype of the element type.

_Example._
Let `type ErrorCode = Int`.\
Let error `type E = String :+: ErrorCode :+: Throwable :+: CNil`, handled error `type H = AnyRef`.\
After handling, the resulting error `type R = ErrorCode :+: CNil`, since the compiler is able to prove
`String <: AnyRef` and `Throwable <: AnyRef`.

_Example._
Let `T1` and `T2` be traits and `type C <: T1 with T2`. Let error `type E = T1 :+: T2 :+: CNil`. If an error of type `C`
occurs, it will only be guaranteed to be handled if both `T1` and `T2` are handled.

## Mergeable Monads

Having monad parameterized by varying error type breaks composability, since we essentially are working with a different
monad for each given error type.\
The proposed solution is to define a merge operation over monads.

_Definition._ Let `ℳ` be a set of monads. Let `μ` map any two monads `M₁ ∈ ℳ` and `M₂ ∈ ℳ` to a triplet `(R, N₁, N₂)`
where `R ∈ ℳ`, `N₁` is a natural transformation from `M₁` to `R`, and `N₂` is a natural transformation from `M₂` to `R`.
We call the pair `(ℳ, μ)` a mergeable monad set.

This structure enables us to generalize `flatMap` to different monads from `ℳ`. We introduce `flatMapMerge` operation:\
`flatMapMerge(m1: M₁[A], f: A => M₂[B]): R[B] = flatMap(N₁(m1), a => N₂(f(a)))`.

## Implementation

We implement mergeable monad set as a type family `MonadMerge[M[_, _], C[_, _, _]]`.\
`M[_, _]` is a monad family, `M[L, *]` being a monad for each type `L` admissible as the left type parameter.\
`C[_, _, _]` is a type family encoding the merge relation between monads in `M`. For each two admissible
monad-parameterizing types `L1` and `L2` there must exist a single admissible monad-parameterizing type `R` such that an
implicit instance of `C[L1, L2, R]` is defined.

For error handling, we use `EitherT[F, *, *]` as `M`, `F[_]` being the undelying monad. The admissible
monad-parameterizing types are `shapeless.Coproduct` subtypes. Parameterization with `CNil` means that no errors may
occur in the left channel.

`C` is implemented using implicit macros (see _fundep materialization_).

### Error Type Normalization

One set of possible error types should normally be represented by the same `Coproduct` type. That is,

1. types constituting the coproduct should be ordered deterministically;
2. if type `A` is an element of error coproduct type, then no other type element `B` in this coproduct may be such
   that `A <: B` is provable.

Coproducts not satisfying these conditions do not arise when merging two error coproducts, since the implementation
takes care of yielding a normalized type.

Non-normalized coproducts may still occur when

1. manually constructing `EitherT[L <: Coproduct, A]`; `L` may be an arbitrary non-normalized coproduct.
2. working with generic error types.

_Example._\
Suppose this function is defined:

```
  // the inferred result type depends on type parameter A:
  def useDelegate[A](delegate: EitherT[IO, A :+: CNil, Array[Byte]])
    : EitherT[IO, String :+: A :+: CNil, Array[Byte]] =
     delegate >>+ { bytes =>
       leftToCoproduct(EitherT.cond[IO](
         bytes.length < (1 << 20),
         bytes,
         "Array is too long."))
     }
```

At the call site:

```
  val result: EitherT[IO, String :+: String :+: CNil, Array[Byte]] = 
    useDelegate(leftToCoproduct(
      EitherT.leftT[IO, Array[Byte]]("Could not fetch the array.")))
```

the resulting error type becomes non-normalized `String :+: String :+: CNil`, since `A` is `String`.

As merge result must be normalized, we additionally require that, given a mergeable monad set `(ℳ, μ)`,\
`μ(M₁, M₂) = (R, N₁, N₂) => μ(R, R) = (R, id, id)`.

Normalization may be manually performed via `normalize` when needed.
