package org.ornamental

import cats.data.EitherT
import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.ornamental.errors.syntax._
import org.ornamental.mergeable.syntax._
import shapeless.{:+:, Coproduct}

import java.util.Locale

object Playground extends IOApp {

  private val random: Random[IO] = Random.javaUtilConcurrentThreadLocalRandom[IO]

  final case class MisconfigurationError()

  final case class User(id: Int, name: String, email: String, language: Locale)

  final class UserApiClient {

    /**
     * Contract: This client does NOT guarantee that the effect is infallible, (`Throwable`
     * channel of `IO` has to be inspected).
     */
    def getUserData(userId: Int): IO[Option[User]] =
      random.nextBoolean >>= { failure =>
        IO.raiseWhen(failure)(new Throwable("Error retrieving user data.")) as
          Option.when(userId > 0 && userId < 10_000)(
            User(userId, s"User $userId", s"user-$userId@ornamental.org", Locale.FRANCE))
      }
  }

  final case class TemplateNotFound(name: String)

  final case class BadTemplateArgs(arg: String, error: String)

  final case class NetworkingError(cause: String)

  final class UserNotificationClient {

    type SendNotificationError = TemplateNotFound :+: BadTemplateArgs |+: NetworkingError

    /**
     * Contract: This client guarantees that the effect is infallible, all errors are in left
     * channel of `EitherT`, `IO` does not throw.
     */
    def sendNotification(
        email: String,
        templateName: String,
        templateArgs: Map[String, Any]): EitherT[IO, SendNotificationError, Unit] =
      EitherT.fromEither[IO] {
        for {
          _ <- Either.cond(
            templateName === "greeting-template",
            (),
            Coproduct[SendNotificationError](TemplateNotFound("greeting-template"))
          )
          _ <- Either.cond(
            templateArgs.contains("user-name"),
            (),
            Coproduct[SendNotificationError](
              BadTemplateArgs("user-name", "Missing required template argument.")))
          _ <- Either.cond(
            email.endsWith("@ornamental.org"),
            (),
            Coproduct[SendNotificationError](NetworkingError("Mailbox not accessible.")))
        } yield ()
      }
  }

  final case class UserNotFound()

  override def run(args: List[String]): IO[ExitCode] = {

    val userClient = new UserApiClient
    val userNotificationClient = new UserNotificationClient

    type SendGreetingError = MisconfigurationError :+: NetworkingError |+: UserNotFound

    def sendGreeting(userId: Int): EitherT[IO, SendGreetingError, Unit] = {

      val raiseMisconfigurationError = (_: Any) => MisconfigurationError().raiseError[IO, Unit]

      userClient
        .getUserData(userId)
        .liftErrorAs((ex: Throwable) => NetworkingError(ex.getMessage))
        .flatMapMerge {
          case Some(user) =>
            userNotificationClient
              .sendNotification(user.email, "greeting-template", Map("user-name" -> user.name))
              .handle[TemplateNotFound](raiseMisconfigurationError)
              .handle[BadTemplateArgs](raiseMisconfigurationError)
              .widenError[SendGreetingError]()
          case None =>
            EitherT.leftT[IO, Unit](Coproduct[SendGreetingError](UserNotFound()))
        }
    }

    (sendGreeting(100) as ExitCode.Success)
      .handle[MisconfigurationError] { _ => ExitCode(-1).pureNoError[IO] }
      .handle[UserNotFound] { _ => ExitCode(-2).pureNoError[IO] }
      .handle[NetworkingError] { _ => ExitCode(-4).pureNoError[IO] }
      .allHandled
  }
}
