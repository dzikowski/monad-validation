package com.softwaremill.monadvalidation.lib

import cats.Monad
import cats.data.EitherT

import scala.language.higherKinds

trait ValidationMonad[M[_]] extends Monad[M] {

  def pure[A](x: A): M[A]

  def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]

  override def tailRecM[A, B](a: A)(f: A => M[Either[A, B]]): M[B] =
    flatMap(f(a))({
      case Right(b) => pure(b)
      case Left(a1) => tailRecM(a1)(f)
    })
}

trait ValidationResultLib[M[_]] {

  type ValidationResult[F, S] = EitherT[M, F, S]

  object ValidationResult {

    def successful[F, S](s: S)(implicit m: ValidationMonad[M]): ValidationResult[F, S] =
      EitherT.rightT(s)

    def failed[F, S](f: F)(implicit m: ValidationMonad[M]): ValidationResult[F, S] =
      EitherT.leftT(f)

    def ensure[F](c: => Boolean, onFailure: => F)(implicit m: ValidationMonad[M]): ValidationResult[F, Unit] =
      EitherT.cond[M](c, Unit, onFailure)

    def ensureF[F](c: => M[Boolean], onFailure: => F)(implicit m: ValidationMonad[M]): ValidationResult[F, Unit] =
      EitherT.right(c).ensure(onFailure)(b => b).map(_ => Unit)

    def fromOptionF[F, S](opt: M[Option[S]], ifNone: => F)(implicit m: ValidationMonad[M]): ValidationResult[F, S] =
      EitherT.fromOptionF(opt, ifNone)
  }

  implicit class ValidationResultOps[F, S](vr: ValidationResult[F, S]) {

    def onSuccess[S2](s2: => M[S2])(implicit m: ValidationMonad[M]): M[Either[F, S2]] =
      vr.onSuccess(_ => s2)

    def onSuccess[S2](fn: S => M[S2])(implicit m: ValidationMonad[M]): M[Either[F, S2]] =
      vr.semiflatMap(fn).value
  }
}
