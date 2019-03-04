package com.softwaremill.monadvalidation

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

object ValidationResult {

  type ValidationResult[M[_], F, S] = EitherT[M, F, S]

  def successfulT[M[_]: ValidationMonad, S](s: S): ValidationResult[M, _, S] =
    EitherT.rightT[M, S](s)

  def successful[M[_]: ValidationMonad, F, S](fs: M[S]): ValidationResult[M, F, S] =
    EitherT.right(fs)

  def failedT[M[_]: ValidationMonad, F, S](f: F): ValidationResult[M, F, S] =
    EitherT.leftT(f)

  def cond[M[_]: ValidationMonad, F, S](c: => Boolean, success: S, failure: F): ValidationResult[M, F, S] =
    EitherT.cond[M](c, success, failure)

  def fromOptionF[M[_]: ValidationMonad, F, S](opt: M[Option[S]], ifNone: => F): ValidationResult[M, F, S] =
    EitherT.fromOptionF(opt, ifNone)

  implicit class ValidationResultOps[M[_]: ValidationMonad, F, S](vr: ValidationResult[M, F, S]){

    def onSuccess[S2](s2: => M[S2]): M[Either[F, S2]] =
      vr.onSuccess(_ => s2)

    def onSuccess[S2](fn: S => M[S2]): M[Either[F, S2]] =
      vr.semiflatMap(fn).value
  }
}