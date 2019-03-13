package com.softwaremill.monadvalidation.domain.future

import com.softwaremill.monadvalidation.domain.ValidationError._
import com.softwaremill.monadvalidation.domain._
import com.softwaremill.monadvalidation.lib.ValidationResultLib

import scala.concurrent.{ExecutionContext, Future}

object FuturesValidation extends ValidationResultLib[Future] {
  import cats.Monad
  import cats.instances.future.catsStdInstancesForFuture

  implicit def monad(implicit ec: ExecutionContext): Monad[Future] = catsStdInstancesForFuture
}

class UserServiceFinal(repository: UserRepository, ageService: AgeService)(implicit ec: ExecutionContext) extends UserService {

  import FuturesValidation._

  def saveUser(name: String, age: Int, country: String): Future[Either[ValidationError, User]] = {
    val validationResult =
      for {
        _ <- validateIfUserDoesNotExist(name)
        _ <- validateName(name)
        _ <- validateAge(age, country)
      } yield ()

    validationResult.onSuccess(repository.putUser(User(name, age, country)))
  }

  def updateAge(name: String, age: Int): Future[Either[ValidationError, User]] = {
    val validationResult =
      for {
        u <- getUser(name)
        _ <- validateAge(age, u.country)
      } yield u

    validationResult.onSuccess(u => repository.putUser(u.copy(age = age)))
  }

  private def validateIfUserDoesNotExist(name: String): ValidationResult[ValidationError, Unit] = {
    val userDoesNotExist = repository.findUser(name).map(_.isEmpty)
    ValidationResult.ensureF(userDoesNotExist, onFailure = UserAlreadyExists(name))
  }

  private def getUser(name: String): ValidationResult[ValidationError, User] =
    ValidationResult.fromOptionF(repository.findUser(name), ifNone = UserNofFound(name))

  private def validateName(name: String): ValidationResult[ValidationError, Unit] =
    ValidationResult.ensure(name.length > 2, onFailure = InvalidName(name))

  private def validateAge(age: Int, country: String): ValidationResult[ValidationError, Unit] = {
    val isAgeValid = ageService.isAgeValid(age, country)
    ValidationResult.ensureF(isAgeValid, onFailure = InvalidAge(age, country))
  }
}
