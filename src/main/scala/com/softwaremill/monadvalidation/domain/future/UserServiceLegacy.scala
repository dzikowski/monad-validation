package com.softwaremill.monadvalidation.domain.future

import com.softwaremill.monadvalidation.domain.ValidationError._
import com.softwaremill.monadvalidation.domain.{User, UserRepository, UserService, ValidationError}

import scala.concurrent.{ExecutionContext, Future}

class UserServiceLegacy(repository: UserRepository)(implicit ec: ExecutionContext) extends UserService {

  def saveUser(name: String, age: Int): Future[Either[ValidationError, User]] =
    repository.findUser(name).flatMap {
      case Some(user) => Future.successful(Left(UserAlreadyExists(user.name)))
      case None if !nameIsValid(name) => Future.successful(Left(InvalidName(name)))
      case None if !ageIsValid(age) => Future.successful(Left(InvalidAge(age)))
      case _ => repository.putUser(User(name, age)).map(Right.apply)
    }

  def updateAge(name: String, age: Int): Future[Either[ValidationError, User]] =
    repository.findUser(name).flatMap {
      case None => Future.successful(Left(UserNofFound(name)))
      case Some(_) if !ageIsValid(age) => Future.successful(Left(InvalidAge(age)))
      case Some(u) => repository.putUser(u.copy(age = age)).map(Right.apply)
    }

  private def nameIsValid(name: String): Boolean =
    name.length > 2

  private def ageIsValid(age: Int): Boolean =
    0 <= age && age <= 150
}
