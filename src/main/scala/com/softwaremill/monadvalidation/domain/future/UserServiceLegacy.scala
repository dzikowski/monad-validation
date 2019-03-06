package com.softwaremill.monadvalidation.domain.future

import com.softwaremill.monadvalidation.domain.ValidationError._
import com.softwaremill.monadvalidation.domain.{User, UserRepository, UserService, ValidationError}

import scala.concurrent.{ExecutionContext, Future}

trait AgeService {
  def isAgeValid(age: Int, country: String): Future[Boolean]
}

class UserServiceLegacy(repository: UserRepository, ageService: AgeService)(implicit ec: ExecutionContext) extends UserService {

  def saveUser(name: String, age: Int, country: String): Future[Either[ValidationError, User]] =
    if (name.length >= 2)
      repository.findUser(name).flatMap {
        case Some(_) => Future.successful(Left(UserAlreadyExists(name)))
        case None =>
          ageService.isAgeValid(age, country).flatMap {
            case false => Future.successful(Left(InvalidAge(age, country)))
            case true => repository.putUser(User(name, age, country)).map(u => Right(u))
          }
      }
    else
      Future.successful(Left(InvalidName(name)))

  def updateAge(name: String, age: Int): Future[Either[ValidationError, User]] =
    repository.findUser(name).flatMap {
      case None => Future.successful(Left(UserNofFound(name)))
      case Some(user) =>
        ageService.isAgeValid(age, user.country).flatMap {
          case false => Future.successful(Left(InvalidAge(age, user.country)))
          case true => repository.putUser(user.copy(age = age)).map(u => Right(u))
        }
    }
}
