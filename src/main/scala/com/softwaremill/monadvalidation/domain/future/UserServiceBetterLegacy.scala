package com.softwaremill.monadvalidation.domain.future

import com.softwaremill.monadvalidation.domain.ValidationError._
import com.softwaremill.monadvalidation.domain.{User, UserRepository, UserService, ValidationError}

import scala.concurrent.{ExecutionContext, Future}


class UserServiceBetterLegacy(repository: UserRepository, ageService: AgeService)(implicit ec: ExecutionContext) extends UserService {

  def saveUser(name: String, age: Int, country: String): Future[Either[ValidationError, User]] =
    onSuccess(validateName(name), { _: Unit =>
      onSuccess(validateUser(name), { _: Unit =>
        onSuccess(validateAge(age, country), { _: Unit =>
          repository.putUser(User(name, age, country)).map(u => Right(u))
        })
      })
    })

  def updateAge(name: String, age: Int): Future[Either[ValidationError, User]] =
    onSuccess(getUser(name), { user: User =>
      onSuccess(validateAge(age, user.country), { _: Unit =>
        repository.putUser(user.copy(age = age)).map(u => Right(u))
      })
    })

  private def validateName(name: String): Future[Either[ValidationError, Unit]] =
    Future.successful(Either.cond(name.length >= 2, right = Unit, left = InvalidName(name)))

  private def validateUser(name: String): Future[Either[ValidationError, Unit]] =
    repository.findUser(name)
      .map(opt => Either.cond(opt.isEmpty, right = Unit, left = UserAlreadyExists(name)))

  private def validateAge(age: Int, country: String): Future[Either[ValidationError, Unit]] =
    ageService.isAgeValid(age, country)
      .map(isValid => Either.cond(isValid, right = Unit, left = InvalidAge(age, country)))

  private def getUser(name: String): Future[Either[ValidationError, User]] =
    repository.findUser(name).map {
      case Some(user) => Right(user)
      case None => Left(UserNofFound(name))
    }

  private def onSuccess[T1, T2](et: Future[Either[ValidationError, T1]], fn: T1 => Future[Either[ValidationError, T2]]): Future[Either[ValidationError, T2]] =
    et.flatMap {
      case Right(t1) => fn(t1)
      case Left(f) => Future.successful(Left(f))
    }
}
