package com.softwaremill.monadvalidation

import scala.concurrent.Future

package object domain {

  case class User(name: String, age: Int, country: String)

  trait UserRepository {
    def putUser(user: User): Future[User]
    def findUser(name: String): Future[Option[User]]
  }

  trait AgeService {
    def isAgeValid(age: Int, country: String): Future[Boolean]
  }

  sealed trait ValidationError

  object ValidationError {
    case class UserAlreadyExists(name: String) extends ValidationError
    case class UserNofFound(name: String) extends ValidationError
    case class InvalidName(name: String) extends ValidationError
    case class InvalidAge(age: Int, country: String) extends ValidationError
  }

  trait UserService {
    def saveUser(name: String, age: Int, country: String): Future[Either[ValidationError, User]]
    def updateAge(name: String, age: Int): Future[Either[ValidationError, User]]
  }
}
