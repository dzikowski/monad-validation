package com.softwaremill.monadvalidation

import scala.concurrent.Future

object domain {

  case class User(name: String, age: Int)

  trait UserRepository {
    def putUser(user: User): Future[User]
    def findUser(name: String): Future[Option[User]]
  }

  sealed trait ValidationError

  object ValidationError {
    case class UserExists(name: String) extends ValidationError
    case class UserNofFound(name: String) extends ValidationError
    case class InvalidName(name: String) extends ValidationError
    case class InvalidAge(age: Int) extends ValidationError
  }

  trait UserService {
    def saveUser(name: String, age: Int): Future[Either[ValidationError, User]]
    def updateAge(name: String, age: Int): Future[Either[ValidationError, User]]
  }
}
