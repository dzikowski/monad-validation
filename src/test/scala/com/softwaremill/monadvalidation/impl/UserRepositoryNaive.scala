package com.softwaremill.monadvalidation.impl

import com.softwaremill.monadvalidation.domain.{User, UserRepository}

import scala.concurrent.Future

class UserRepositoryNaive() extends UserRepository {

  private var users: Map[String, User] = Map.empty

  def putUser(user: User): Future[User] =
    Future.successful({
      users = users + (user.name -> user)
      user
    })

  def findUser(name: String): Future[Option[User]] =
    Future.successful(users.get(name))
}

