package com.softwaremill.monadvalidation

import com.softwaremill.monadvalidation.domain.{UserRepository, UserService}

case class TestApp(repository: UserRepository, service: UserService)

trait TestAppBuilder {
  def withTestApp(test: TestApp => Unit)
}


