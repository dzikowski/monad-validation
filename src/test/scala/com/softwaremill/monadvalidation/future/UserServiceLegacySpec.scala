package com.softwaremill.monadvalidation.future

import com.softwaremill.monadvalidation.domain.future.{AgeServiceNaive, UserRepositoryNaive, UserServiceLegacy}
import com.softwaremill.monadvalidation.{AbstractUserServiceSpec, TestApp, TestAppBuilder}

import scala.concurrent.ExecutionContext

class UserServiceLegacySpec extends AbstractUserServiceSpec with TestAppBuilder {

  implicit def ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  override def withTestApp(test: TestApp => Unit): Unit =
    test {
      val repository = new UserRepositoryNaive()
      val service = new UserServiceLegacy(repository, new AgeServiceNaive)
      TestApp(repository, service)
    }
}
