package com.softwaremill.monadvalidation.future

import com.softwaremill.monadvalidation.domain.future.{AgeServiceNaive, UserRepositoryNaive, UserServiceFinal}
import com.softwaremill.monadvalidation.{AbstractUserServiceSpec, TestApp, TestAppBuilder}

import scala.concurrent.ExecutionContext

class UserServiceFinalSpec extends AbstractUserServiceSpec with TestAppBuilder {

  implicit def ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  override def withTestApp(test: TestApp => Unit): Unit =
    test {
      val repository = new UserRepositoryNaive()
      val service = new UserServiceFinal(repository, new AgeServiceNaive)
      TestApp(repository, service)
    }
}

