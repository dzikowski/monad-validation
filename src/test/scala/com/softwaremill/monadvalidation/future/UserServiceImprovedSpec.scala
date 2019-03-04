package com.softwaremill.monadvalidation.future

import com.softwaremill.monadvalidation.domain.future.{UserRepositoryNaive, UserServiceImproved}
import com.softwaremill.monadvalidation.{AbstractMonadValidationSpec, TestApp, TestAppBuilder}

import scala.concurrent.ExecutionContext

class UserServiceImprovedSpec extends AbstractMonadValidationSpec with TestAppBuilder {

  implicit def ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  override def withTestApp(test: TestApp => Unit): Unit =
    test {
      val repository = new UserRepositoryNaive()
      val service = new UserServiceImproved(repository)
      TestApp(repository, service)
    }
}

