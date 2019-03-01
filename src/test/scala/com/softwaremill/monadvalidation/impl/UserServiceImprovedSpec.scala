package com.softwaremill.monadvalidation.impl

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

