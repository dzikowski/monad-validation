package com.softwaremill.monadvalidation

import com.softwaremill.monadvalidation.domain.ValidationError._
import com.softwaremill.monadvalidation.domain._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext

abstract class AbstractMonadValidationSpec
  extends FlatSpec
    with ScalaFutures
    with Matchers
    with TestAppBuilder {

  implicit def ec: ExecutionContext

  it should "save user" in {

    // Given
    val app = buildTestApp()
    val name = "John"
    val age = 42

    // When
    val result = app.service.saveUser(name, age).futureValue

    // Then
    result shouldBe Right(User(name, age))
    app.repository.findUser(name).futureValue shouldBe Some(User(name, age))
  }

  it should "fail save user if user with the same name exists" in {

    // Given
    val app = buildTestApp()
    val name = "John"
    val age = 42

    app.service.saveUser(name, age).futureValue
    app.repository.findUser(name).futureValue shouldBe Some(User(name, age))

    // When
    val result = app.service.saveUser(name, 33).futureValue

    // Then
    result shouldBe Left(UserExists(name))
    app.repository.findUser(name).futureValue shouldBe Some(User(name, age))
  }

  it should "fail to save user if the name is invalid" in {

    // Given
    val app = buildTestApp()
    val name = "y"
    val age = 42

    // When
    val result = app.service.saveUser(name, age).futureValue

    // Then
    result shouldBe Left(InvalidName(name))
    app.repository.findUser(name).futureValue shouldBe None
  }

  it should "fail to save user if the age is invalid" in {

    // Given
    val app = buildTestApp()
    val name = "John"
    val age = -42

    // When
    val result = app.service.saveUser(name, age).futureValue

    // Then
    result shouldBe Left(InvalidAge(age))
    app.repository.findUser(name).futureValue shouldBe None
  }

  it should "update user's age" in {

    // Given
    val app = buildTestApp()
    val name = "John"
    val age1 = 42
    val age2 = 43

    app.service.saveUser(name, age1).futureValue
    app.repository.findUser(name).futureValue shouldBe Some(User(name, age1))

    // When
    val result = app.service.updateAge(name, age2).futureValue

    // Then
    result shouldBe Right(User(name, age2))
    app.repository.findUser(name).futureValue shouldBe Some(User(name, age2))
  }

  it should "fail tu update user's age if user is missing" in {

    // Given
    val app = buildTestApp()
    val name = "John"
    val age = 42

    app.repository.findUser(name).futureValue shouldBe None

    // When
    val result = app.service.updateAge(name, age).futureValue

    // Then
    result shouldBe Left(UserNofFound(name))
    app.repository.findUser(name).futureValue shouldBe None
  }

  it should "fail tu update user's age if the age is invalid" in {

    // Given
    val app = buildTestApp()
    val name = "John"
    val age1 = 42
    val age2 = 430

    app.service.saveUser(name, age1).futureValue
    app.repository.findUser(name).futureValue shouldBe Some(User(name, age1))

    // When
    val result = app.service.updateAge(name, age2).futureValue

    // Then
    result shouldBe Left(InvalidAge(age2))
    app.repository.findUser(name).futureValue shouldBe Some(User(name, age1))
  }
}
