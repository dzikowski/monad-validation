package com.softwaremill.monadvalidation.domain.future
import scala.concurrent.Future

class AgeServiceNaive extends  AgeService {
  override def isAgeValid(age: Int, country: String): Future[Boolean] =
    Future.successful {
      if (country == "US")
        age >= 21
      else
        age >= 18
    }
}
