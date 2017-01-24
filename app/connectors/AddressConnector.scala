/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package connectors

import models.{DetailedAddress, SimpleAddress}
import play.api.libs.json.{JsArray, JsDefined, JsNumber, JsValue}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ExecutionContext, Future}

class AddressConnector(http: HttpGet with HttpPost with HttpPut)(implicit ec: ExecutionContext) extends ServicesConfig {

  val url = baseUrl("external-business-rates-data-platform") + "/address-management-api/address"

  def find(postcode: String)(implicit hc: HeaderCarrier): Future[Seq[SimpleAddress]] = {
    http.GET[JsValue](s"""$url?pageSize=100&startPoint=1&SearchParameters={"postcode": "$postcode"}""") map { js =>
      js \ "addressDetails" match {
        case JsDefined(a@JsArray(_)) => a.as[Seq[DetailedAddress]].map(_.simplify)
        case _ => Nil
      }
    }
  }

  def get(addressUnitId: Int)(implicit hc: HeaderCarrier): Future[Option[DetailedAddress]] = {
    http.GET[Option[DetailedAddress]](s"$url/$addressUnitId")
  }

  def create(address: SimpleAddress)(implicit hc: HeaderCarrier): Future[Int] = {
    http.POST[DetailedAddress, JsValue](s"$url/non_standard_address", address.toDetailedAddress) map { js =>
      js \ "addressUnitId" match {
        case JsDefined(JsNumber(n)) => n.toInt
        case _ => throw new Exception(s"Failed to create record for address $address")
      }
    }
  }

  def update(nonAbpId: Int, address: DetailedAddress)(implicit hc: HeaderCarrier): Future[Unit] = {
    http.PUT[DetailedAddress, HttpResponse](s"$url/non_standard_address/$nonAbpId", address) map { _ => () }
  }
}
