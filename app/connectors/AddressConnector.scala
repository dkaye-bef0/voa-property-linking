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

import models.{APIAddressLookupResult, DetailedAddress, SimpleAddress}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsDefined, JsNumber, JsValue}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.Future

class AddressConnector(http: HttpGet with HttpPost with HttpPut) extends ServicesConfig {

  lazy val url = baseUrl("external-business-rates-data-platform") + "/address-management-api/address"

  def find(postcode: String)(implicit hc: HeaderCarrier): Future[Seq[SimpleAddress]] = {
    http.GET[APIAddressLookupResult](s"""$url?pageSize=100&startPoint=1&searchparams={"postcode": "$postcode"}""") map { res =>
      res.addressDetails.map(_.simplify)
    }
  }

  def get(addressUnitId: Int)(implicit hc: HeaderCarrier): Future[Option[DetailedAddress]] = {
    http.GET[APIAddressLookupResult](s"$url/$addressUnitId") map { _.addressDetails.headOption }
  }

  def create(address: SimpleAddress)(implicit hc: HeaderCarrier): Future[Int] = {
    http.POST[DetailedAddress, JsValue](s"$url/non_standard_address", address.toDetailedAddress) map { js =>
      js \ "id" match {
        case JsDefined(JsNumber(n)) => n.toInt
        case _ => throw new Exception(s"Failed to create record for address $address")
      }
    }
  }

  def update(nonAbpId: Int, address: DetailedAddress)(implicit hc: HeaderCarrier): Future[Unit] = {
    http.PUT[DetailedAddress, HttpResponse](s"$url/non_standard_address/$nonAbpId", address) map { _ => () }
  }
}
