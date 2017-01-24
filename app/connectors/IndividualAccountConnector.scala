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

import config.Wiring
import models.{APIDetailedIndividualAccount, APIIndividualAccount, IndividualAccount, IndividualAccountWrite}
import play.api.libs.json.JsValue
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ExecutionContext, Future}

class IndividualAccountConnector(http: HttpGet with HttpPut with HttpPost)(implicit ec: ExecutionContext)
  extends ServicesConfig {

  lazy val baseUrl: String = baseUrl("external-business-rates-data-platform") + "/customer-management-api/person"
  val addresses = Wiring().addresses

  def create(account: IndividualAccountWrite)(implicit hc: HeaderCarrier): Future[JsValue] = {
    account.details.address.addressUnitId match {
      case Some(id) => http.POST[APIIndividualAccount, JsValue](baseUrl, account.toAPIIndividualAccount(id))
      case None => addresses.create(account.details.address) flatMap { id =>
        http.POST[APIIndividualAccount, JsValue](baseUrl, account.toAPIIndividualAccount(id))
      }
    }
  }

  def get(id: Int)(implicit hc: HeaderCarrier): Future[Option[IndividualAccount]] = {
    http.GET[Option[APIDetailedIndividualAccount]](s"$baseUrl?personId=$id") flatMap {
      case Some(a) => addresses.get(a.personLatestDetail.addressUnitId) map {
        case Some(address) => Some(a.toIndividualAccount(address.simplify))
        case None => None
      }
      case None => Future.successful(None)
    }
  }

  def findByGGID(ggId: String)(implicit hc: HeaderCarrier): Future[Option[IndividualAccount]] = {
    http.GET[Option[APIDetailedIndividualAccount]](s"$baseUrl?governmentGatewayExternalId=$ggId") flatMap {
      case Some(a) => addresses.get(a.personLatestDetail.addressUnitId) map {
        case Some(address) => Some(a.toIndividualAccount(address.simplify))
        case None => None
      }
      case None => Future.successful(None)
    }
  }

}
