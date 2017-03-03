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

import javax.inject.Inject

import config.VOABackendWSHttp
import models._
import play.api.libs.json.JsValue
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class GroupAccountConnector@Inject()(
                                       addresses: AddressConnector,
                                       http: VOABackendWSHttp)(implicit ec: ExecutionContext)
  extends ServicesConfig {

  lazy val baseUrl: String = baseUrl("external-business-rates-data-platform")
  lazy val url =  baseUrl + "/customer-management-api/organisation"

  def create(account: GroupAccountSubmission)(implicit hc: HeaderCarrier): Future[JsValue] = {
    account.address.addressUnitId match {
      case Some(id) => http.POST[APIGroupAccount, JsValue](url, account.toApiAccount(id))
      case None => addresses.create(account.address) flatMap { id =>
        http.POST[APIGroupAccount, JsValue](url, account.toApiAccount(id))
      }
    }
  }

  def get(id: Long)(implicit hc: HeaderCarrier): Future[Option[GroupAccount]] = {
    http.GET[Option[APIDetailedGroupAccount]](s"$url?organisationId=$id") flatMap {
      case Some(a) => addresses.get(a.organisationLatestDetail.addressUnitId) map {
        case Some(address) => Some(a.toGroupAccount(address.simplify))
        case None => Some(a.toGroupAccount(SimpleAddress(None, "", "", "", "", "")))
      }
      case None => Future.successful(None)
    }
  }

  def findByGGID(ggId: String)(implicit hc: HeaderCarrier): Future[Option[GroupAccount]] = {
    http.GET[Option[APIDetailedGroupAccount]](s"$url?governmentGatewayGroupId=$ggId") flatMap {
      case Some(a) => addresses.get(a.organisationLatestDetail.addressUnitId) map {
        case Some(address) => Some(a.toGroupAccount(address.simplify))
        case None => None
      }
      case None => Future.successful(None)
    }
  }

  def withAgentCode(agentCode: String)(implicit hc: HeaderCarrier): Future[Option[GroupAccount]] = {
    http.GET[Option[APIDetailedGroupAccount]](s"$url?representativeCode=$agentCode") flatMap {
      case Some(a) => addresses.get(a.organisationLatestDetail.addressUnitId) map {
        case Some(address) => Some(a.toGroupAccount(address.simplify))
        case None => None
      }
      case None => Future.successful(None)
    }
  }
}
