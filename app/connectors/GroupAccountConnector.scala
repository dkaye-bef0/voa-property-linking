/*
 * Copyright 2016 HM Revenue & Customs
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

import models.{APIDetailedGroupAccount, APIGroupAccount, GroupAccount, GroupAccountSubmission}
import play.api.libs.json.JsValue
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet, HttpPost}

import scala.concurrent.{ExecutionContext, Future}

class GroupAccountConnector(http: HttpGet with HttpPost)(implicit ec: ExecutionContext) extends ServicesConfig {

  lazy val url =  baseUrl("external-business-rates-data-platform") + "/organisation"

  def create(account: GroupAccountSubmission)(implicit hc: HeaderCarrier): Future[JsValue] = {
    http.POST[APIGroupAccount, JsValue](url, account.toApiAccount)
  }

  def get(id: Int)(implicit hc: HeaderCarrier): Future[Option[GroupAccount]] = {
    http.GET[Option[APIDetailedGroupAccount]](s"$url?organisationId=$id") map { _.map { _.toGroupAccount }}
  }

  def findByGGID(ggId: String)(implicit hc: HeaderCarrier): Future[Option[GroupAccount]] = {
    http.GET[Option[APIDetailedGroupAccount]](s"$url?governmentGatewayExternalId=$ggId") map { _.map { _.toGroupAccount }}
  }

  def withAgentCode(agentCode: String)(implicit hc: HeaderCarrier): Future[Option[GroupAccount]] = {
    http.GET[Option[APIDetailedGroupAccount]](s"$url?representativeCode=$agentCode") map { _.map { _.toGroupAccount }}
  }
}
