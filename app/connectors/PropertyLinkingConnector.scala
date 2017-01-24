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

import java.net.URLEncoder

import models._
import play.api.libs.json.JsValue
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ExecutionContext, Future}

class PropertyLinkingConnector(http: HttpGet with HttpPut with HttpPost)(implicit ec: ExecutionContext)
  extends ServicesConfig {
  lazy val baseUrl: String = baseUrl("external-business-rates-data-platform")
  val listYear = 2017

  def create(linkId: String, linkingRequest: PropertyLinkRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    val url = baseUrl + s"/property-links/$linkId"
    http.POST[PropertyLinkRequest, HttpResponse](url, linkingRequest) map { _ => () }
  }

  def find(organisationId: Int)(implicit hc: HeaderCarrier): Future[Seq[APIAuthorisation]] = {
    val url = baseUrl + s"/mdtp-dashboard-management-api/mdtp_dashboard/properties_view?listYear=$listYear&organisationId=$organisationId"
    http.GET[JsValue](url).map(js =>{
      (js \ "authorisations").as[Seq[APIAuthorisation]]
    }).map( _
        .filterNot(_.authorisationStatus.toUpperCase == "REVOKED")
        .filterNot(_.authorisationStatus.toUpperCase == "DECLINED")
      )
  }

  def get(linkId: String)(implicit hc: HeaderCarrier): Future[Option[PropertyLink]] = {
    val url = baseUrl + s"/property-links/$linkId"
    http.GET[Option[PropertyLink]](url)
  }

  def getAssessment(authorisationId: Int)(implicit hc: HeaderCarrier) = {
    val url = baseUrl + s"/mdtp-dashboard-management-api/mdtp_dashboard/view_assessment?listYear=$listYear&authorisationId=$authorisationId"
    http.GET[APIAuthorisation](url).map(pLink =>{
      pLink.valuationHistory.map(assessment => Assessment.fromAPIValuationHistory(assessment, authorisationId, CapacityDeclaration(pLink.authorisationOwnerCapacity, pLink.startDate, pLink.endDate)))
    })
  }
}
