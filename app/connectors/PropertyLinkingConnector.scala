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
import javax.inject.{Inject, Named}

import models.{APIAuthorisationQuery, _}
import org.omg.CosNaming.NamingContextPackage.NotFound
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.http.ws.WSHttp

import scala.concurrent.{ExecutionContext, Future}

class PropertyLinkingConnector @Inject() (@Named("VoaBackendWsHttp") http: WSHttp)(implicit ec: ExecutionContext)
  extends ServicesConfig {
  lazy val baseUrl: String = baseUrl("external-business-rates-data-platform")
  val listYear = 2017

  def create(linkingRequest: APIPropertyLinkRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    val url = baseUrl + s"/property-management-api/property/save_property_link"
    http.POST[APIPropertyLinkRequest, HttpResponse](url, linkingRequest) map { _ => () }
  }

  def find(organisationId: Long)(implicit hc: HeaderCarrier): Future[Seq[APIAuthorisation]] = {
    val url = baseUrl + s"/mdtp-dashboard-management-api/mdtp_dashboard/properties_view?listYear=$listYear&organisationId=$organisationId"
    val props = http.GET[JsValue](url).map(js =>{
      (js \ "authorisations").as[Seq[APIAuthorisation]]
    }).map( _
        .filterNot(_.authorisationStatus.toUpperCase == "REVOKED")
        .filterNot(_.authorisationStatus.toUpperCase == "DECLINED")
      )
    props.map(_.map(x=> {
      x.copy(parties = {
        x.parties
          .filter(party => List("APPROVED", "PENDING").contains(party.authorisedPartyStatus)) //parties must be approved or pending
          .map(party => party.copy(permissions =  party.permissions.filterNot(_.endDate.isDefined))) //permissions can't have enddate
          .filter(_.permissions.nonEmpty) //and agent must have a permission
      })
    }))
  }

  def getAssessment(authorisationId: Long)(implicit hc: HeaderCarrier): Future[Seq[Assessment]] = {
    val url = baseUrl + s"/mdtp-dashboard-management-api/mdtp_dashboard/view_assessment?listYear=$listYear&authorisationId=$authorisationId"
    http.GET[APIAuthorisation](url).map(pLink => {
      pLink.NDRListValuationHistoryItems.map(assessment => Assessment.fromAPIValuationHistory(assessment, authorisationId, CapacityDeclaration(pLink.authorisationOwnerCapacity, pLink.startDate, pLink.endDate)))
    })
  }

  def setEnd(authorisationId: Long, endRequest: APIPropertyLinkEndDateRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    val url = baseUrl + s"/authorisation-management-api/authorisation/${authorisationId}"
    http.PATCH[APIPropertyLinkEndDateRequest, HttpResponse](url, endRequest) map { _ => () }
  }

  private def findFor(q:APIAuthorisationQuery)(implicit hc: HeaderCarrier) = {
    val url = baseUrl + s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=${URLEncoder.encode(Json.toJson(q).toString, "UTF-8")}"
    http.GET[JsValue](url).map(js => {
      (js \ "authorisations").as[Seq[APIAuthorisationResult]]
    }).map(_
      .filterNot(_.authorisationStatus.toUpperCase == "REVOKED")
      .filterNot(_.authorisationStatus.toUpperCase == "DECLINED")
    )
  }

  def findFor(organisationId: Long, uarn: Option[Long])(implicit hc: HeaderCarrier): Future[Seq[APIAuthorisationResult]] = {
    findFor(new APIAuthorisationQuery(Some(organisationId), uarn, None))
  }

  def getBySubmissionId(submissionId:String)(implicit hc: HeaderCarrier): Future[Option[APIAuthorisationResult]] = {
    findFor(new APIAuthorisationQuery(None, None, Some(submissionId))).map(o => o.headOption)
  }

  def get(authorisationId: Long)(implicit hc: HeaderCarrier): Future[APIAuthorisationResult] = {
    val url = s"$baseUrl/authorisation-management-api/authorisation/$authorisationId"
    http.GET[JsValue](url).map(_.as[APIAuthorisationResult])
  }

}

