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

package controllers

import config.MongoDbProvider
import connectors._
import models._
import org.joda.time.{DateTime, LocalDate}
import org.mockito.ArgumentMatchers.{eq => mockEq, _}
import org.mockito.Mockito.{inOrder => ordered, _}
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.{Configuration, Mode}
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceInjectorBuilder}
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import reactivemongo.api.DB
import uk.gov.hmrc.play.config.inject.ServicesConfig
import uk.gov.hmrc.play.http.HttpReads
import uk.gov.hmrc.play.http.ws.WSHttp
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PropertyLinkingControllerSpec extends UnitSpec with MockitoSugar with WithFakeApplication {

  implicit val request = FakeRequest()

  val mockWS = mock[WSHttp]
  val mockConf = mock[ServicesConfig]
  val mockAddressConnector = mock[AddressConnector]
  val baseUrl = "http://localhost:9999"

  override lazy val fakeApplication = new GuiceApplicationBuilder()
    .in(Mode.Test)
    .overrides(bind[WSHttp].qualifiedWith("VoaBackendWsHttp").toInstance(mockWS))
    .overrides(bind[ServicesConfig].toInstance(mockConf))
    .disable(classOf[play.modules.reactivemongo.ReactiveMongoHmrcModule])
    .disable(classOf[modules.FileTransferScheduler])
    .disable(classOf[modules.MongoStartup])
    .build()

  "find" should {
    when(mockConf.baseUrl(any())).thenReturn(baseUrl)

    "only return the user's properties" in {
      reset(mockWS)
      val testPropertyLinkingController: PropertyLinkingController = fakeApplication.injector.instanceOf[PropertyLinkingController]
      val userOrgId = 111

      val dummyProperties = Seq(
        PropertiesView(101, 101, userOrgId, 103, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil, Nil),
        PropertiesView(102, 102, userOrgId, 104, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil, Nil))

      val propertiesUrl = s"$baseUrl/mdtp-dashboard-management-api/mdtp_dashboard/properties_view" +
        s"?listYear=2017" +
        s"&organisationId=$userOrgId" +
        s"&startPoint=1" +
        s"&pageSize=25" +
        s"&requestTotalRowCount=false"

      when(mockWS.GET(mockEq(propertiesUrl))(any(classOf[HttpReads[PropertiesViewResponse]]), any())).thenReturn(Future(PropertiesViewResponse(None, dummyProperties)))

      val repUrl = s"$baseUrl/mdtp-dashboard-management-api/mdtp_dashboard/agent_representation_requests?status=APPROVED&organisationId=$userOrgId&startPoint=1"
      when(mockWS.GET(mockEq(repUrl))(any(classOf[HttpReads[APIPropertyRepresentations]]), any())).thenReturn(APIPropertyRepresentations(0, Some(0), Nil))

      val res = testPropertyLinkingController.find(userOrgId, PaginationParams(1, 25, requestTotalRowCount = false))(FakeRequest())
      status(res) shouldBe OK
      val uarns = Json.parse(contentAsString(res)).as[PropertyLinkResponse].propertyLinks.map(_.uarn)
      uarns shouldBe Seq(101, 102)
    }

    "only call modernized once per linked party" in {
      reset(mockWS)
      val testPropertyLinkingController: PropertyLinkingController = fakeApplication.injector.instanceOf[PropertyLinkingController]
      val userOrgId = 111
      val baseParty1 = APIParty(1001, "APPROVED", 1001, Seq(Permissions(10001, "", "", None)))
      val baseParty2 = baseParty1.copy(id = 1002, authorisedPartyOrganisationId = 1002)

      val dummyProperties = Seq(
        PropertiesView(101, 101, userOrgId, 103, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(baseParty1, baseParty2)),
        PropertiesView(102, 102, userOrgId, 104, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(baseParty1, baseParty2)),
        PropertiesView(103, 103, userOrgId, 105, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(baseParty1, baseParty2)),
        PropertiesView(104, 104, userOrgId, 106, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(baseParty1, baseParty2)),
        PropertiesView(105, 105, userOrgId, 107, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(baseParty1, baseParty2)),
        PropertiesView(106, 106, userOrgId, 108, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(baseParty1, baseParty2)))


      val propertiesUrl = s"$baseUrl/mdtp-dashboard-management-api/mdtp_dashboard/properties_view" +
        s"?listYear=2017" +
        s"&organisationId=$userOrgId" +
        s"&startPoint=1" +
        s"&pageSize=25" +
        s"&requestTotalRowCount=false".toString
      when(mockWS.GET(mockEq(propertiesUrl))(any(classOf[HttpReads[PropertiesViewResponse]]), any())).thenReturn(Future(PropertiesViewResponse(None, dummyProperties)))

      val repUrl = s"$baseUrl/mdtp-dashboard-management-api/mdtp_dashboard/agent_representation_requests?status=APPROVED&organisationId=$userOrgId&startPoint=1"
      when(mockWS.GET(mockEq(repUrl))(any(classOf[HttpReads[APIPropertyRepresentations]]), any())).thenReturn(APIPropertyRepresentations(0, Some(0), Nil))

      val detailedGroup1 = APIDetailedGroupAccount(11, "ggGroup11", 1111, GroupDetails(1, true, "orgName", "email@add.res", None, LocalDate.now()), Nil)
      val detailedGroup2 = APIDetailedGroupAccount(22, "ggGroup22", 2222, GroupDetails(1, true, "orgName", "email@add.res", None, LocalDate.now()), Nil)

      val org1001Url = s"$baseUrl/customer-management-api/organisation?organisationId=1001"
      val org1002Url = s"$baseUrl/customer-management-api/organisation?organisationId=1002"
      when(mockWS.GET(mockEq(org1001Url))(any(classOf[HttpReads[Option[APIDetailedGroupAccount]]]), any())).thenReturn(Some(detailedGroup1))
      when(mockWS.GET(mockEq(org1002Url))(any(classOf[HttpReads[Option[APIDetailedGroupAccount]]]), any())).thenReturn(Some(detailedGroup2))

      val res = testPropertyLinkingController.find(userOrgId, PaginationParams(1, 25, requestTotalRowCount = false))(FakeRequest())
      status(res) shouldBe OK
      val uarns = Json.parse(contentAsString(res)).as[PropertyLinkResponse].propertyLinks.map(_.uarn)
      uarns shouldBe Seq(101, 102, 103, 104, 105, 106)

      verify(mockWS, times(1)).GET(mockEq(propertiesUrl))(any(classOf[HttpReads[PropertiesViewResponse]]), any())
      verify(mockWS, times(1)).GET(mockEq(org1001Url))(any(classOf[HttpReads[Option[APIDetailedGroupAccount]]]), any())
      verify(mockWS, times(1)).GET(mockEq(org1002Url))(any(classOf[HttpReads[Option[APIDetailedGroupAccount]]]), any())
    }
  }
}
