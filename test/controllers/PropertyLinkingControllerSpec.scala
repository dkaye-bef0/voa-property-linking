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

import com.github.tomakehurst.wiremock.client.WireMock._
import config.VOABackendWSHttp
import connectors._
import models._
import org.joda.time.{DateTime, LocalDate}
import play.api.http.ContentTypes
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global

class PropertyLinkingControllerSpec
  extends ContentTypes
  with WireMockSpec{

  implicit val request = FakeRequest()

  play.api.Play.start(app)
  val http = app.injector.instanceOf[VOABackendWSHttp]
  val propertyLinksConnector = new PropertyLinkingConnector(http) {
    override lazy val baseUrl: String = mockServerUrl
  }
  val addressesConnector = new AddressConnector(http)

  val groupAccountsConnector: GroupAccountConnector = new GroupAccountConnector(addressesConnector, http) {
    override lazy val baseUrl: String = mockServerUrl
  }
  val representationsConnector = new PropertyRepresentationConnector(http) {
    override lazy val baseUrl: String = mockServerUrl
  }
  val testPropertyLinkingController  = new PropertyLinkingController(propertyLinksConnector, groupAccountsConnector, representationsConnector)


  "clientProperties" should {
    "only show the properties assigned to an agent" in {
      val userOrgId = 111
      val agentOrgId = 222
      val otherAgentOrgId = 333

      val dummyProperties = Seq (
        //prop with noAgents
        APIAuthorisation(100, 1, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil),
        //prop with agent
        APIAuthorisation(100, 2, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(APIParty(agentOrgId, Seq(Permissions("CONTINUE_ONLY", "CONTINUE_ONLY"))))),
        //prop with OtherAgent
        APIAuthorisation(100, 3, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(APIParty(otherAgentOrgId, Seq(Permissions("CONTINUE_ONLY", "CONTINUE_ONLY"))))),
        //prop with agent and OtherAgent
        APIAuthorisation(100, 4, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(
            APIParty(otherAgentOrgId, Seq(Permissions("CONTINUE_ONLY", "CONTINUE_ONLY"))),
            APIParty(agentOrgId, Seq(Permissions("CONTINUE_ONLY", "CONTINUE_ONLY")))
          )
        )
      )
      val dummyUserGroupAccount = APIDetailedGroupAccount(
        userOrgId, "123", 1234, GroupDetails(1, true, true, "UserCompany", "aaa@aaa.com", None, LocalDate.now()), Nil
      )
      val dummyAgentGroupAccount = APIDetailedGroupAccount(
        agentOrgId, "123", 1234, GroupDetails(1, true, true, "UserCompany", "aaa@aaa.com", None, LocalDate.now()), Nil
      )
      val dummyOtherAgentGroupAccount = APIDetailedGroupAccount(
        otherAgentOrgId, "123", 1234, GroupDetails(1, true, true, "UserCompany", "aaa@aaa.com", None, LocalDate.now()), Nil
      )

      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/properties_view?listYear=2017&organisationId=${userOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyProperties).toString}}""")
        )
      )
      stubFor(get(urlEqualTo(s"/customer-management-api/organisation?organisationId=${userOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(dummyUserGroupAccount).toString)
        )
      )
      stubFor(get(urlEqualTo(s"/customer-management-api/organisation?organisationId=${agentOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(dummyAgentGroupAccount).toString)
        )
      )
      val res = testPropertyLinkingController.clientProperties(userOrgId, agentOrgId)(FakeRequest())
      status(res) shouldBe OK
      val uarns = Json.parse(contentAsString(res)).as[Seq[ClientProperties]].map(_.uarn)
       uarns shouldBe Seq(2, 4)
    }
  }

  "find" should {
    "only return the users properties if he is not an agent" in {
      val userOrgId = 111

      val dummyProperties = Seq (
        APIAuthorisation(101, 101, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil),
        APIAuthorisation(102, 102, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil)
      )
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/properties_view?listYear=2017&organisationId=${userOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyProperties).toString}}""")
        )
      )
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/agent_representation_requests?status=APPROVED&organisationId=$userOrgId&startPoint=1"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(APIPropertyRepresentations(0, Nil)).toString)
        )
      )
      val res = testPropertyLinkingController.find(userOrgId)(FakeRequest())
      status(res) shouldBe OK
      val uarns = Json.parse(contentAsString(res)).as[Seq[DetailedPropertyLink]].map(_.uarn)
      uarns shouldBe Seq(101, 102)
    }
  }
  it should {
    "return the user's own properties, and the properties it is managing" in {
      val userAgentOrgId = 111
      val otherUserOrgId = 222
      val anotherAgentOrgId = 333

      val usersOwnProperties = Seq (
        APIAuthorisation(1, 101, userAgentOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil),
        APIAuthorisation(2, 102, userAgentOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil)
        )

      //userAgent is representing 2 properties from  otherUser
      //otherUser has 3 properties
      val otherUsersProperties = Seq (
        //1 is managed by userAgentOnly,
        APIAuthorisation(3, 201, otherUserOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(APIParty(userAgentOrgId, Nil))),
        //1 is managed by both userAgent and anotherAgent
        APIAuthorisation(4, 202, otherUserOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(APIParty(userAgentOrgId, Nil),APIParty(anotherAgentOrgId, Nil))),
        //1 is not managed all all
        APIAuthorisation(5, 203, otherUserOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil)
      )

      val propertyRepresentations = APIPropertyRepresentations(
        0,
        Seq(
          APIPropertyRepresentation(1, "", otherUserOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED"),
          APIPropertyRepresentation(2, "", otherUserOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED")
        )
      )
      val dummyUserGroupAccount = APIDetailedGroupAccount(
        userAgentOrgId, "123", 1234, GroupDetails(1, true, true, "UserCompany", "aaa@aaa.com", None, LocalDate.now()), Nil
      )
      val dummyAgentGroupAccount = APIDetailedGroupAccount(
        otherUserOrgId, "123", 1234, GroupDetails(1, true, true, "UserCompany", "aaa@aaa.com", None, LocalDate.now()), Nil
      )
      val dummyOtherAgentGroupAccount = APIDetailedGroupAccount(
        anotherAgentOrgId, "123", 1234, GroupDetails(1, true, true, "UserCompany", "aaa@aaa.com", None, LocalDate.now()), Nil
      )

      stubFor(get(urlEqualTo(s"/customer-management-api/organisation?organisationId=${userAgentOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(dummyUserGroupAccount).toString)
        )
      )
      stubFor(get(urlEqualTo(s"/customer-management-api/organisation?organisationId=${otherUserOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(dummyAgentGroupAccount).toString)
        )
      )
      stubFor(get(urlEqualTo(s"/customer-management-api/organisation?organisationId=${otherUserOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(dummyOtherAgentGroupAccount).toString)
        )
      )
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/properties_view?listYear=2017&organisationId=${userAgentOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(usersOwnProperties).toString}}""")
        )
      )
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/properties_view?listYear=2017&organisationId=${otherUserOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(otherUsersProperties).toString}}""")
        )
      )
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/agent_representation_requests?status=APPROVED&organisationId=$userAgentOrgId&startPoint=1"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(propertyRepresentations).toString)
        )
      )


      val res = testPropertyLinkingController.find(userAgentOrgId)(FakeRequest())
      status(res) shouldBe OK
      val uarns = Json.parse(contentAsString(res)).as[Seq[DetailedPropertyLink]].map(_.uarn)
      uarns.sorted shouldBe Seq(101, 102, 201, 202).sorted

    }
  }

}