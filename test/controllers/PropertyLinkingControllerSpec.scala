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

import java.net.URLEncoder

import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import connectors._
import helpers.WithSimpleWsHttpTestApplication
import infrastructure.SimpleWSHttp
import models.{APIAuthorisation, _}
import org.joda.time.{DateTime, LocalDate}
import play.api.http.ContentTypes
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global

class PropertyLinkingControllerSpec
  extends WireMockSpec with WithSimpleWsHttpTestApplication with ContentTypes {

  implicit val request = FakeRequest()

  var testPropertyLinkingController: PropertyLinkingController = _

  override def beforeAll() {
    super.beforeAll

    val http = fakeApplication.injector.instanceOf[SimpleWSHttp]

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

    testPropertyLinkingController = new PropertyLinkingController(propertyLinksConnector, groupAccountsConnector, representationsConnector)
  }

  "clientProperties" should {
    "only show the properties and permissions for that an agent" in {
      val userOrgId = 111
      val agentOrgId = 222
      val otherAgentOrgId = 333

      val dummyProperties = Seq(
        //prop with noAgents
        APIAuthorisation(100, 1, 5, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil),
        //prop with agent
        APIAuthorisation(100, 2, 6, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(APIParty(1, "APPROVED", agentOrgId, Seq(Permissions(1, "CONTINUE_ONLY", "CONTINUE_ONLY", None))))),
        //prop with OtherAgent
        APIAuthorisation(100, 3, 7, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(APIParty(2, "APPROVED", otherAgentOrgId, Seq(Permissions(2, "CONTINUE_ONLY", "CONTINUE_ONLY", None))))),
        //prop with agent and OtherAgent
        APIAuthorisation(100, 4, 8, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
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
      stubFor(get(urlEqualTo(s"/customer-management-api/organisation?organisationId=${otherAgentOrgId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(dummyAgentGroupAccount).toString)
        )
      )

      val res = testPropertyLinkingController.clientProperties(userOrgId, agentOrgId)(FakeRequest())
      status(res) shouldBe OK
      val uarnsAndPermIds = Json.parse(contentAsString(res)).as[Seq[ClientProperty]].map(x => (x.uarn, x.permissionId))
      uarnsAndPermIds shouldBe Seq((2, 1), (4, 4))
    }
  }

  "find" should {
    "only return the users properties if he is not an agent" in {
      val userOrgId = 111

      val dummyProperties = Seq(
        APIAuthorisation(101, 101, 103, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil),
        APIAuthorisation(102, 102, 104, userOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
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

      val usersOwnProperties = Seq(
        APIAuthorisation(1, 101, 105, userAgentOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil),
        APIAuthorisation(2, 102, 106, userAgentOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil)
      )

      //userAgent is representing 2 properties from  otherUser
      //otherUser has 3 properties
      val otherUsersProperties = Seq(
        //1 is managed by userAgentOnly,
        APIAuthorisation(3, 201, 211, otherUserOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(APIParty(1, "APPROVED", userAgentOrgId, Seq(Permissions(1L, "START_AND_CONTINUE", "START_AND_CONTINUE", None)))
          )
        ),
        //1 is managed by both userAgent and anotherAgent
        APIAuthorisation(4, 202, 212, otherUserOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Seq(APIParty(2, "APPROVED", userAgentOrgId, Seq(Permissions(2L, "START_AND_CONTINUE", "START_AND_CONTINUE", None))),
            APIParty(3, "APPROVED", anotherAgentOrgId, Seq(Permissions(3L, "START_AND_CONTINUE", "START_AND_CONTINUE", None)))
          )
        ),
        //1 is not managed all all
        APIAuthorisation(5, 203, 213, otherUserOrgId, "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Nil,
          Nil)
      )

      val propertyRepresentations = APIPropertyRepresentations(
        0,
        Seq(
          APIPropertyRepresentation(userAgentOrgId, "", otherUserOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED"),
          APIPropertyRepresentation(userAgentOrgId, "", otherUserOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED")
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
      stubFor(get(urlEqualTo(s"/customer-management-api/organisation?organisationId=${anotherAgentOrgId}"))
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

  "get" should {
    "show the authorisation with a given authorisationId" in {
      val userOrgId = 111
      val agentOrgId = 222
      val otherUserOrgId = 333
      val specificAuthorisationId = 100

      val dummyProperty = APIAuthorisationResult(Some(specificAuthorisationId), 4, 8, Some(userOrgId),
        "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1231", Some(Seq())
      )

      val dummyAuthorisedProperty = Seq(
        //prop with agent and OtherAgent
        APIAuthorisationResult(Some(12346), 4, 8, Some(userOrgId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "submissionId1",
          Some(Seq(
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        )
      )

      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation/${specificAuthorisationId}"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(dummyProperty).toString)
        )
      )

      val propertyRepresentations = APIPropertyRepresentations(
        0,
        Seq(APIPropertyRepresentation(userOrgId, "submissionId1", otherUserOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED"))
      )
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/agent_representation_requests?status=APPROVED&organisationId=$userOrgId&startPoint=1"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(propertyRepresentations).toString)
        )
      )
      val p = APIAuthorisationQuery(None, None, Some("submissionId1"))
      val params = URLEncoder.encode(Json.toJson(p).toString, "UTF-8")
      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=$params"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyAuthorisedProperty).toString}}""")
        )
      )

      val res = testPropertyLinkingController.get(specificAuthorisationId)(FakeRequest())
      status(res) shouldBe OK
      val r = Json.parse(contentAsString(res)).as[BasicPropertyLink]
      r.authorisationId shouldBe specificAuthorisationId
    }
  }

  "findFor" should {
    "return any user's property links for specific UARN when they don't have any clients" in {
      val userUserId = 1
      val userOrgId = 111
      val agentOrgId = 222
      val otherAgentOrgId = 333
      val specificUarn = 9999

      val dummyProperties = Seq(
        APIAuthorisationResult(Some(100), specificUarn, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1230",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        ),
        APIAuthorisationResult(Some(101), specificUarn, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now().minusDays(100), Some(LocalDate.now().minusDays(1)), "1231",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        ),
        APIAuthorisationResult(Some(102), specificUarn, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now().minusDays(200), Some(LocalDate.now().minusDays(101)), "1232",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        )
      )

      val userAndUarnQuery = URLEncoder.encode(Json.toJson(APIAuthorisationQuery(Some(userOrgId), Some(specificUarn), None)).toString, "UTF-8")
      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=$userAndUarnQuery"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyProperties).toString}}""")
        )
      )

      val propertyRepresentations = APIPropertyRepresentations(0, Seq())
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/agent_representation_requests?status=APPROVED&organisationId=$userOrgId&startPoint=1"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(propertyRepresentations).toString)
        )
      )

      val res = testPropertyLinkingController.findFor(userOrgId, specificUarn)(FakeRequest())
      status(res) shouldBe OK
      val uarns = Json.parse(contentAsString(res)).as[Seq[BasicPropertyLink]].map(_.uarn).distinct
      uarns shouldBe Seq(specificUarn)
      val userOrgIds = Json.parse(contentAsString(res)).as[Seq[BasicPropertyLink]].map(_.organisationId).distinct
      userOrgIds shouldBe Seq(userOrgId)
    }

    "return any user's property links for specific UARN" in {
      val userUserId = 101
      val userOrgId = 111
      val clientOrgId = 112
      val agentOrgId = 222
      val otherAgentOrgId = 333
      val otherUserId = 212
      val specificUarn = 9999

      val dummyProperties = Seq(
        APIAuthorisationResult(Some(100), specificUarn, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1230",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        ),
        APIAuthorisationResult(Some(101), specificUarn, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now().minusDays(100), Some(LocalDate.now().minusDays(1)), "1231",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        ),
        APIAuthorisationResult(Some(102), specificUarn, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now().minusDays(200), Some(LocalDate.now().minusDays(101)), "1232",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        )
      )

      val userAndUarnQuery = URLEncoder.encode(Json.toJson(APIAuthorisationQuery(Some(userOrgId), Some(specificUarn), None)).toString, "UTF-8")
      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=$userAndUarnQuery"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyProperties).toString}}""")
        )
      )

      val propertyRepresentations = APIPropertyRepresentations(
        0, Seq(
          APIPropertyRepresentation(userOrgId, "submissionId1", clientOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED"),
          APIPropertyRepresentation(userOrgId, "submissionId2", clientOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED")
        )
      )
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/agent_representation_requests?status=APPROVED&organisationId=$userOrgId&startPoint=1"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(propertyRepresentations).toString)
        )
      )

      val dummyAuthorisedProperty1 = Seq(
        //prop with agent and OtherAgent
        APIAuthorisationResult(Some(12346), specificUarn, clientOrgId, Some(otherUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "submissionId1",
          Some(Seq(APIParty(4, "APPROVED", userOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))))
        )
      )
      val dummyAuthorisedProperty2 = Seq(
        //prop with agent and OtherAgent
        APIAuthorisationResult(Some(12347), 111111, otherAgentOrgId, Some(otherUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "submissionId2",
          Some(Seq(APIParty(5, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))))
        )
      )
      val submission1Query = URLEncoder.encode(Json.toJson(APIAuthorisationQuery(None, None, Some("submissionId1"))).toString, "UTF-8")
      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=$submission1Query"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyAuthorisedProperty1).toString}}""")
        )
      )
      val submission2Query = URLEncoder.encode(Json.toJson(APIAuthorisationQuery(None, None, Some("submissionId2"))).toString, "UTF-8")
      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=$submission2Query"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyAuthorisedProperty2).toString}}""")
        )
      )

      val res = testPropertyLinkingController.findFor(userOrgId, specificUarn)(FakeRequest())
      status(res) shouldBe OK
      val uarns = Json.parse(contentAsString(res)).as[Seq[BasicPropertyLink]].map(_.uarn).distinct
      uarns shouldBe Seq(specificUarn)
      val userOrgIds = Json.parse(contentAsString(res)).as[Seq[BasicPropertyLink]].map(_.organisationId).distinct
      userOrgIds shouldBe Seq(userOrgId, clientOrgId)
    }
    "return all properties for an organisationId" in {
      val userUserId = 101
      val userOrgId = 111
      val clientOrgId = 112
      val agentOrgId = 222
      val otherAgentOrgId = 606
      val otherUserOrgId = 333
      val otherUserId = 212

      val dummyProperties = Seq(
        APIAuthorisationResult(Some(100), 9999, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "1230",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        ),
        APIAuthorisationResult(Some(101), 8888, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now().minusDays(100), Some(LocalDate.now().minusDays(1)), "1231",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        ),
        APIAuthorisationResult(Some(102), 7777, userOrgId, Some(userUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now().minusDays(200), Some(LocalDate.now().minusDays(101)), "1232",
          Some(Seq(
            APIParty(3, "APPROVED", otherAgentOrgId, Seq(Permissions(3, "START_AND_CONTINUE", "CONTINUE_ONLY", None))),
            APIParty(4, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))
          ))
        )
      )

      val userAndUarnQuery = URLEncoder.encode(Json.toJson(APIAuthorisationQuery(Some(userOrgId), None, None)).toString, "UTF-8")
      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=$userAndUarnQuery"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyProperties).toString}}""")
        )
      )

      val propertyRepresentations = APIPropertyRepresentations(
        0, Seq(
          APIPropertyRepresentation(userOrgId, "submissionId1", clientOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED"),
          APIPropertyRepresentation(userOrgId, "submissionId2", clientOrgId, "Client1Ltd", "", "", "", LocalDate.now(), "APPROVED")
        )
      )
      stubFor(get(urlEqualTo(s"/mdtp-dashboard-management-api/mdtp_dashboard/agent_representation_requests?status=APPROVED&organisationId=$userOrgId&startPoint=1"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(Json.toJson(propertyRepresentations).toString)
        )
      )

      val dummyAuthorisedProperty1 = Seq(
        //prop with agent and OtherAgent
        APIAuthorisationResult(Some(12346), 6666, clientOrgId, Some(otherUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "submissionId1",
          Some(Seq(APIParty(4, "APPROVED", userOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))))
        )
      )
      val dummyAuthorisedProperty2 = Seq(
        //prop with agent and OtherAgent
        APIAuthorisationResult(Some(12347), 5555, otherUserOrgId, Some(otherUserId), "AAA", "ASDf", "string", DateTime.now(), LocalDate.now(), None, "submissionId2",
          Some(Seq(APIParty(5, "APPROVED", agentOrgId, Seq(Permissions(4, "CONTINUE_ONLY", "NOT_PERMITTED", None)))))
        )
      )
      val submission1Query = URLEncoder.encode(Json.toJson(APIAuthorisationQuery(None, None, Some("submissionId1"))).toString, "UTF-8")
      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=$submission1Query"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyAuthorisedProperty1).toString}}""")
        )
      )
      val submission2Query = URLEncoder.encode(Json.toJson(APIAuthorisationQuery(None, None, Some("submissionId2"))).toString, "UTF-8")
      stubFor(get(urlEqualTo(s"/authorisation-management-api/authorisation?startPoint=1&pageSize=100&searchParameters=$submission2Query"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(s"""{"authorisations": ${Json.toJson(dummyAuthorisedProperty2).toString}}""")
        )
      )

      val res = testPropertyLinkingController.findForOrg(userOrgId)(FakeRequest())
      status(res) shouldBe OK
      val uarns = Json.parse(contentAsString(res)).as[Seq[BasicPropertyLink]].map(_.uarn).distinct
      uarns shouldBe Seq(9999,8888,7777,6666,5555)
      val userOrgIds = Json.parse(contentAsString(res)).as[Seq[BasicPropertyLink]].map(_.organisationId).distinct.sorted
      userOrgIds shouldBe Seq(userOrgId, otherUserOrgId, clientOrgId).sorted
    }
  }

  "setEnd" should {

    val validAuthId = 99
    val invalidAuthId = 88
    val requestData = PropertyLinkEndDateRequest(validAuthId.toString, LocalDate.now())

    "return OK with valid call" in {
      stubFor(patch(urlEqualTo(s"/authorisation-management-api/authorisation/$validAuthId"))
        .willReturn(aResponse
          .withStatus(200)
          .withHeader("Content-Type", JSON)
          .withBody(
            """{
              |   "id":99,
              |   "message":"Success",
              |   "responseTime":78
              |}""".stripMargin)
        )
      )

      val res = testPropertyLinkingController.setEnd(validAuthId)(request.withBody(Json.toJson(requestData)))
      status(res) shouldBe OK

    }

    "return 404 when called with a non existing property link" in {
      stubFor(patch(urlEqualTo(s"/authorisation-management-api/authorisation/$invalidAuthId"))
        .willReturn(aResponse
          .withStatus(404)
          .withHeader("Content-Type", JSON)
          .withBody(
            """{
              |   "uniqueId":"88",
              |   "errorTypeNum":404,
              |   "errorTypeDesc":"string",
              |   "errorMessage":"string"
              |}""".stripMargin))
      )

      val res = testPropertyLinkingController.setEnd(invalidAuthId)(request.withBody(Json.toJson(requestData.copy(authorisationId = invalidAuthId.toString()))))
      res.onComplete {
        case _ => status(res) shouldBe NOT_FOUND
      }
    }
  }
}
